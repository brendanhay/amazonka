module Text.XML.LibXML.Enumerator
    ( parseBytesST
    ) where

import           Control.Exception         (ErrorCall(..))
import           Control.Monad             (unless, void)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.ST          (ST)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString           as BS
import           Data.ByteString           (ByteString)
import           Data.Enumerator           (Enumeratee, (>>==))
import qualified Data.Enumerator           as E
import qualified Data.IORef                as IO
import qualified Data.STRef                as ST
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Types
import qualified Text.LibSAX               as SAX

setCallbacks :: Monad m => Bool -> SAX.Parser m -> (Event -> m Bool) -> m ()
setCallbacks expandRefs p addEvent = do
    set SAX.parsedBeginDocument  (addEvent EventBeginDocument)
    set SAX.parsedEndDocument    (addEvent EventEndDocument)
    set SAX.parsedBeginElement   ((addEvent .) . EventBeginElement)
    set SAX.parsedEndElement     (addEvent . EventEndElement)
    set SAX.parsedCharacters     (addEvent . EventContent . ContentText)
    set SAX.parsedCDATA          (addEvent . EventCDATA)
    set SAX.parsedComment        (addEvent . EventComment)
    set SAX.parsedInstruction    (addEvent . EventInstruction)
    set SAX.parsedExternalSubset ((addEvent .) . EventBeginDoctype)

    unless expandRefs
        (set SAX.parsedReference (addEvent . EventContent . ContentEntity))
  where
    set = SAX.setCallback p

parseBytesST :: Bool       -- ^ Whether to expand entity references
             -> Maybe Text -- ^ An optional filename or URI
             -> Enumeratee ByteString Event (ST s) b
parseBytesST expandRefs name s = E.Iteratee $do
    p <- SAX.newParserST name

    -- Error handling
    errRef <- ST.newSTRef Nothing
    SAX.setCallback p SAX.reportError $ \msg -> do
        ST.writeSTRef errRef (Just msg)
        return False

    -- Event storage
    eventRef <- ST.newSTRef []
    let addEvent e = do
        ST.modifySTRef eventRef (e:)
        return True

    setCallbacks expandRefs p addEvent

    let withEvents st = do
        ST.writeSTRef eventRef []
        ST.writeSTRef errRef Nothing
        void st
        events <- ST.readSTRef eventRef
        err <- ST.readSTRef errRef
        return (reverse events, err)

    let parseChunk bytes = withEvents (SAX.parseBytes p bytes)
    let complete = withEvents (SAX.parseComplete p)

    E.runIteratee $ eneeParser parseChunk complete s







eneeParser :: Monad m
           => (a -> m ([Event], Maybe Text))
           -> m ([Event], Maybe Text)
           -> Enumeratee a Event m b
eneeParser parseChunk parseComplete = E.checkDone (E.continue . step)
  where
    step k (E.Chunks xs) = parseLoop k xs
    step k E.EOF         =
        checkEvents k E.EOF parseComplete (\k' -> E.yield (E.Continue k') E.EOF)

    parseLoop k []     = E.continue (step k)
    parseLoop k (x:xs) =
        checkEvents k (E.Chunks xs) (parseChunk x) (\k' -> parseLoop k' xs)

    checkEvents k extra getEvents next = do
        (events, maybeErr) <- lift getEvents
        let checkError k' = case maybeErr of
            Just err -> E.throwError (ErrorCall (Text.unpack err))
            Nothing  -> next k'
        if null events
            then checkError k
            else k (E.Chunks events) >>== E.checkDoneEx extra checkError
