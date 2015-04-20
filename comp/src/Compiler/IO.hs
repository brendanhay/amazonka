{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.IO
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.IO where

import           Compiler.Types
import           Control.Error
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.Text.Lazy         as LText
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO      as LText
import qualified Filesystem             as FS
import           Formatting             hiding (left)
import           Formatting.Internal    (runFormat)

listDirectory :: MonadIO m => Path -> EitherT LazyText m Dir
listDirectory d = Dir d <$> io (FS.listDirectory d)

isFile :: MonadIO m => Path -> EitherT LazyText m Bool
isFile = io . FS.isFile

readByteString :: MonadIO m => Path -> MaybeT m ByteString
readByteString f = hushT $ do
    p <- isFile f
    if p
        then say ("Reading "  % path) f >> io (FS.readFile f)
        else say ("Skipping " % path) f >> failure ("Missing " % path) f

say :: MonadIO m => Format (EitherT LazyText m ()) a -> a
say m = runFormat m (io . LText.putStrLn . toLazyText)

run :: EitherT LazyText IO a -> IO a
run = runScript . fmapLT LText.unpack

io :: MonadIO m => IO a -> EitherT LazyText m a
io = fmapLT (LText.pack . show) . syncIO
