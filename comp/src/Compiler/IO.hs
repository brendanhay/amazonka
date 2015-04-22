{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.ByteString           (ByteString)
import qualified Data.Text.Lazy            as LText
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.Text.Lazy.IO         as LText
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import           Formatting                hiding (left)
import           Formatting.Internal       (runFormat)
import qualified Text.EDE                  as EDE

isFile :: MonadIO m => Path -> Compiler m Bool
isFile = io . FS.isFile

readBSFile :: MonadIO m => Path -> MaybeT m ByteString
readBSFile f = hushT $ do
    p <- isFile f
    if p
        then say ("Reading "  % path) f >> io (FS.readFile f)
        else failure ("Missing " % path) f

writeLTFile :: MonadIO m => Path -> LazyText -> Compiler m ()
writeLTFile f t = io $ FS.withFile f FS.WriteMode (`LText.hPutStr` t)

createDir :: MonadIO m => Path -> Compiler m ()
createDir d = do
    p <- io (FS.isDirectory d)
    unless p $ do
        say ("Creating " % path) d
        io (FS.createTree d)

listDir :: MonadIO m => Path -> Compiler m [Path]
listDir = io . FS.listDirectory

copyDir :: MonadIO m => Path -> Path -> Compiler m ()
copyDir src dst = io (FS.listDirectory src >>= mapM_ copy)
  where
    copy f = do
        let p = dst </> filename f
        fprint (" -> Copying " % path % " to " % path % "\n") f (directory p)
        FS.copyFile f p

readTemplate :: MonadIO m => Path -> Path -> Compiler m EDE.Template
readTemplate d f = do
    let p = d </> f
    say ("Parsing " % path) p
    io (EDE.eitherParseFile (encodeString p))
        >>= hoistEither . first LText.pack

title :: MonadIO m => Format (Compiler m ()) a -> a
title m = runFormat m (io . LText.putStrLn . toLazyText)

say :: MonadIO m => Format (Compiler m ()) a -> a
say = title . (" -> " %)

done :: MonadIO m => Compiler m ()
done = title ""

run :: Compiler IO a -> IO a
run = runScript . fmapLT LText.unpack

io :: MonadIO m => IO a -> Compiler m a
io = fmapLT (LText.pack . show) . syncIO
