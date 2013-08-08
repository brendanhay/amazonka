-- |
-- Module      : Network.AWS.Internal.IO
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.IO where

import           Control.Monad.IO.Class
import           Data.Aeson.Hastache
import           Data.ByteString            (ByteString)
import           Network.AWS.Internal.Types
import           System.IO.Streams          (InputStream)
import qualified System.IO.Streams          as Streams
import           Text.Hastache

render :: (MonadIO m, Template a) =>  a -> m (InputStream ByteString)
render tmpl = liftIO $
    hastacheStr defaultConfig (readTemplate tmpl) (jsonContext tmpl) >>=
        Streams.fromLazyByteString
