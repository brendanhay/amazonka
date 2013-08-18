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

module Network.AWS.Internal.IO
    ( render
    ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy       as LBS
import           Data.Data
import           Network.AWS.Internal.Types
import           Text.Hastache
import           Text.Hastache.Context

render :: (MonadIO m, Data a, Template a) => a -> m LBS.ByteString
render tmpl = liftIO $
    hastacheStr defaultConfig (readTemplate tmpl) (mkGenericContext tmpl)
