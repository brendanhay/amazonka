-- Module      : Network.AWS.Sign
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Sign.Types
    (

    ) where

import Network.HTTP.Client

newtype Signed a = Signed Request

data Context = Context
    { _time :: UTCTime
    , _cred :: Credentials
    , _ver  :: Version
    , _reg  :: Region
    }

-- Parameters:
-- time
-- credentials
-- version of service
-- region
-- service witness

-- | Class representing signing algorithms.
class Signer a where
    data Ctx a :: *

    sign :: Ctx a -> Context -> Signed a

data S3

instance Signer S3 where
    data Ctx S3 = Bucket Text

    sign (Ctx b) ctx
