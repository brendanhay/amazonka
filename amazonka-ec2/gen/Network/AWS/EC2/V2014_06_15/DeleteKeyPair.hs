{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified key pair, by removing the public key from Amazon EC2.
-- Example This example request deletes the key pair named my-key-pair.
-- https://ec2.amazonaws.com/?Action=DeleteKeyPair &amp;KeyName=my-key-pair
-- &amp;AUTHPARAMS &lt;DeleteKeyPairResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteKeyPairResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteKeyPair
    (
    -- * Request
      DeleteKeyPair
    -- ** Request constructor
    , deleteKeyPair
    -- ** Request lenses
    , dkprKeyName

    -- * Response
    , DeleteKeyPairResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteKeyPair' request.
deleteKeyPair :: Text -- ^ 'dkprKeyName'
              -> DeleteKeyPair
deleteKeyPair p1 = DeleteKeyPair
    { _dkprKeyName = p1
    }

data DeleteKeyPair = DeleteKeyPair
    { _dkprKeyName :: Text
      -- ^ The name of the key pair.
    } deriving (Show, Generic)

-- | The name of the key pair.
dkprKeyName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteKeyPair
    -> f DeleteKeyPair
dkprKeyName f x =
    (\y -> x { _dkprKeyName = y })
       <$> f (_dkprKeyName x)
{-# INLINE dkprKeyName #-}

instance ToQuery DeleteKeyPair where
    toQuery = genericQuery def

data DeleteKeyPairResponse = DeleteKeyPairResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteKeyPair where
    type Sv DeleteKeyPair = EC2
    type Rs DeleteKeyPair = DeleteKeyPairResponse

    request = post "DeleteKeyPair"
    response _ = nullaryResponse DeleteKeyPairResponse
