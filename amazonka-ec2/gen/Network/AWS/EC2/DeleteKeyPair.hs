{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteKeyPair
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
module Network.AWS.EC2.DeleteKeyPair
    (
    -- * Request
      DeleteKeyPair
    -- ** Request constructor
    , deleteKeyPair
    -- ** Request lenses
    , dkpKeyName

    -- * Response
    , DeleteKeyPairResponse
    -- ** Response constructor
    , deleteKeyPairResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteKeyPair = DeleteKeyPair
    { _dkpKeyName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteKeyPair' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @KeyName ::@ @Text@
--
deleteKeyPair :: Text -- ^ 'dkpKeyName'
              -> DeleteKeyPair
deleteKeyPair p1 = DeleteKeyPair
    { _dkpKeyName = p1
    }

-- | The name of the key pair.
dkpKeyName :: Lens' DeleteKeyPair Text
dkpKeyName = lens _dkpKeyName (\s a -> s { _dkpKeyName = a })

instance ToQuery DeleteKeyPair where
    toQuery = genericQuery def

data DeleteKeyPairResponse = DeleteKeyPairResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteKeyPairResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteKeyPairResponse :: DeleteKeyPairResponse
deleteKeyPairResponse = DeleteKeyPairResponse

instance AWSRequest DeleteKeyPair where
    type Sv DeleteKeyPair = EC2
    type Rs DeleteKeyPair = DeleteKeyPairResponse

    request = post "DeleteKeyPair"
    response _ = nullaryResponse DeleteKeyPairResponse
