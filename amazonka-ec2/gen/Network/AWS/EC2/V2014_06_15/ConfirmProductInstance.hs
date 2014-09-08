{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ConfirmProductInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Determines whether a product code is associated with an instance. This
-- action can only be used by the owner of the product code. It is useful when
-- a product code owner needs to verify whether another user's instance is
-- eligible for support. Example This example determines whether the specified
-- product code is associated with the specified instance.
-- https://ec2.amazonaws.com/?Action=ConfirmProductInstance
-- &amp;ProductCode=774F4FF8 &amp;InstanceId=i-10a64379 &amp;AUTHPARAMS
-- &lt;ConfirmProductInstanceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;ownerId&gt;111122223333&lt;/ownerId&gt;
-- &lt;/ConfirmProductInstanceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ConfirmProductInstance
    (
    -- * Request
      ConfirmProductInstance
    -- ** Request constructor
    , mkConfirmProductInstance
    -- ** Request lenses
    , cpiProductCode
    , cpiInstanceId

    -- * Response
    , ConfirmProductInstanceResponse
    -- ** Response constructor
    , mkConfirmProductInstanceResponse
    -- ** Response lenses
    , cpirOwnerId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data ConfirmProductInstance = ConfirmProductInstance
    { _cpiProductCode :: Text
    , _cpiInstanceId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmProductInstance' request.
mkConfirmProductInstance :: Text -- ^ 'cpiProductCode'
                         -> Text -- ^ 'cpiInstanceId'
                         -> ConfirmProductInstance
mkConfirmProductInstance p1 p2 = ConfirmProductInstance
    { _cpiProductCode = p1
    , _cpiInstanceId = p2
    }

-- | The product code. This must be an Amazon DevPay product code that you own.
cpiProductCode :: Lens' ConfirmProductInstance Text
cpiProductCode = lens _cpiProductCode (\s a -> s { _cpiProductCode = a })

-- | The ID of the instance.
cpiInstanceId :: Lens' ConfirmProductInstance Text
cpiInstanceId = lens _cpiInstanceId (\s a -> s { _cpiInstanceId = a })

instance ToQuery ConfirmProductInstance where
    toQuery = genericQuery def

-- | 
newtype ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { _cpirOwnerId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmProductInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkConfirmProductInstanceResponse :: ConfirmProductInstanceResponse
mkConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { _cpirOwnerId = Nothing
    }

-- | The AWS account ID of the instance owner. This is only present if the
-- product code is attached to the instance.
cpirOwnerId :: Lens' ConfirmProductInstanceResponse (Maybe Text)
cpirOwnerId = lens _cpirOwnerId (\s a -> s { _cpirOwnerId = a })

instance FromXML ConfirmProductInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ConfirmProductInstance where
    type Sv ConfirmProductInstance = EC2
    type Rs ConfirmProductInstance = ConfirmProductInstanceResponse

    request = post "ConfirmProductInstance"
    response _ = xmlResponse
