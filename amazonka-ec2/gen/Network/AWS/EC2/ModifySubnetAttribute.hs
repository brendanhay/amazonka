{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies a subnet attribute. Example This example modifies the attribute
-- for subnet-1a2b3c4d to specify that all instances launched into this subnet
-- are assigned a public IP address.
-- https://ec2.amazonaws.com/?Action=ModifySubnetAttribute
-- &amp;SubnetId=subnet-1a2b3c4d &amp;MapPublicIpOnLaunch.Value=true
-- &amp;AUTHPARAMS.
module Network.AWS.EC2.ModifySubnetAttribute
    (
    -- * Request
      ModifySubnetAttribute
    -- ** Request constructor
    , modifySubnetAttribute
    -- ** Request lenses
    , msa1SubnetId
    , msa1MapPublicIpOnLaunch

    -- * Response
    , ModifySubnetAttributeResponse
    -- ** Response constructor
    , modifySubnetAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ModifySubnetAttribute = ModifySubnetAttribute
    { _msa1SubnetId :: Text
    , _msa1MapPublicIpOnLaunch :: Maybe AttributeBooleanValue
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifySubnetAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubnetId ::@ @Text@
--
-- * @MapPublicIpOnLaunch ::@ @Maybe AttributeBooleanValue@
--
modifySubnetAttribute :: Text -- ^ 'msa1SubnetId'
                      -> ModifySubnetAttribute
modifySubnetAttribute p1 = ModifySubnetAttribute
    { _msa1SubnetId = p1
    , _msa1MapPublicIpOnLaunch = Nothing
    }

-- | The ID of the subnet.
msa1SubnetId :: Lens' ModifySubnetAttribute Text
msa1SubnetId = lens _msa1SubnetId (\s a -> s { _msa1SubnetId = a })

-- | 
msa1MapPublicIpOnLaunch :: Lens' ModifySubnetAttribute (Maybe AttributeBooleanValue)
msa1MapPublicIpOnLaunch =
    lens _msa1MapPublicIpOnLaunch
         (\s a -> s { _msa1MapPublicIpOnLaunch = a })

instance ToQuery ModifySubnetAttribute where
    toQuery = genericQuery def

data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifySubnetAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
modifySubnetAttributeResponse :: ModifySubnetAttributeResponse
modifySubnetAttributeResponse = ModifySubnetAttributeResponse

instance AWSRequest ModifySubnetAttribute where
    type Sv ModifySubnetAttribute = EC2
    type Rs ModifySubnetAttribute = ModifySubnetAttributeResponse

    request = post "ModifySubnetAttribute"
    response _ = nullaryResponse ModifySubnetAttributeResponse
