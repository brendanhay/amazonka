{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified network interface attribute. You can specify only
-- one attribute at a time. Example This example sets source/destination
-- checking to false for the specified network interface.
-- https://ec2.amazonaws.com/?Action=ModifyNetworkInterfaceAttribute
-- &amp;NetworkInterfaceId=eni-ffda3197 &amp;SourceDestCheck.Value=false
-- &amp;AUTHPARAMS &lt;ModifyNetworkInterfaceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;657a4623-5620-4232-b03b-427e852d71cf&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/ModifyNetworkInterfaceAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ModifyNetworkInterfaceAttribute
    (
    -- * Request
      ModifyNetworkInterfaceAttribute
    -- ** Request constructor
    , mkModifyNetworkInterfaceAttribute
    -- ** Request lenses
    , mniaNetworkInterfaceId
    , mniaDescription
    , mniaSourceDestCheck
    , mniaGroups
    , mniaAttachment

    -- * Response
    , ModifyNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , mkModifyNetworkInterfaceAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { _mniaNetworkInterfaceId :: Text
    , _mniaDescription :: Maybe AttributeValue
    , _mniaSourceDestCheck :: Maybe AttributeBooleanValue
    , _mniaGroups :: [Text]
    , _mniaAttachment :: Maybe NetworkInterfaceAttachmentChanges
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyNetworkInterfaceAttribute' request.
mkModifyNetworkInterfaceAttribute :: Text -- ^ 'mniaNetworkInterfaceId'
                                  -> ModifyNetworkInterfaceAttribute
mkModifyNetworkInterfaceAttribute p1 = ModifyNetworkInterfaceAttribute
    { _mniaNetworkInterfaceId = p1
    , _mniaDescription = Nothing
    , _mniaSourceDestCheck = Nothing
    , _mniaGroups = mempty
    , _mniaAttachment = Nothing
    }

-- | The ID of the network interface.
mniaNetworkInterfaceId :: Lens' ModifyNetworkInterfaceAttribute Text
mniaNetworkInterfaceId =
    lens _mniaNetworkInterfaceId (\s a -> s { _mniaNetworkInterfaceId = a })

-- | A description for the network interface.
mniaDescription :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeValue)
mniaDescription = lens _mniaDescription (\s a -> s { _mniaDescription = a })

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This value
-- must be false for a NAT instance to perform NAT. For more information, see
-- NAT Instances in the Amazon Virtual Private Cloud User Guide.
mniaSourceDestCheck :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeBooleanValue)
mniaSourceDestCheck =
    lens _mniaSourceDestCheck (\s a -> s { _mniaSourceDestCheck = a })

-- | Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least one
-- group, even if it's just the default security group in the VPC. You must
-- specify the ID of the security group, not the name.
mniaGroups :: Lens' ModifyNetworkInterfaceAttribute [Text]
mniaGroups = lens _mniaGroups (\s a -> s { _mniaGroups = a })

-- | The ID of the interface attachment.
mniaAttachment :: Lens' ModifyNetworkInterfaceAttribute (Maybe NetworkInterfaceAttachmentChanges)
mniaAttachment = lens _mniaAttachment (\s a -> s { _mniaAttachment = a })

instance ToQuery ModifyNetworkInterfaceAttribute where
    toQuery = genericQuery def

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyNetworkInterfaceAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkModifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse
mkModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse

instance AWSRequest ModifyNetworkInterfaceAttribute where
    type Sv ModifyNetworkInterfaceAttribute = EC2
    type Rs ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttributeResponse

    request = post "ModifyNetworkInterfaceAttribute"
    response _ = nullaryResponse ModifyNetworkInterfaceAttributeResponse
