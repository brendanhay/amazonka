{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified network interface attribute. You can specify only
-- one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyNetworkInterfaceAttribute.html>
module Network.AWS.EC2.ModifyNetworkInterfaceAttribute
    (
    -- * Request
      ModifyNetworkInterfaceAttribute
    -- ** Request constructor
    , modifyNetworkInterfaceAttribute
    -- ** Request lenses
    , mniaAttachment
    , mniaDescription
    , mniaDryRun
    , mniaGroups
    , mniaNetworkInterfaceId
    , mniaSourceDestCheck

    -- * Response
    , ModifyNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , modifyNetworkInterfaceAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { _mniaAttachment         :: Maybe NetworkInterfaceAttachmentChanges
    , _mniaDescription        :: Maybe AttributeValue
    , _mniaDryRun             :: Maybe Bool
    , _mniaGroups             :: List "SecurityGroupId" Text
    , _mniaNetworkInterfaceId :: Text
    , _mniaSourceDestCheck    :: Maybe AttributeBooleanValue
    } deriving (Eq, Show)

-- | 'ModifyNetworkInterfaceAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mniaAttachment' @::@ 'Maybe' 'NetworkInterfaceAttachmentChanges'
--
-- * 'mniaDescription' @::@ 'Maybe' 'AttributeValue'
--
-- * 'mniaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'mniaGroups' @::@ ['Text']
--
-- * 'mniaNetworkInterfaceId' @::@ 'Text'
--
-- * 'mniaSourceDestCheck' @::@ 'Maybe' 'AttributeBooleanValue'
--
modifyNetworkInterfaceAttribute :: Text -- ^ 'mniaNetworkInterfaceId'
                                -> ModifyNetworkInterfaceAttribute
modifyNetworkInterfaceAttribute p1 = ModifyNetworkInterfaceAttribute
    { _mniaNetworkInterfaceId = p1
    , _mniaDryRun             = Nothing
    , _mniaDescription        = Nothing
    , _mniaSourceDestCheck    = Nothing
    , _mniaGroups             = mempty
    , _mniaAttachment         = Nothing
    }

-- | Information about the interface attachment. If modifying the 'delete on
-- termination' attribute, you must specify the ID of the interface
-- attachment.
mniaAttachment :: Lens' ModifyNetworkInterfaceAttribute (Maybe NetworkInterfaceAttachmentChanges)
mniaAttachment = lens _mniaAttachment (\s a -> s { _mniaAttachment = a })

-- | A description for the network interface.
mniaDescription :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeValue)
mniaDescription = lens _mniaDescription (\s a -> s { _mniaDescription = a })

mniaDryRun :: Lens' ModifyNetworkInterfaceAttribute (Maybe Bool)
mniaDryRun = lens _mniaDryRun (\s a -> s { _mniaDryRun = a })

-- | Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least
-- one group, even if it's just the default security group in the VPC. You
-- must specify the ID of the security group, not the name.
mniaGroups :: Lens' ModifyNetworkInterfaceAttribute [Text]
mniaGroups = lens _mniaGroups (\s a -> s { _mniaGroups = a }) . _List

-- | The ID of the network interface.
mniaNetworkInterfaceId :: Lens' ModifyNetworkInterfaceAttribute Text
mniaNetworkInterfaceId =
    lens _mniaNetworkInterfaceId (\s a -> s { _mniaNetworkInterfaceId = a })

-- | Indicates whether source/destination checking is enabled. A value of
-- 'true' means checking is enabled, and 'false' means checking is disabled.
-- This value must be 'false' for a NAT instance to perform NAT. For more
-- information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html
-- NAT Instances> in the /Amazon Virtual Private Cloud User Guide/.
mniaSourceDestCheck :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeBooleanValue)
mniaSourceDestCheck =
    lens _mniaSourceDestCheck (\s a -> s { _mniaSourceDestCheck = a })

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ModifyNetworkInterfaceAttributeResponse' constructor.
modifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse
modifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse

instance ToPath ModifyNetworkInterfaceAttribute where
    toPath = const "/"

instance ToQuery ModifyNetworkInterfaceAttribute where
    toQuery ModifyNetworkInterfaceAttribute{..} = mconcat
        [ "attachment"         =? _mniaAttachment
        , "description"        =? _mniaDescription
        , "dryRun"             =? _mniaDryRun
        , "SecurityGroupId"    =? _mniaGroups
        , "networkInterfaceId" =? _mniaNetworkInterfaceId
        , "sourceDestCheck"    =? _mniaSourceDestCheck
        ]

instance ToHeaders ModifyNetworkInterfaceAttribute

instance AWSRequest ModifyNetworkInterfaceAttribute where
    type Sv ModifyNetworkInterfaceAttribute = EC2
    type Rs ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttributeResponse

    request  = post "ModifyNetworkInterfaceAttribute"
    response = nullResponse ModifyNetworkInterfaceAttributeResponse
