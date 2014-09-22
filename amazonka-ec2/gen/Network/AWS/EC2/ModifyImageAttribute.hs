{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified attribute of the specified AMI. You can specify only
-- one attribute at a time. AWS Marketplace product codes cannot be modified.
-- Images with an AWS Marketplace product code cannot be made public. Example.
module Network.AWS.EC2.ModifyImageAttribute
    (
    -- * Request
      ModifyImageAttribute
    -- ** Request constructor
    , modifyImageAttribute
    -- ** Request lenses
    , miaImageId
    , miaAttribute
    , miaOperationType
    , miaUserId
    , miaUserGroup
    , miaProductCode
    , miaValue
    , miaLaunchPermission
    , miaDescription

    -- * Response
    , ModifyImageAttributeResponse
    -- ** Response constructor
    , modifyImageAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ModifyImageAttribute = ModifyImageAttribute
    { _miaImageId :: Text
    , _miaAttribute :: Maybe Text
    , _miaOperationType :: Maybe Text
    , _miaUserId :: [Text]
    , _miaUserGroup :: [Text]
    , _miaProductCode :: [Text]
    , _miaValue :: Maybe Text
    , _miaLaunchPermission :: Maybe LaunchPermissionModifications
    , _miaDescription :: Maybe AttributeValue
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyImageAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Text@
--
-- * @Attribute ::@ @Maybe Text@
--
-- * @OperationType ::@ @Maybe Text@
--
-- * @UserId ::@ @[Text]@
--
-- * @UserGroup ::@ @[Text]@
--
-- * @ProductCode ::@ @[Text]@
--
-- * @Value ::@ @Maybe Text@
--
-- * @LaunchPermission ::@ @Maybe LaunchPermissionModifications@
--
-- * @Description ::@ @Maybe AttributeValue@
--
modifyImageAttribute :: Text -- ^ 'miaImageId'
                     -> ModifyImageAttribute
modifyImageAttribute p1 = ModifyImageAttribute
    { _miaImageId = p1
    , _miaAttribute = Nothing
    , _miaOperationType = Nothing
    , _miaUserId = mempty
    , _miaUserGroup = mempty
    , _miaProductCode = mempty
    , _miaValue = Nothing
    , _miaLaunchPermission = Nothing
    , _miaDescription = Nothing
    }

-- | The ID of the AMI.
miaImageId :: Lens' ModifyImageAttribute Text
miaImageId = lens _miaImageId (\s a -> s { _miaImageId = a })

-- | The name of the attribute to modify.
miaAttribute :: Lens' ModifyImageAttribute (Maybe Text)
miaAttribute = lens _miaAttribute (\s a -> s { _miaAttribute = a })

-- | The operation type.
miaOperationType :: Lens' ModifyImageAttribute (Maybe Text)
miaOperationType =
    lens _miaOperationType (\s a -> s { _miaOperationType = a })

-- | One or more AWS account IDs. This is only valid when modifying the
-- launchPermission attribute.
miaUserId :: Lens' ModifyImageAttribute [Text]
miaUserId = lens _miaUserId (\s a -> s { _miaUserId = a })

-- | One or more user groups. This is only valid when modifying the
-- launchPermission attribute.
miaUserGroup :: Lens' ModifyImageAttribute [Text]
miaUserGroup = lens _miaUserGroup (\s a -> s { _miaUserGroup = a })

-- | One or more product codes. After you add a product code to an AMI, it can't
-- be removed. This is only valid when modifying the productCodes attribute.
miaProductCode :: Lens' ModifyImageAttribute [Text]
miaProductCode = lens _miaProductCode (\s a -> s { _miaProductCode = a })

-- | The value of the attribute being modified. This is only valid when
-- modifying the description attribute.
miaValue :: Lens' ModifyImageAttribute (Maybe Text)
miaValue = lens _miaValue (\s a -> s { _miaValue = a })

-- | 
miaLaunchPermission :: Lens' ModifyImageAttribute (Maybe LaunchPermissionModifications)
miaLaunchPermission =
    lens _miaLaunchPermission (\s a -> s { _miaLaunchPermission = a })

-- | A description for the AMI.
miaDescription :: Lens' ModifyImageAttribute (Maybe AttributeValue)
miaDescription = lens _miaDescription (\s a -> s { _miaDescription = a })

instance ToQuery ModifyImageAttribute where
    toQuery = genericQuery def

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyImageAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
modifyImageAttributeResponse :: ModifyImageAttributeResponse
modifyImageAttributeResponse = ModifyImageAttributeResponse

instance AWSRequest ModifyImageAttribute where
    type Sv ModifyImageAttribute = EC2
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse

    request = post "ModifyImageAttribute"
    response _ = nullaryResponse ModifyImageAttributeResponse
