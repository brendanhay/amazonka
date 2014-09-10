{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
module Network.AWS.EC2
    (
    -- * Request
      ModifyImageAttribute
    -- ** Request constructor
    , mkModifyImageAttribute
    -- ** Request lenses
    , miaImageId
    , miaAttribute
    , miaOperationType
    , miaUserIds
    , miaUserGroups
    , miaProductCodes
    , miaValue
    , miaLaunchPermission
    , miaDescription

    -- * Response
    , ModifyImageAttributeResponse
    -- ** Response constructor
    , mkModifyImageAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ModifyImageAttribute = ModifyImageAttribute
    { _miaImageId :: !Text
    , _miaAttribute :: !(Maybe Text)
    , _miaOperationType :: !(Maybe Text)
    , _miaUserIds :: [Text]
    , _miaUserGroups :: [Text]
    , _miaProductCodes :: [Text]
    , _miaValue :: !(Maybe Text)
    , _miaLaunchPermission :: Maybe LaunchPermissionModifications
    , _miaDescription :: Maybe AttributeValue
    } deriving (Show, Generic)

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
-- * @UserIds ::@ @[Text]@
--
-- * @UserGroups ::@ @[Text]@
--
-- * @ProductCodes ::@ @[Text]@
--
-- * @Value ::@ @Maybe Text@
--
-- * @LaunchPermission ::@ @Maybe LaunchPermissionModifications@
--
-- * @Description ::@ @Maybe AttributeValue@
--
mkModifyImageAttribute :: Text -- ^ 'miaImageId'
                       -> ModifyImageAttribute
mkModifyImageAttribute p1 = ModifyImageAttribute
    { _miaImageId = p1
    , _miaAttribute = Nothing
    , _miaOperationType = Nothing
    , _miaUserIds = mempty
    , _miaUserGroups = mempty
    , _miaProductCodes = mempty
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
miaUserIds :: Lens' ModifyImageAttribute [Text]
miaUserIds = lens _miaUserIds (\s a -> s { _miaUserIds = a })

-- | One or more user groups. This is only valid when modifying the
-- launchPermission attribute.
miaUserGroups :: Lens' ModifyImageAttribute [Text]
miaUserGroups = lens _miaUserGroups (\s a -> s { _miaUserGroups = a })

-- | One or more product codes. After you add a product code to an AMI, it can't
-- be removed. This is only valid when modifying the productCodes attribute.
miaProductCodes :: Lens' ModifyImageAttribute [Text]
miaProductCodes = lens _miaProductCodes (\s a -> s { _miaProductCodes = a })

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
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyImageAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkModifyImageAttributeResponse :: ModifyImageAttributeResponse
mkModifyImageAttributeResponse = ModifyImageAttributeResponse

instance AWSRequest ModifyImageAttribute where
    type Sv ModifyImageAttribute = EC2
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse

    request = post "ModifyImageAttribute"
    response _ = nullaryResponse ModifyImageAttributeResponse
