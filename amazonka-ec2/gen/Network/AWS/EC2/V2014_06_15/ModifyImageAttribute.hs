{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifyImageAttribute
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
module Network.AWS.EC2.V2014_06_15.ModifyImageAttribute
    (
    -- * Request
      ModifyImageAttribute
    -- ** Request constructor
    , mkModifyImageAttributeRequest
    -- ** Request lenses
    , miarImageId
    , miarAttribute
    , miarOperationType
    , miarUserIds
    , miarUserGroups
    , miarProductCodes
    , miarValue
    , miarLaunchPermission
    , miarDescription

    -- * Response
    , ModifyImageAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyImageAttribute' request.
mkModifyImageAttributeRequest :: Text -- ^ 'miarImageId'
                              -> ModifyImageAttribute
mkModifyImageAttributeRequest p1 = ModifyImageAttribute
    { _miarImageId = p1
    , _miarAttribute = Nothing
    , _miarOperationType = Nothing
    , _miarUserIds = mempty
    , _miarUserGroups = mempty
    , _miarProductCodes = mempty
    , _miarValue = Nothing
    , _miarLaunchPermission = Nothing
    , _miarDescription = Nothing
    }
{-# INLINE mkModifyImageAttributeRequest #-}

data ModifyImageAttribute = ModifyImageAttribute
    { _miarImageId :: Text
      -- ^ The ID of the AMI.
    , _miarAttribute :: Maybe Text
      -- ^ The name of the attribute to modify.
    , _miarOperationType :: Maybe Text
      -- ^ The operation type.
    , _miarUserIds :: [Text]
      -- ^ One or more AWS account IDs. This is only valid when modifying
      -- the launchPermission attribute.
    , _miarUserGroups :: [Text]
      -- ^ One or more user groups. This is only valid when modifying the
      -- launchPermission attribute.
    , _miarProductCodes :: [Text]
      -- ^ One or more product codes. After you add a product code to an
      -- AMI, it can't be removed. This is only valid when modifying the
      -- productCodes attribute.
    , _miarValue :: Maybe Text
      -- ^ The value of the attribute being modified. This is only valid
      -- when modifying the description attribute.
    , _miarLaunchPermission :: Maybe LaunchPermissionModifications
      -- ^ 
    , _miarDescription :: Maybe AttributeValue
      -- ^ A description for the AMI.
    } deriving (Show, Generic)

-- | The ID of the AMI.
miarImageId :: Lens' ModifyImageAttribute (Text)
miarImageId = lens _miarImageId (\s a -> s { _miarImageId = a })
{-# INLINE miarImageId #-}

-- | The name of the attribute to modify.
miarAttribute :: Lens' ModifyImageAttribute (Maybe Text)
miarAttribute = lens _miarAttribute (\s a -> s { _miarAttribute = a })
{-# INLINE miarAttribute #-}

-- | The operation type.
miarOperationType :: Lens' ModifyImageAttribute (Maybe Text)
miarOperationType = lens _miarOperationType (\s a -> s { _miarOperationType = a })
{-# INLINE miarOperationType #-}

-- | One or more AWS account IDs. This is only valid when modifying the
-- launchPermission attribute.
miarUserIds :: Lens' ModifyImageAttribute ([Text])
miarUserIds = lens _miarUserIds (\s a -> s { _miarUserIds = a })
{-# INLINE miarUserIds #-}

-- | One or more user groups. This is only valid when modifying the
-- launchPermission attribute.
miarUserGroups :: Lens' ModifyImageAttribute ([Text])
miarUserGroups = lens _miarUserGroups (\s a -> s { _miarUserGroups = a })
{-# INLINE miarUserGroups #-}

-- | One or more product codes. After you add a product code to an AMI, it can't
-- be removed. This is only valid when modifying the productCodes attribute.
miarProductCodes :: Lens' ModifyImageAttribute ([Text])
miarProductCodes = lens _miarProductCodes (\s a -> s { _miarProductCodes = a })
{-# INLINE miarProductCodes #-}

-- | The value of the attribute being modified. This is only valid when
-- modifying the description attribute.
miarValue :: Lens' ModifyImageAttribute (Maybe Text)
miarValue = lens _miarValue (\s a -> s { _miarValue = a })
{-# INLINE miarValue #-}

-- | 
miarLaunchPermission :: Lens' ModifyImageAttribute (Maybe LaunchPermissionModifications)
miarLaunchPermission = lens _miarLaunchPermission (\s a -> s { _miarLaunchPermission = a })
{-# INLINE miarLaunchPermission #-}

-- | A description for the AMI.
miarDescription :: Lens' ModifyImageAttribute (Maybe AttributeValue)
miarDescription = lens _miarDescription (\s a -> s { _miarDescription = a })
{-# INLINE miarDescription #-}

instance ToQuery ModifyImageAttribute where
    toQuery = genericQuery def

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ModifyImageAttribute where
    type Sv ModifyImageAttribute = EC2
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse

    request = post "ModifyImageAttribute"
    response _ = nullaryResponse ModifyImageAttributeResponse
