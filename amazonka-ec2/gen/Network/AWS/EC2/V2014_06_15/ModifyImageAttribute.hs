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
    , modifyImageAttribute
    -- ** Request lenses
    , miarImageId
    , miarDescription
    , miarLaunchPermission
    , miarProductCodes
    , miarUserGroups
    , miarUserIds
    , miarAttribute
    , miarOperationType
    , miarValue

    -- * Response
    , ModifyImageAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyImageAttribute' request.
modifyImageAttribute :: Text -- ^ 'miarImageId'
                     -> ModifyImageAttribute
modifyImageAttribute p1 = ModifyImageAttribute
    { _miarImageId = p1
    , _miarDescription = Nothing
    , _miarLaunchPermission = Nothing
    , _miarProductCodes = mempty
    , _miarUserGroups = mempty
    , _miarUserIds = mempty
    , _miarAttribute = Nothing
    , _miarOperationType = Nothing
    , _miarValue = Nothing
    }
{-# INLINE modifyImageAttribute #-}

data ModifyImageAttribute = ModifyImageAttribute
    { _miarImageId :: Text
      -- ^ The ID of the AMI.
    , _miarDescription :: Maybe AttributeValue
      -- ^ A description for the AMI.
    , _miarLaunchPermission :: Maybe LaunchPermissionModifications
      -- ^ 
    , _miarProductCodes :: [Text]
      -- ^ One or more product codes. After you add a product code to an
      -- AMI, it can't be removed. This is only valid when modifying the
      -- productCodes attribute.
    , _miarUserGroups :: [Text]
      -- ^ One or more user groups. This is only valid when modifying the
      -- launchPermission attribute.
    , _miarUserIds :: [Text]
      -- ^ One or more AWS account IDs. This is only valid when modifying
      -- the launchPermission attribute.
    , _miarAttribute :: Maybe Text
      -- ^ The name of the attribute to modify.
    , _miarOperationType :: Maybe Text
      -- ^ The operation type.
    , _miarValue :: Maybe Text
      -- ^ The value of the attribute being modified. This is only valid
      -- when modifying the description attribute.
    } deriving (Show, Generic)

-- | The ID of the AMI.
miarImageId :: Lens' ModifyImageAttribute Text
miarImageId f x =
    f (_miarImageId x) <&> \y -> x { _miarImageId = y }
{-# INLINE miarImageId #-}

-- | A description for the AMI.
miarDescription :: Lens' ModifyImageAttribute (Maybe AttributeValue)
miarDescription f x =
    f (_miarDescription x) <&> \y -> x { _miarDescription = y }
{-# INLINE miarDescription #-}

-- | 
miarLaunchPermission :: Lens' ModifyImageAttribute (Maybe LaunchPermissionModifications)
miarLaunchPermission f x =
    f (_miarLaunchPermission x) <&> \y -> x { _miarLaunchPermission = y }
{-# INLINE miarLaunchPermission #-}

-- | One or more product codes. After you add a product code to an AMI, it can't
-- be removed. This is only valid when modifying the productCodes attribute.
miarProductCodes :: Lens' ModifyImageAttribute [Text]
miarProductCodes f x =
    f (_miarProductCodes x) <&> \y -> x { _miarProductCodes = y }
{-# INLINE miarProductCodes #-}

-- | One or more user groups. This is only valid when modifying the
-- launchPermission attribute.
miarUserGroups :: Lens' ModifyImageAttribute [Text]
miarUserGroups f x =
    f (_miarUserGroups x) <&> \y -> x { _miarUserGroups = y }
{-# INLINE miarUserGroups #-}

-- | One or more AWS account IDs. This is only valid when modifying the
-- launchPermission attribute.
miarUserIds :: Lens' ModifyImageAttribute [Text]
miarUserIds f x =
    f (_miarUserIds x) <&> \y -> x { _miarUserIds = y }
{-# INLINE miarUserIds #-}

-- | The name of the attribute to modify.
miarAttribute :: Lens' ModifyImageAttribute (Maybe Text)
miarAttribute f x =
    f (_miarAttribute x) <&> \y -> x { _miarAttribute = y }
{-# INLINE miarAttribute #-}

-- | The operation type.
miarOperationType :: Lens' ModifyImageAttribute (Maybe Text)
miarOperationType f x =
    f (_miarOperationType x) <&> \y -> x { _miarOperationType = y }
{-# INLINE miarOperationType #-}

-- | The value of the attribute being modified. This is only valid when
-- modifying the description attribute.
miarValue :: Lens' ModifyImageAttribute (Maybe Text)
miarValue f x =
    f (_miarValue x) <&> \y -> x { _miarValue = y }
{-# INLINE miarValue #-}

instance ToQuery ModifyImageAttribute where
    toQuery = genericQuery def

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ModifyImageAttribute where
    type Sv ModifyImageAttribute = EC2
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse

    request = post "ModifyImageAttribute"
    response _ = nullaryResponse ModifyImageAttributeResponse
