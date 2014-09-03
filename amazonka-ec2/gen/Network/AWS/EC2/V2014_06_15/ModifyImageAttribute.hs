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
miarImageId
    :: Functor f
    => (Text
    -> f (Text))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarImageId f x =
    (\y -> x { _miarImageId = y })
       <$> f (_miarImageId x)
{-# INLINE miarImageId #-}

-- | A description for the AMI.
miarDescription
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarDescription f x =
    (\y -> x { _miarDescription = y })
       <$> f (_miarDescription x)
{-# INLINE miarDescription #-}

-- | 
miarLaunchPermission
    :: Functor f
    => (Maybe LaunchPermissionModifications
    -> f (Maybe LaunchPermissionModifications))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarLaunchPermission f x =
    (\y -> x { _miarLaunchPermission = y })
       <$> f (_miarLaunchPermission x)
{-# INLINE miarLaunchPermission #-}

-- | One or more product codes. After you add a product code to an AMI, it can't
-- be removed. This is only valid when modifying the productCodes attribute.
miarProductCodes
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarProductCodes f x =
    (\y -> x { _miarProductCodes = y })
       <$> f (_miarProductCodes x)
{-# INLINE miarProductCodes #-}

-- | One or more user groups. This is only valid when modifying the
-- launchPermission attribute.
miarUserGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarUserGroups f x =
    (\y -> x { _miarUserGroups = y })
       <$> f (_miarUserGroups x)
{-# INLINE miarUserGroups #-}

-- | One or more AWS account IDs. This is only valid when modifying the
-- launchPermission attribute.
miarUserIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarUserIds f x =
    (\y -> x { _miarUserIds = y })
       <$> f (_miarUserIds x)
{-# INLINE miarUserIds #-}

-- | The name of the attribute to modify.
miarAttribute
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarAttribute f x =
    (\y -> x { _miarAttribute = y })
       <$> f (_miarAttribute x)
{-# INLINE miarAttribute #-}

-- | The operation type.
miarOperationType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarOperationType f x =
    (\y -> x { _miarOperationType = y })
       <$> f (_miarOperationType x)
{-# INLINE miarOperationType #-}

-- | The value of the attribute being modified. This is only valid when
-- modifying the description attribute.
miarValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyImageAttribute
    -> f ModifyImageAttribute
miarValue f x =
    (\y -> x { _miarValue = y })
       <$> f (_miarValue x)
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
