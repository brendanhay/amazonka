{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    -- ** Default constructor
    , modifyImageAttribute
    -- ** Accessors and lenses
    , _miarImageId
    , miarImageId
    , _miarDescription
    , miarDescription
    , _miarLaunchPermission
    , miarLaunchPermission
    , _miarProductCodes
    , miarProductCodes
    , _miarUserGroups
    , miarUserGroups
    , _miarUserIds
    , miarUserIds
    , _miarAttribute
    , miarAttribute
    , _miarOperationType
    , miarOperationType
    , _miarValue
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

makeSiglessLenses ''ModifyImageAttribute

instance ToQuery ModifyImageAttribute where
    toQuery = genericQuery def

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Show, Generic)

makeSiglessLenses ''ModifyImageAttributeResponse

instance AWSRequest ModifyImageAttribute where
    type Sv ModifyImageAttribute = EC2
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse

    request = post "ModifyImageAttribute"
    response _ = nullaryResponse ModifyImageAttributeResponse

-- | The ID of the AMI.
miarImageId :: Lens' ModifyImageAttribute (Text)

-- | A description for the AMI.
miarDescription :: Lens' ModifyImageAttribute (Maybe AttributeValue)

-- | 
miarLaunchPermission :: Lens' ModifyImageAttribute (Maybe LaunchPermissionModifications)

-- | One or more product codes. After you add a product code to an AMI, it can't
-- be removed. This is only valid when modifying the productCodes attribute.
miarProductCodes :: Lens' ModifyImageAttribute ([Text])

-- | One or more user groups. This is only valid when modifying the
-- launchPermission attribute.
miarUserGroups :: Lens' ModifyImageAttribute ([Text])

-- | One or more AWS account IDs. This is only valid when modifying the
-- launchPermission attribute.
miarUserIds :: Lens' ModifyImageAttribute ([Text])

-- | The name of the attribute to modify.
miarAttribute :: Lens' ModifyImageAttribute (Maybe Text)

-- | The operation type.
miarOperationType :: Lens' ModifyImageAttribute (Maybe Text)

-- | The value of the attribute being modified. This is only valid when
-- modifying the description attribute.
miarValue :: Lens' ModifyImageAttribute (Maybe Text)
