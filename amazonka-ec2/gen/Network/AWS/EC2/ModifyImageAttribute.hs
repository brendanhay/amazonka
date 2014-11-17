{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyImageAttribute.html>
module Network.AWS.EC2.ModifyImageAttribute
    (
    -- * Request
      ModifyImageAttribute
    -- ** Request constructor
    , modifyImageAttribute
    -- ** Request lenses
    , miaAttribute
    , miaDescription
    , miaDryRun
    , miaImageId
    , miaLaunchPermission
    , miaOperationType
    , miaProductCodes
    , miaUserGroups
    , miaUserIds
    , miaValue

    -- * Response
    , ModifyImageAttributeResponse
    -- ** Response constructor
    , modifyImageAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifyImageAttribute = ModifyImageAttribute
    { _miaAttribute        :: Maybe Text
    , _miaDescription      :: Maybe AttributeValue
    , _miaDryRun           :: Maybe Bool
    , _miaImageId          :: Text
    , _miaLaunchPermission :: Maybe LaunchPermissionModifications
    , _miaOperationType    :: Maybe Text
    , _miaProductCodes     :: [Text]
    , _miaUserGroups       :: [Text]
    , _miaUserIds          :: [Text]
    , _miaValue            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ModifyImageAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'miaAttribute' @::@ 'Maybe' 'Text'
--
-- * 'miaDescription' @::@ 'Maybe' 'AttributeValue'
--
-- * 'miaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'miaImageId' @::@ 'Text'
--
-- * 'miaLaunchPermission' @::@ 'Maybe' 'LaunchPermissionModifications'
--
-- * 'miaOperationType' @::@ 'Maybe' 'Text'
--
-- * 'miaProductCodes' @::@ ['Text']
--
-- * 'miaUserGroups' @::@ ['Text']
--
-- * 'miaUserIds' @::@ ['Text']
--
-- * 'miaValue' @::@ 'Maybe' 'Text'
--
modifyImageAttribute :: Text -- ^ 'miaImageId'
                     -> ModifyImageAttribute
modifyImageAttribute p1 = ModifyImageAttribute
    { _miaImageId          = p1
    , _miaDryRun           = Nothing
    , _miaAttribute        = Nothing
    , _miaOperationType    = Nothing
    , _miaUserIds          = mempty
    , _miaUserGroups       = mempty
    , _miaProductCodes     = mempty
    , _miaValue            = Nothing
    , _miaLaunchPermission = Nothing
    , _miaDescription      = Nothing
    }

-- | The name of the attribute to modify.
miaAttribute :: Lens' ModifyImageAttribute (Maybe Text)
miaAttribute = lens _miaAttribute (\s a -> s { _miaAttribute = a })

-- | A description for the AMI.
miaDescription :: Lens' ModifyImageAttribute (Maybe AttributeValue)
miaDescription = lens _miaDescription (\s a -> s { _miaDescription = a })

miaDryRun :: Lens' ModifyImageAttribute (Maybe Bool)
miaDryRun = lens _miaDryRun (\s a -> s { _miaDryRun = a })

-- | The ID of the AMI.
miaImageId :: Lens' ModifyImageAttribute Text
miaImageId = lens _miaImageId (\s a -> s { _miaImageId = a })

-- | 
miaLaunchPermission :: Lens' ModifyImageAttribute (Maybe LaunchPermissionModifications)
miaLaunchPermission =
    lens _miaLaunchPermission (\s a -> s { _miaLaunchPermission = a })

-- | The operation type.
miaOperationType :: Lens' ModifyImageAttribute (Maybe Text)
miaOperationType = lens _miaOperationType (\s a -> s { _miaOperationType = a })

-- | One or more product codes. After you add a product code to an AMI, it
-- can't be removed. This is only valid when modifying the productCodes
-- attribute.
miaProductCodes :: Lens' ModifyImageAttribute [Text]
miaProductCodes = lens _miaProductCodes (\s a -> s { _miaProductCodes = a })

-- | One or more user groups. This is only valid when modifying the
-- launchPermission attribute.
miaUserGroups :: Lens' ModifyImageAttribute [Text]
miaUserGroups = lens _miaUserGroups (\s a -> s { _miaUserGroups = a })

-- | One or more AWS account IDs. This is only valid when modifying the
-- launchPermission attribute.
miaUserIds :: Lens' ModifyImageAttribute [Text]
miaUserIds = lens _miaUserIds (\s a -> s { _miaUserIds = a })

-- | The value of the attribute being modified. This is only valid when
-- modifying the description attribute.
miaValue :: Lens' ModifyImageAttribute (Maybe Text)
miaValue = lens _miaValue (\s a -> s { _miaValue = a })

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ModifyImageAttributeResponse' constructor.
modifyImageAttributeResponse :: ModifyImageAttributeResponse
modifyImageAttributeResponse = ModifyImageAttributeResponse

instance AWSRequest ModifyImageAttribute where
    type Sv ModifyImageAttribute = EC2
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse

    request  = post "ModifyImageAttribute"
    response = nullResponse ModifyImageAttributeResponse

instance ToPath ModifyImageAttribute where
    toPath = const "/"

instance ToHeaders ModifyImageAttribute

instance ToQuery ModifyImageAttribute
