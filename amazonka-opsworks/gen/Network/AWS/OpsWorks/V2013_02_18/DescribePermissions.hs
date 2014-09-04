{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribePermissions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the permissions for a specified stack. Required Permissions: To
-- use this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribePermissions
    (
    -- * Request
      DescribePermissions
    -- ** Request constructor
    , mkDescribePermissionsRequest
    -- ** Request lenses
    , dprIamUserArn
    , dprStackId

    -- * Response
    , DescribePermissionsResponse
    -- ** Response lenses
    , dpsPermissions
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribePermissions' request.
mkDescribePermissionsRequest :: DescribePermissions
mkDescribePermissionsRequest = DescribePermissions
    { _dprIamUserArn = Nothing
    , _dprStackId = Nothing
    }
{-# INLINE mkDescribePermissionsRequest #-}

data DescribePermissions = DescribePermissions
    { _dprIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN. For more information about IAM ARNs, see
      -- Using Identifiers.
    , _dprStackId :: Maybe Text
      -- ^ The stack ID.
    } deriving (Show, Generic)

-- | The user's IAM ARN. For more information about IAM ARNs, see Using
-- Identifiers.
dprIamUserArn :: Lens' DescribePermissions (Maybe Text)
dprIamUserArn = lens _dprIamUserArn (\s a -> s { _dprIamUserArn = a })
{-# INLINE dprIamUserArn #-}

-- | The stack ID.
dprStackId :: Lens' DescribePermissions (Maybe Text)
dprStackId = lens _dprStackId (\s a -> s { _dprStackId = a })
{-# INLINE dprStackId #-}

instance ToPath DescribePermissions

instance ToQuery DescribePermissions

instance ToHeaders DescribePermissions

instance ToJSON DescribePermissions

newtype DescribePermissionsResponse = DescribePermissionsResponse
    { _dpsPermissions :: [Permission]
      -- ^ An array of Permission objects that describe the stack
      -- permissions. If the request object contains only a stack ID, the
      -- array contains a Permission object with permissions for each of
      -- the stack IAM ARNs. If the request object contains only an IAM
      -- ARN, the array contains a Permission object with permissions for
      -- each of the user's stack IDs. If the request contains a stack ID
      -- and an IAM ARN, the array contains a single Permission object
      -- with permissions for the specified stack and IAM ARN.
    } deriving (Show, Generic)

-- | An array of Permission objects that describe the stack permissions. If the
-- request object contains only a stack ID, the array contains a Permission
-- object with permissions for each of the stack IAM ARNs. If the request
-- object contains only an IAM ARN, the array contains a Permission object
-- with permissions for each of the user's stack IDs. If the request contains
-- a stack ID and an IAM ARN, the array contains a single Permission object
-- with permissions for the specified stack and IAM ARN.
dpsPermissions :: Lens' DescribePermissionsResponse ([Permission])
dpsPermissions = lens _dpsPermissions (\s a -> s { _dpsPermissions = a })
{-# INLINE dpsPermissions #-}

instance FromJSON DescribePermissionsResponse

instance AWSRequest DescribePermissions where
    type Sv DescribePermissions = OpsWorks
    type Rs DescribePermissions = DescribePermissionsResponse

    request = get
    response _ = jsonResponse
