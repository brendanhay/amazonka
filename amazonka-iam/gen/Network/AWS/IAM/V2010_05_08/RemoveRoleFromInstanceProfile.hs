{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.RemoveRoleFromInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified role from the specified instance profile. Make sure
-- you do not have any Amazon EC2 instances running with the role you are
-- about to remove from the instance profile. Removing a role from an instance
-- profile that is associated with a running instance will break any
-- applications running on the instance. For more information about roles, go
-- to Working with Roles. For more information about instance profiles, go to
-- About Instance Profiles. https://iam.amazonaws.com/
-- ?Action=RemoveRoleFromInstanceProfile &InstanceProfileName=Webserver
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.RemoveRoleFromInstanceProfile
    (
    -- * Request
      RemoveRoleFromInstanceProfile
    -- ** Request constructor
    , mkRemoveRoleFromInstanceProfile
    -- ** Request lenses
    , rrfipInstanceProfileName
    , rrfipRoleName

    -- * Response
    , RemoveRoleFromInstanceProfileResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile
    { _rrfipInstanceProfileName :: Text
    , _rrfipRoleName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveRoleFromInstanceProfile' request.
mkRemoveRoleFromInstanceProfile :: Text -- ^ 'rrfipInstanceProfileName'
                                -> Text -- ^ 'rrfipRoleName'
                                -> RemoveRoleFromInstanceProfile
mkRemoveRoleFromInstanceProfile p1 p2 = RemoveRoleFromInstanceProfile
    { _rrfipInstanceProfileName = p1
    , _rrfipRoleName = p2
    }
{-# INLINE mkRemoveRoleFromInstanceProfile #-}

-- | Name of the instance profile to update.
rrfipInstanceProfileName :: Lens' RemoveRoleFromInstanceProfile Text
rrfipInstanceProfileName =
    lens _rrfipInstanceProfileName
         (\s a -> s { _rrfipInstanceProfileName = a })
{-# INLINE rrfipInstanceProfileName #-}

-- | Name of the role to remove.
rrfipRoleName :: Lens' RemoveRoleFromInstanceProfile Text
rrfipRoleName = lens _rrfipRoleName (\s a -> s { _rrfipRoleName = a })
{-# INLINE rrfipRoleName #-}

instance ToQuery RemoveRoleFromInstanceProfile where
    toQuery = genericQuery def

data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RemoveRoleFromInstanceProfile where
    type Sv RemoveRoleFromInstanceProfile = IAM
    type Rs RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfileResponse

    request = post "RemoveRoleFromInstanceProfile"
    response _ = nullaryResponse RemoveRoleFromInstanceProfileResponse
