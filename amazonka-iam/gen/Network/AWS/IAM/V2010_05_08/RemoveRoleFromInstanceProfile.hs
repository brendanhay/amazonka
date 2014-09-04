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
    , removeRoleFromInstanceProfile
    -- ** Request lenses
    , rrfiprInstanceProfileName
    , rrfiprRoleName

    -- * Response
    , RemoveRoleFromInstanceProfileResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RemoveRoleFromInstanceProfile' request.
removeRoleFromInstanceProfile :: Text -- ^ 'rrfiprInstanceProfileName'
                              -> Text -- ^ 'rrfiprRoleName'
                              -> RemoveRoleFromInstanceProfile
removeRoleFromInstanceProfile p1 p2 = RemoveRoleFromInstanceProfile
    { _rrfiprInstanceProfileName = p1
    , _rrfiprRoleName = p2
    }
{-# INLINE removeRoleFromInstanceProfile #-}

data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile
    { _rrfiprInstanceProfileName :: Text
      -- ^ Name of the instance profile to update.
    , _rrfiprRoleName :: Text
      -- ^ Name of the role to remove.
    } deriving (Show, Generic)

-- | Name of the instance profile to update.
rrfiprInstanceProfileName :: Lens' RemoveRoleFromInstanceProfile (Text)
rrfiprInstanceProfileName f x =
    f (_rrfiprInstanceProfileName x)
        <&> \y -> x { _rrfiprInstanceProfileName = y }
{-# INLINE rrfiprInstanceProfileName #-}

-- | Name of the role to remove.
rrfiprRoleName :: Lens' RemoveRoleFromInstanceProfile (Text)
rrfiprRoleName f x =
    f (_rrfiprRoleName x)
        <&> \y -> x { _rrfiprRoleName = y }
{-# INLINE rrfiprRoleName #-}

instance ToQuery RemoveRoleFromInstanceProfile where
    toQuery = genericQuery def

data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RemoveRoleFromInstanceProfile where
    type Sv RemoveRoleFromInstanceProfile = IAM
    type Rs RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfileResponse

    request = post "RemoveRoleFromInstanceProfile"
    response _ = nullaryResponse RemoveRoleFromInstanceProfileResponse
