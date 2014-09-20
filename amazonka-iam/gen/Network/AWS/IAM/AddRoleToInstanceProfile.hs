{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds the specified role to the specified instance profile. For more
-- information about roles, go to Working with Roles. For more information
-- about instance profiles, go to About Instance Profiles.
-- https://iam.amazonaws.com/ ?Action=AddRoleToInstanceProfile
-- &InstanceProfileName=Webserver &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS 12657608-99f2-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.AddRoleToInstanceProfile
    (
    -- * Request
      AddRoleToInstanceProfile
    -- ** Request constructor
    , addRoleToInstanceProfile
    -- ** Request lenses
    , artipInstanceProfileName
    , artipRoleName

    -- * Response
    , AddRoleToInstanceProfileResponse
    -- ** Response constructor
    , addRoleToInstanceProfileResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data AddRoleToInstanceProfile = AddRoleToInstanceProfile
    { _artipInstanceProfileName :: Text
    , _artipRoleName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddRoleToInstanceProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceProfileName ::@ @Text@
--
-- * @RoleName ::@ @Text@
--
addRoleToInstanceProfile :: Text -- ^ 'artipInstanceProfileName'
                         -> Text -- ^ 'artipRoleName'
                         -> AddRoleToInstanceProfile
addRoleToInstanceProfile p1 p2 = AddRoleToInstanceProfile
    { _artipInstanceProfileName = p1
    , _artipRoleName = p2
    }

-- | Name of the instance profile to update.
artipInstanceProfileName :: Lens' AddRoleToInstanceProfile Text
artipInstanceProfileName =
    lens _artipInstanceProfileName
         (\s a -> s { _artipInstanceProfileName = a })

-- | Name of the role to add.
artipRoleName :: Lens' AddRoleToInstanceProfile Text
artipRoleName = lens _artipRoleName (\s a -> s { _artipRoleName = a })

instance ToQuery AddRoleToInstanceProfile where
    toQuery = genericQuery def

data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddRoleToInstanceProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
addRoleToInstanceProfileResponse :: AddRoleToInstanceProfileResponse
addRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse

instance AWSRequest AddRoleToInstanceProfile where
    type Sv AddRoleToInstanceProfile = IAM
    type Rs AddRoleToInstanceProfile = AddRoleToInstanceProfileResponse

    request = post "AddRoleToInstanceProfile"
    response _ = nullaryResponse AddRoleToInstanceProfileResponse
