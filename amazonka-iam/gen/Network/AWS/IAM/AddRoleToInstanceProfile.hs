{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data AddRoleToInstanceProfile = AddRoleToInstanceProfile
    { _artipInstanceProfileName :: Text
    , _artipRoleName            :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddRoleToInstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artipInstanceProfileName' @::@ 'Text'
--
-- * 'artipRoleName' @::@ 'Text'
--
addRoleToInstanceProfile :: Text -- ^ 'artipInstanceProfileName'
                         -> Text -- ^ 'artipRoleName'
                         -> AddRoleToInstanceProfile
addRoleToInstanceProfile p1 p2 = AddRoleToInstanceProfile
    { _artipInstanceProfileName = p1
    , _artipRoleName            = p2
    }

-- | The name of the instance profile to update.
artipInstanceProfileName :: Lens' AddRoleToInstanceProfile Text
artipInstanceProfileName =
    lens _artipInstanceProfileName
        (\s a -> s { _artipInstanceProfileName = a })

-- | The name of the role to add.
artipRoleName :: Lens' AddRoleToInstanceProfile Text
artipRoleName = lens _artipRoleName (\s a -> s { _artipRoleName = a })

instance ToQuery AddRoleToInstanceProfile

instance ToPath AddRoleToInstanceProfile where
    toPath = const "/"

data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddRoleToInstanceProfileResponse' constructor.
addRoleToInstanceProfileResponse :: AddRoleToInstanceProfileResponse
addRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse

instance AWSRequest AddRoleToInstanceProfile where
    type Sv AddRoleToInstanceProfile = IAM
    type Rs AddRoleToInstanceProfile = AddRoleToInstanceProfileResponse

    request  = post "AddRoleToInstanceProfile"
    response = nullaryResponse AddRoleToInstanceProfileResponse
