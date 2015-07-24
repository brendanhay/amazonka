{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified role to the specified instance profile. For more
-- information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
-- For more information about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddRoleToInstanceProfile.html>
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

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addRoleToInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artipInstanceProfileName'
--
-- * 'artipRoleName'
data AddRoleToInstanceProfile = AddRoleToInstanceProfile'
    { _artipInstanceProfileName :: !Text
    , _artipRoleName            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddRoleToInstanceProfile' smart constructor.
addRoleToInstanceProfile :: Text -> Text -> AddRoleToInstanceProfile
addRoleToInstanceProfile pInstanceProfileName_ pRoleName_ =
    AddRoleToInstanceProfile'
    { _artipInstanceProfileName = pInstanceProfileName_
    , _artipRoleName = pRoleName_
    }

-- | The name of the instance profile to update.
artipInstanceProfileName :: Lens' AddRoleToInstanceProfile Text
artipInstanceProfileName = lens _artipInstanceProfileName (\ s a -> s{_artipInstanceProfileName = a});

-- | The name of the role to add.
artipRoleName :: Lens' AddRoleToInstanceProfile Text
artipRoleName = lens _artipRoleName (\ s a -> s{_artipRoleName = a});

instance AWSRequest AddRoleToInstanceProfile where
        type Sv AddRoleToInstanceProfile = IAM
        type Rs AddRoleToInstanceProfile =
             AddRoleToInstanceProfileResponse
        request = post "AddRoleToInstanceProfile"
        response
          = receiveNull AddRoleToInstanceProfileResponse'

instance ToHeaders AddRoleToInstanceProfile where
        toHeaders = const mempty

instance ToPath AddRoleToInstanceProfile where
        toPath = const "/"

instance ToQuery AddRoleToInstanceProfile where
        toQuery AddRoleToInstanceProfile'{..}
          = mconcat
              ["Action" =:
                 ("AddRoleToInstanceProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "InstanceProfileName" =: _artipInstanceProfileName,
               "RoleName" =: _artipRoleName]

-- | /See:/ 'addRoleToInstanceProfileResponse' smart constructor.
data AddRoleToInstanceProfileResponse =
    AddRoleToInstanceProfileResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddRoleToInstanceProfileResponse' smart constructor.
addRoleToInstanceProfileResponse :: AddRoleToInstanceProfileResponse
addRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse'
