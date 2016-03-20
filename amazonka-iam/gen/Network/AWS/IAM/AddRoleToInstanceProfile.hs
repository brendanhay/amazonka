{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified role to the specified instance profile. For more
-- information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
-- For more information about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
module Network.AWS.IAM.AddRoleToInstanceProfile
    (
    -- * Creating a Request
      addRoleToInstanceProfile
    , AddRoleToInstanceProfile
    -- * Request Lenses
    , artipInstanceProfileName
    , artipRoleName

    -- * Destructuring the Response
    , addRoleToInstanceProfileResponse
    , AddRoleToInstanceProfileResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addRoleToInstanceProfile' smart constructor.
data AddRoleToInstanceProfile = AddRoleToInstanceProfile'
    { _artipInstanceProfileName :: !Text
    , _artipRoleName            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddRoleToInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artipInstanceProfileName'
--
-- * 'artipRoleName'
addRoleToInstanceProfile
    :: Text -- ^ 'artipInstanceProfileName'
    -> Text -- ^ 'artipRoleName'
    -> AddRoleToInstanceProfile
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
        type Rs AddRoleToInstanceProfile =
             AddRoleToInstanceProfileResponse
        request = postQuery iAM
        response
          = receiveNull AddRoleToInstanceProfileResponse'

instance Hashable AddRoleToInstanceProfile

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

-- | Creates a value of 'AddRoleToInstanceProfileResponse' with the minimum fields required to make a request.
--
addRoleToInstanceProfileResponse
    :: AddRoleToInstanceProfileResponse
addRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse'
