{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveRoleFromInstanceProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified role from the specified instance profile.
--
-- Make sure you do not have any Amazon EC2 instances running with the role
-- you are about to remove from the instance profile. Removing a role from
-- an instance profile that is associated with a running instance will
-- break any applications running on the instance.
--
-- For more information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
-- For more information about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveRoleFromInstanceProfile.html>
module Network.AWS.IAM.RemoveRoleFromInstanceProfile
    (
    -- * Request
      RemoveRoleFromInstanceProfile
    -- ** Request constructor
    , removeRoleFromInstanceProfile
    -- ** Request lenses
    , rrfiprqInstanceProfileName
    , rrfiprqRoleName

    -- * Response
    , RemoveRoleFromInstanceProfileResponse
    -- ** Response constructor
    , removeRoleFromInstanceProfileResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'removeRoleFromInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrfiprqInstanceProfileName'
--
-- * 'rrfiprqRoleName'
data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile'
    { _rrfiprqInstanceProfileName :: !Text
    , _rrfiprqRoleName            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveRoleFromInstanceProfile' smart constructor.
removeRoleFromInstanceProfile :: Text -> Text -> RemoveRoleFromInstanceProfile
removeRoleFromInstanceProfile pInstanceProfileName_ pRoleName_ =
    RemoveRoleFromInstanceProfile'
    { _rrfiprqInstanceProfileName = pInstanceProfileName_
    , _rrfiprqRoleName = pRoleName_
    }

-- | The name of the instance profile to update.
rrfiprqInstanceProfileName :: Lens' RemoveRoleFromInstanceProfile Text
rrfiprqInstanceProfileName = lens _rrfiprqInstanceProfileName (\ s a -> s{_rrfiprqInstanceProfileName = a});

-- | The name of the role to remove.
rrfiprqRoleName :: Lens' RemoveRoleFromInstanceProfile Text
rrfiprqRoleName = lens _rrfiprqRoleName (\ s a -> s{_rrfiprqRoleName = a});

instance AWSRequest RemoveRoleFromInstanceProfile
         where
        type Sv RemoveRoleFromInstanceProfile = IAM
        type Rs RemoveRoleFromInstanceProfile =
             RemoveRoleFromInstanceProfileResponse
        request = post
        response
          = receiveNull RemoveRoleFromInstanceProfileResponse'

instance ToHeaders RemoveRoleFromInstanceProfile
         where
        toHeaders = const mempty

instance ToPath RemoveRoleFromInstanceProfile where
        toPath = const "/"

instance ToQuery RemoveRoleFromInstanceProfile where
        toQuery RemoveRoleFromInstanceProfile'{..}
          = mconcat
              ["Action" =:
                 ("RemoveRoleFromInstanceProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "InstanceProfileName" =: _rrfiprqInstanceProfileName,
               "RoleName" =: _rrfiprqRoleName]

-- | /See:/ 'removeRoleFromInstanceProfileResponse' smart constructor.
data RemoveRoleFromInstanceProfileResponse =
    RemoveRoleFromInstanceProfileResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveRoleFromInstanceProfileResponse' smart constructor.
removeRoleFromInstanceProfileResponse :: RemoveRoleFromInstanceProfileResponse
removeRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse'
