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
-- Module      : Network.AWS.IAM.RemoveRoleFromInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified IAM role from the specified EC2 instance profile.
--
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the role you are about to remove from the instance profile. Removing a role from an instance profile that is associated with a running instance might break any applications running on the instance.
--
-- For more information about IAM roles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> . For more information about instance profiles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
module Network.AWS.IAM.RemoveRoleFromInstanceProfile
    (
    -- * Creating a Request
      removeRoleFromInstanceProfile
    , RemoveRoleFromInstanceProfile
    -- * Request Lenses
    , rrfipInstanceProfileName
    , rrfipRoleName

    -- * Destructuring the Response
    , removeRoleFromInstanceProfileResponse
    , RemoveRoleFromInstanceProfileResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeRoleFromInstanceProfile' smart constructor.
data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile'
  { _rrfipInstanceProfileName :: !Text
  , _rrfipRoleName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveRoleFromInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrfipInstanceProfileName' - The name of the instance profile to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'rrfipRoleName' - The name of the role to remove. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
removeRoleFromInstanceProfile
    :: Text -- ^ 'rrfipInstanceProfileName'
    -> Text -- ^ 'rrfipRoleName'
    -> RemoveRoleFromInstanceProfile
removeRoleFromInstanceProfile pInstanceProfileName_ pRoleName_ =
  RemoveRoleFromInstanceProfile'
    { _rrfipInstanceProfileName = pInstanceProfileName_
    , _rrfipRoleName = pRoleName_
    }


-- | The name of the instance profile to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
rrfipInstanceProfileName :: Lens' RemoveRoleFromInstanceProfile Text
rrfipInstanceProfileName = lens _rrfipInstanceProfileName (\ s a -> s{_rrfipInstanceProfileName = a})

-- | The name of the role to remove. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
rrfipRoleName :: Lens' RemoveRoleFromInstanceProfile Text
rrfipRoleName = lens _rrfipRoleName (\ s a -> s{_rrfipRoleName = a})

instance AWSRequest RemoveRoleFromInstanceProfile
         where
        type Rs RemoveRoleFromInstanceProfile =
             RemoveRoleFromInstanceProfileResponse
        request = postQuery iam
        response
          = receiveNull RemoveRoleFromInstanceProfileResponse'

instance Hashable RemoveRoleFromInstanceProfile where

instance NFData RemoveRoleFromInstanceProfile where

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
               "InstanceProfileName" =: _rrfipInstanceProfileName,
               "RoleName" =: _rrfipRoleName]

-- | /See:/ 'removeRoleFromInstanceProfileResponse' smart constructor.
data RemoveRoleFromInstanceProfileResponse =
  RemoveRoleFromInstanceProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveRoleFromInstanceProfileResponse' with the minimum fields required to make a request.
--
removeRoleFromInstanceProfileResponse
    :: RemoveRoleFromInstanceProfileResponse
removeRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse'


instance NFData RemoveRoleFromInstanceProfileResponse
         where
