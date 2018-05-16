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
-- Module      : Network.AWS.CodeStar.UpdateTeamMember
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a team member's attributes in an AWS CodeStar project. For example, you can change a team member's role in the project, or change whether they have remote access to project resources.
--
--
module Network.AWS.CodeStar.UpdateTeamMember
    (
    -- * Creating a Request
      updateTeamMember
    , UpdateTeamMember
    -- * Request Lenses
    , utmRemoteAccessAllowed
    , utmProjectRole
    , utmProjectId
    , utmUserARN

    -- * Destructuring the Response
    , updateTeamMemberResponse
    , UpdateTeamMemberResponse
    -- * Response Lenses
    , utmrsUserARN
    , utmrsRemoteAccessAllowed
    , utmrsProjectRole
    , utmrsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTeamMember' smart constructor.
data UpdateTeamMember = UpdateTeamMember'
  { _utmRemoteAccessAllowed :: !(Maybe Bool)
  , _utmProjectRole         :: !(Maybe Text)
  , _utmProjectId           :: !Text
  , _utmUserARN             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTeamMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utmRemoteAccessAllowed' - Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile. Even if this is set to True, the user must associate a public key with their profile before the user can access resources.
--
-- * 'utmProjectRole' - The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
--
-- * 'utmProjectId' - The ID of the project.
--
-- * 'utmUserARN' - The Amazon Resource Name (ARN) of the user for whom you want to change team membership attributes.
updateTeamMember
    :: Text -- ^ 'utmProjectId'
    -> Text -- ^ 'utmUserARN'
    -> UpdateTeamMember
updateTeamMember pProjectId_ pUserARN_ =
  UpdateTeamMember'
    { _utmRemoteAccessAllowed = Nothing
    , _utmProjectRole = Nothing
    , _utmProjectId = pProjectId_
    , _utmUserARN = pUserARN_
    }


-- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile. Even if this is set to True, the user must associate a public key with their profile before the user can access resources.
utmRemoteAccessAllowed :: Lens' UpdateTeamMember (Maybe Bool)
utmRemoteAccessAllowed = lens _utmRemoteAccessAllowed (\ s a -> s{_utmRemoteAccessAllowed = a})

-- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
utmProjectRole :: Lens' UpdateTeamMember (Maybe Text)
utmProjectRole = lens _utmProjectRole (\ s a -> s{_utmProjectRole = a})

-- | The ID of the project.
utmProjectId :: Lens' UpdateTeamMember Text
utmProjectId = lens _utmProjectId (\ s a -> s{_utmProjectId = a})

-- | The Amazon Resource Name (ARN) of the user for whom you want to change team membership attributes.
utmUserARN :: Lens' UpdateTeamMember Text
utmUserARN = lens _utmUserARN (\ s a -> s{_utmUserARN = a})

instance AWSRequest UpdateTeamMember where
        type Rs UpdateTeamMember = UpdateTeamMemberResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTeamMemberResponse' <$>
                   (x .?> "userArn") <*> (x .?> "remoteAccessAllowed")
                     <*> (x .?> "projectRole")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateTeamMember where

instance NFData UpdateTeamMember where

instance ToHeaders UpdateTeamMember where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.UpdateTeamMember" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTeamMember where
        toJSON UpdateTeamMember'{..}
          = object
              (catMaybes
                 [("remoteAccessAllowed" .=) <$>
                    _utmRemoteAccessAllowed,
                  ("projectRole" .=) <$> _utmProjectRole,
                  Just ("projectId" .= _utmProjectId),
                  Just ("userArn" .= _utmUserARN)])

instance ToPath UpdateTeamMember where
        toPath = const "/"

instance ToQuery UpdateTeamMember where
        toQuery = const mempty

-- | /See:/ 'updateTeamMemberResponse' smart constructor.
data UpdateTeamMemberResponse = UpdateTeamMemberResponse'
  { _utmrsUserARN             :: !(Maybe Text)
  , _utmrsRemoteAccessAllowed :: !(Maybe Bool)
  , _utmrsProjectRole         :: !(Maybe Text)
  , _utmrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTeamMemberResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utmrsUserARN' - The Amazon Resource Name (ARN) of the user whose team membership attributes were updated.
--
-- * 'utmrsRemoteAccessAllowed' - Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile.
--
-- * 'utmrsProjectRole' - The project role granted to the user.
--
-- * 'utmrsResponseStatus' - -- | The response status code.
updateTeamMemberResponse
    :: Int -- ^ 'utmrsResponseStatus'
    -> UpdateTeamMemberResponse
updateTeamMemberResponse pResponseStatus_ =
  UpdateTeamMemberResponse'
    { _utmrsUserARN = Nothing
    , _utmrsRemoteAccessAllowed = Nothing
    , _utmrsProjectRole = Nothing
    , _utmrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the user whose team membership attributes were updated.
utmrsUserARN :: Lens' UpdateTeamMemberResponse (Maybe Text)
utmrsUserARN = lens _utmrsUserARN (\ s a -> s{_utmrsUserARN = a})

-- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile.
utmrsRemoteAccessAllowed :: Lens' UpdateTeamMemberResponse (Maybe Bool)
utmrsRemoteAccessAllowed = lens _utmrsRemoteAccessAllowed (\ s a -> s{_utmrsRemoteAccessAllowed = a})

-- | The project role granted to the user.
utmrsProjectRole :: Lens' UpdateTeamMemberResponse (Maybe Text)
utmrsProjectRole = lens _utmrsProjectRole (\ s a -> s{_utmrsProjectRole = a})

-- | -- | The response status code.
utmrsResponseStatus :: Lens' UpdateTeamMemberResponse Int
utmrsResponseStatus = lens _utmrsResponseStatus (\ s a -> s{_utmrsResponseStatus = a})

instance NFData UpdateTeamMemberResponse where
