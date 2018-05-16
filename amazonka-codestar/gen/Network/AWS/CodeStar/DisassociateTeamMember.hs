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
-- Module      : Network.AWS.CodeStar.DisassociateTeamMember
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user from a project. Removing a user from a project also removes the IAM policies from that user that allowed access to the project and its resources. Disassociating a team member does not remove that user's profile from AWS CodeStar. It does not remove the user from IAM.
--
--
module Network.AWS.CodeStar.DisassociateTeamMember
    (
    -- * Creating a Request
      disassociateTeamMember
    , DisassociateTeamMember
    -- * Request Lenses
    , dtmProjectId
    , dtmUserARN

    -- * Destructuring the Response
    , disassociateTeamMemberResponse
    , DisassociateTeamMemberResponse
    -- * Response Lenses
    , dtmrsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateTeamMember' smart constructor.
data DisassociateTeamMember = DisassociateTeamMember'
  { _dtmProjectId :: !Text
  , _dtmUserARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateTeamMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmProjectId' - The ID of the AWS CodeStar project from which you want to remove a team member.
--
-- * 'dtmUserARN' - The Amazon Resource Name (ARN) of the IAM user or group whom you want to remove from the project.
disassociateTeamMember
    :: Text -- ^ 'dtmProjectId'
    -> Text -- ^ 'dtmUserARN'
    -> DisassociateTeamMember
disassociateTeamMember pProjectId_ pUserARN_ =
  DisassociateTeamMember' {_dtmProjectId = pProjectId_, _dtmUserARN = pUserARN_}


-- | The ID of the AWS CodeStar project from which you want to remove a team member.
dtmProjectId :: Lens' DisassociateTeamMember Text
dtmProjectId = lens _dtmProjectId (\ s a -> s{_dtmProjectId = a})

-- | The Amazon Resource Name (ARN) of the IAM user or group whom you want to remove from the project.
dtmUserARN :: Lens' DisassociateTeamMember Text
dtmUserARN = lens _dtmUserARN (\ s a -> s{_dtmUserARN = a})

instance AWSRequest DisassociateTeamMember where
        type Rs DisassociateTeamMember =
             DisassociateTeamMemberResponse
        request = postJSON codeStar
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateTeamMemberResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateTeamMember where

instance NFData DisassociateTeamMember where

instance ToHeaders DisassociateTeamMember where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.DisassociateTeamMember" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateTeamMember where
        toJSON DisassociateTeamMember'{..}
          = object
              (catMaybes
                 [Just ("projectId" .= _dtmProjectId),
                  Just ("userArn" .= _dtmUserARN)])

instance ToPath DisassociateTeamMember where
        toPath = const "/"

instance ToQuery DisassociateTeamMember where
        toQuery = const mempty

-- | /See:/ 'disassociateTeamMemberResponse' smart constructor.
newtype DisassociateTeamMemberResponse = DisassociateTeamMemberResponse'
  { _dtmrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateTeamMemberResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmrsResponseStatus' - -- | The response status code.
disassociateTeamMemberResponse
    :: Int -- ^ 'dtmrsResponseStatus'
    -> DisassociateTeamMemberResponse
disassociateTeamMemberResponse pResponseStatus_ =
  DisassociateTeamMemberResponse' {_dtmrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtmrsResponseStatus :: Lens' DisassociateTeamMemberResponse Int
dtmrsResponseStatus = lens _dtmrsResponseStatus (\ s a -> s{_dtmrsResponseStatus = a})

instance NFData DisassociateTeamMemberResponse where
