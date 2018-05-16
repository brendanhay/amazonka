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
-- Module      : Network.AWS.AlexaBusiness.RevokeInvitation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes an invitation and invalidates the enrollment URL.
--
--
module Network.AWS.AlexaBusiness.RevokeInvitation
    (
    -- * Creating a Request
      revokeInvitation
    , RevokeInvitation
    -- * Request Lenses
    , riEnrollmentId
    , riUserARN

    -- * Destructuring the Response
    , revokeInvitationResponse
    , RevokeInvitationResponse
    -- * Response Lenses
    , rirsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'revokeInvitation' smart constructor.
data RevokeInvitation = RevokeInvitation'
  { _riEnrollmentId :: !(Maybe Text)
  , _riUserARN      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeInvitation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riEnrollmentId' - The ARN of the enrollment invitation to revoke. Required.
--
-- * 'riUserARN' - The ARN of the user for whom to revoke an enrollment invitation. Required.
revokeInvitation
    :: RevokeInvitation
revokeInvitation =
  RevokeInvitation' {_riEnrollmentId = Nothing, _riUserARN = Nothing}


-- | The ARN of the enrollment invitation to revoke. Required.
riEnrollmentId :: Lens' RevokeInvitation (Maybe Text)
riEnrollmentId = lens _riEnrollmentId (\ s a -> s{_riEnrollmentId = a})

-- | The ARN of the user for whom to revoke an enrollment invitation. Required.
riUserARN :: Lens' RevokeInvitation (Maybe Text)
riUserARN = lens _riUserARN (\ s a -> s{_riUserARN = a})

instance AWSRequest RevokeInvitation where
        type Rs RevokeInvitation = RevokeInvitationResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 RevokeInvitationResponse' <$> (pure (fromEnum s)))

instance Hashable RevokeInvitation where

instance NFData RevokeInvitation where

instance ToHeaders RevokeInvitation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.RevokeInvitation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RevokeInvitation where
        toJSON RevokeInvitation'{..}
          = object
              (catMaybes
                 [("EnrollmentId" .=) <$> _riEnrollmentId,
                  ("UserArn" .=) <$> _riUserARN])

instance ToPath RevokeInvitation where
        toPath = const "/"

instance ToQuery RevokeInvitation where
        toQuery = const mempty

-- | /See:/ 'revokeInvitationResponse' smart constructor.
newtype RevokeInvitationResponse = RevokeInvitationResponse'
  { _rirsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeInvitationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirsResponseStatus' - -- | The response status code.
revokeInvitationResponse
    :: Int -- ^ 'rirsResponseStatus'
    -> RevokeInvitationResponse
revokeInvitationResponse pResponseStatus_ =
  RevokeInvitationResponse' {_rirsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rirsResponseStatus :: Lens' RevokeInvitationResponse Int
rirsResponseStatus = lens _rirsResponseStatus (\ s a -> s{_rirsResponseStatus = a})

instance NFData RevokeInvitationResponse where
