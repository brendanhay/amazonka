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
-- Module      : Network.AWS.GuardDuty.AcceptInvitation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the invitation to be monitored by a master GuardDuty account.
module Network.AWS.GuardDuty.AcceptInvitation
    (
    -- * Creating a Request
      acceptInvitation
    , AcceptInvitation
    -- * Request Lenses
    , aiMasterId
    , aiInvitationId
    , aiDetectorId

    -- * Destructuring the Response
    , acceptInvitationResponse
    , AcceptInvitationResponse
    -- * Response Lenses
    , airsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | AcceptInvitation request body.
--
-- /See:/ 'acceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { _aiMasterId     :: !(Maybe Text)
  , _aiInvitationId :: !(Maybe Text)
  , _aiDetectorId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptInvitation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiMasterId' - The account ID of the master GuardDuty account whose invitation you're accepting.
--
-- * 'aiInvitationId' - This value is used to validate the master account to the member account.
--
-- * 'aiDetectorId' - The unique ID of the detector of the GuardDuty member account.
acceptInvitation
    :: Text -- ^ 'aiDetectorId'
    -> AcceptInvitation
acceptInvitation pDetectorId_ =
  AcceptInvitation'
    { _aiMasterId = Nothing
    , _aiInvitationId = Nothing
    , _aiDetectorId = pDetectorId_
    }


-- | The account ID of the master GuardDuty account whose invitation you're accepting.
aiMasterId :: Lens' AcceptInvitation (Maybe Text)
aiMasterId = lens _aiMasterId (\ s a -> s{_aiMasterId = a})

-- | This value is used to validate the master account to the member account.
aiInvitationId :: Lens' AcceptInvitation (Maybe Text)
aiInvitationId = lens _aiInvitationId (\ s a -> s{_aiInvitationId = a})

-- | The unique ID of the detector of the GuardDuty member account.
aiDetectorId :: Lens' AcceptInvitation Text
aiDetectorId = lens _aiDetectorId (\ s a -> s{_aiDetectorId = a})

instance AWSRequest AcceptInvitation where
        type Rs AcceptInvitation = AcceptInvitationResponse
        request = postJSON guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 AcceptInvitationResponse' <$> (pure (fromEnum s)))

instance Hashable AcceptInvitation where

instance NFData AcceptInvitation where

instance ToHeaders AcceptInvitation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcceptInvitation where
        toJSON AcceptInvitation'{..}
          = object
              (catMaybes
                 [("masterId" .=) <$> _aiMasterId,
                  ("invitationId" .=) <$> _aiInvitationId])

instance ToPath AcceptInvitation where
        toPath AcceptInvitation'{..}
          = mconcat
              ["/detector/", toBS _aiDetectorId, "/master"]

instance ToQuery AcceptInvitation where
        toQuery = const mempty

-- | /See:/ 'acceptInvitationResponse' smart constructor.
newtype AcceptInvitationResponse = AcceptInvitationResponse'
  { _airsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptInvitationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airsResponseStatus' - -- | The response status code.
acceptInvitationResponse
    :: Int -- ^ 'airsResponseStatus'
    -> AcceptInvitationResponse
acceptInvitationResponse pResponseStatus_ =
  AcceptInvitationResponse' {_airsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
airsResponseStatus :: Lens' AcceptInvitationResponse Int
airsResponseStatus = lens _airsResponseStatus (\ s a -> s{_airsResponseStatus = a})

instance NFData AcceptInvitationResponse where
