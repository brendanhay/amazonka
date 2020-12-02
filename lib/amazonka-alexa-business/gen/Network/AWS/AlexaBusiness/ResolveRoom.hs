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
-- Module      : Network.AWS.AlexaBusiness.ResolveRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the details for the room from which a skill request was invoked. This operation is used by skill developers.
--
--
module Network.AWS.AlexaBusiness.ResolveRoom
    (
    -- * Creating a Request
      resolveRoom
    , ResolveRoom
    -- * Request Lenses
    , rrUserId
    , rrSkillId

    -- * Destructuring the Response
    , resolveRoomResponse
    , ResolveRoomResponse
    -- * Response Lenses
    , rrrsRoomSkillParameters
    , rrrsRoomARN
    , rrrsRoomName
    , rrrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resolveRoom' smart constructor.
data ResolveRoom = ResolveRoom'
  { _rrUserId  :: !Text
  , _rrSkillId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrUserId' - The ARN of the user. Required.
--
-- * 'rrSkillId' - The ARN of the skill that was requested. Required.
resolveRoom
    :: Text -- ^ 'rrUserId'
    -> Text -- ^ 'rrSkillId'
    -> ResolveRoom
resolveRoom pUserId_ pSkillId_ =
  ResolveRoom' {_rrUserId = pUserId_, _rrSkillId = pSkillId_}


-- | The ARN of the user. Required.
rrUserId :: Lens' ResolveRoom Text
rrUserId = lens _rrUserId (\ s a -> s{_rrUserId = a})

-- | The ARN of the skill that was requested. Required.
rrSkillId :: Lens' ResolveRoom Text
rrSkillId = lens _rrSkillId (\ s a -> s{_rrSkillId = a})

instance AWSRequest ResolveRoom where
        type Rs ResolveRoom = ResolveRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 ResolveRoomResponse' <$>
                   (x .?> "RoomSkillParameters" .!@ mempty) <*>
                     (x .?> "RoomArn")
                     <*> (x .?> "RoomName")
                     <*> (pure (fromEnum s)))

instance Hashable ResolveRoom where

instance NFData ResolveRoom where

instance ToHeaders ResolveRoom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.ResolveRoom" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResolveRoom where
        toJSON ResolveRoom'{..}
          = object
              (catMaybes
                 [Just ("UserId" .= _rrUserId),
                  Just ("SkillId" .= _rrSkillId)])

instance ToPath ResolveRoom where
        toPath = const "/"

instance ToQuery ResolveRoom where
        toQuery = const mempty

-- | /See:/ 'resolveRoomResponse' smart constructor.
data ResolveRoomResponse = ResolveRoomResponse'
  { _rrrsRoomSkillParameters :: !(Maybe [RoomSkillParameter])
  , _rrrsRoomARN             :: !(Maybe Text)
  , _rrrsRoomName            :: !(Maybe Text)
  , _rrrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrrsRoomSkillParameters' - Response to get the room profile request. Required.
--
-- * 'rrrsRoomARN' - The ARN of the room from which the skill request was invoked.
--
-- * 'rrrsRoomName' - The name of the room from which the skill request was invoked.
--
-- * 'rrrsResponseStatus' - -- | The response status code.
resolveRoomResponse
    :: Int -- ^ 'rrrsResponseStatus'
    -> ResolveRoomResponse
resolveRoomResponse pResponseStatus_ =
  ResolveRoomResponse'
    { _rrrsRoomSkillParameters = Nothing
    , _rrrsRoomARN = Nothing
    , _rrrsRoomName = Nothing
    , _rrrsResponseStatus = pResponseStatus_
    }


-- | Response to get the room profile request. Required.
rrrsRoomSkillParameters :: Lens' ResolveRoomResponse [RoomSkillParameter]
rrrsRoomSkillParameters = lens _rrrsRoomSkillParameters (\ s a -> s{_rrrsRoomSkillParameters = a}) . _Default . _Coerce

-- | The ARN of the room from which the skill request was invoked.
rrrsRoomARN :: Lens' ResolveRoomResponse (Maybe Text)
rrrsRoomARN = lens _rrrsRoomARN (\ s a -> s{_rrrsRoomARN = a})

-- | The name of the room from which the skill request was invoked.
rrrsRoomName :: Lens' ResolveRoomResponse (Maybe Text)
rrrsRoomName = lens _rrrsRoomName (\ s a -> s{_rrrsRoomName = a})

-- | -- | The response status code.
rrrsResponseStatus :: Lens' ResolveRoomResponse Int
rrrsResponseStatus = lens _rrrsResponseStatus (\ s a -> s{_rrrsResponseStatus = a})

instance NFData ResolveRoomResponse where
