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
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill group with a given room. This enables all skills in the associated skill group on all devices in the room.
--
--
module Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
    (
    -- * Creating a Request
      associateSkillGroupWithRoom
    , AssociateSkillGroupWithRoom
    -- * Request Lenses
    , asgwrSkillGroupARN
    , asgwrRoomARN

    -- * Destructuring the Response
    , associateSkillGroupWithRoomResponse
    , AssociateSkillGroupWithRoomResponse
    -- * Response Lenses
    , asgwrrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateSkillGroupWithRoom' smart constructor.
data AssociateSkillGroupWithRoom = AssociateSkillGroupWithRoom'
  { _asgwrSkillGroupARN :: !(Maybe Text)
  , _asgwrRoomARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSkillGroupWithRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgwrSkillGroupARN' - The ARN of the skill group to associate with a room. Required.
--
-- * 'asgwrRoomARN' - The ARN of the room with which to associate the skill group. Required.
associateSkillGroupWithRoom
    :: AssociateSkillGroupWithRoom
associateSkillGroupWithRoom =
  AssociateSkillGroupWithRoom'
    {_asgwrSkillGroupARN = Nothing, _asgwrRoomARN = Nothing}


-- | The ARN of the skill group to associate with a room. Required.
asgwrSkillGroupARN :: Lens' AssociateSkillGroupWithRoom (Maybe Text)
asgwrSkillGroupARN = lens _asgwrSkillGroupARN (\ s a -> s{_asgwrSkillGroupARN = a})

-- | The ARN of the room with which to associate the skill group. Required.
asgwrRoomARN :: Lens' AssociateSkillGroupWithRoom (Maybe Text)
asgwrRoomARN = lens _asgwrRoomARN (\ s a -> s{_asgwrRoomARN = a})

instance AWSRequest AssociateSkillGroupWithRoom where
        type Rs AssociateSkillGroupWithRoom =
             AssociateSkillGroupWithRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateSkillGroupWithRoomResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateSkillGroupWithRoom where

instance NFData AssociateSkillGroupWithRoom where

instance ToHeaders AssociateSkillGroupWithRoom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.AssociateSkillGroupWithRoom" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateSkillGroupWithRoom where
        toJSON AssociateSkillGroupWithRoom'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _asgwrSkillGroupARN,
                  ("RoomArn" .=) <$> _asgwrRoomARN])

instance ToPath AssociateSkillGroupWithRoom where
        toPath = const "/"

instance ToQuery AssociateSkillGroupWithRoom where
        toQuery = const mempty

-- | /See:/ 'associateSkillGroupWithRoomResponse' smart constructor.
newtype AssociateSkillGroupWithRoomResponse = AssociateSkillGroupWithRoomResponse'
  { _asgwrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSkillGroupWithRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgwrrsResponseStatus' - -- | The response status code.
associateSkillGroupWithRoomResponse
    :: Int -- ^ 'asgwrrsResponseStatus'
    -> AssociateSkillGroupWithRoomResponse
associateSkillGroupWithRoomResponse pResponseStatus_ =
  AssociateSkillGroupWithRoomResponse'
    {_asgwrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
asgwrrsResponseStatus :: Lens' AssociateSkillGroupWithRoomResponse Int
asgwrrsResponseStatus = lens _asgwrrsResponseStatus (\ s a -> s{_asgwrrsResponseStatus = a})

instance NFData AssociateSkillGroupWithRoomResponse
         where
