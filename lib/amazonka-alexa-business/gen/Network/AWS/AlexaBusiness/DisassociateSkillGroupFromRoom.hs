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
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill group from a specified room. This disables all skills in the skill group on all devices in the room.
--
--
module Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
    (
    -- * Creating a Request
      disassociateSkillGroupFromRoom
    , DisassociateSkillGroupFromRoom
    -- * Request Lenses
    , dsgfrSkillGroupARN
    , dsgfrRoomARN

    -- * Destructuring the Response
    , disassociateSkillGroupFromRoomResponse
    , DisassociateSkillGroupFromRoomResponse
    -- * Response Lenses
    , dsgfrrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateSkillGroupFromRoom' smart constructor.
data DisassociateSkillGroupFromRoom = DisassociateSkillGroupFromRoom'
  { _dsgfrSkillGroupARN :: !(Maybe Text)
  , _dsgfrRoomARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateSkillGroupFromRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgfrSkillGroupARN' - The ARN of the skill group to disassociate from a room. Required.
--
-- * 'dsgfrRoomARN' - The ARN of the room from which the skill group is to be disassociated. Required.
disassociateSkillGroupFromRoom
    :: DisassociateSkillGroupFromRoom
disassociateSkillGroupFromRoom =
  DisassociateSkillGroupFromRoom'
    {_dsgfrSkillGroupARN = Nothing, _dsgfrRoomARN = Nothing}


-- | The ARN of the skill group to disassociate from a room. Required.
dsgfrSkillGroupARN :: Lens' DisassociateSkillGroupFromRoom (Maybe Text)
dsgfrSkillGroupARN = lens _dsgfrSkillGroupARN (\ s a -> s{_dsgfrSkillGroupARN = a})

-- | The ARN of the room from which the skill group is to be disassociated. Required.
dsgfrRoomARN :: Lens' DisassociateSkillGroupFromRoom (Maybe Text)
dsgfrRoomARN = lens _dsgfrRoomARN (\ s a -> s{_dsgfrRoomARN = a})

instance AWSRequest DisassociateSkillGroupFromRoom
         where
        type Rs DisassociateSkillGroupFromRoom =
             DisassociateSkillGroupFromRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateSkillGroupFromRoomResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateSkillGroupFromRoom
         where

instance NFData DisassociateSkillGroupFromRoom where

instance ToHeaders DisassociateSkillGroupFromRoom
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DisassociateSkillGroupFromRoom" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateSkillGroupFromRoom where
        toJSON DisassociateSkillGroupFromRoom'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _dsgfrSkillGroupARN,
                  ("RoomArn" .=) <$> _dsgfrRoomARN])

instance ToPath DisassociateSkillGroupFromRoom where
        toPath = const "/"

instance ToQuery DisassociateSkillGroupFromRoom where
        toQuery = const mempty

-- | /See:/ 'disassociateSkillGroupFromRoomResponse' smart constructor.
newtype DisassociateSkillGroupFromRoomResponse = DisassociateSkillGroupFromRoomResponse'
  { _dsgfrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateSkillGroupFromRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgfrrsResponseStatus' - -- | The response status code.
disassociateSkillGroupFromRoomResponse
    :: Int -- ^ 'dsgfrrsResponseStatus'
    -> DisassociateSkillGroupFromRoomResponse
disassociateSkillGroupFromRoomResponse pResponseStatus_ =
  DisassociateSkillGroupFromRoomResponse'
    {_dsgfrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsgfrrsResponseStatus :: Lens' DisassociateSkillGroupFromRoomResponse Int
dsgfrrsResponseStatus = lens _dsgfrrsResponseStatus (\ s a -> s{_dsgfrrsResponseStatus = a})

instance NFData
           DisassociateSkillGroupFromRoomResponse
         where
