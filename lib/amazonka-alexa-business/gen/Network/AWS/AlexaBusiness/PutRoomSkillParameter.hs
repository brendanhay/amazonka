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
-- Module      : Network.AWS.AlexaBusiness.PutRoomSkillParameter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room skill parameter details by room, skill, and parameter key ID. Not all skills have a room skill parameter.
--
--
module Network.AWS.AlexaBusiness.PutRoomSkillParameter
    (
    -- * Creating a Request
      putRoomSkillParameter
    , PutRoomSkillParameter
    -- * Request Lenses
    , prspRoomARN
    , prspSkillId
    , prspRoomSkillParameter

    -- * Destructuring the Response
    , putRoomSkillParameterResponse
    , PutRoomSkillParameterResponse
    -- * Response Lenses
    , prsprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRoomSkillParameter' smart constructor.
data PutRoomSkillParameter = PutRoomSkillParameter'
  { _prspRoomARN            :: !(Maybe Text)
  , _prspSkillId            :: !Text
  , _prspRoomSkillParameter :: !RoomSkillParameter
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRoomSkillParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prspRoomARN' - The ARN of the room associated with the room skill parameter. Required.
--
-- * 'prspSkillId' - The ARN of the skill associated with the room skill parameter. Required.
--
-- * 'prspRoomSkillParameter' - The updated room skill parameter. Required.
putRoomSkillParameter
    :: Text -- ^ 'prspSkillId'
    -> RoomSkillParameter -- ^ 'prspRoomSkillParameter'
    -> PutRoomSkillParameter
putRoomSkillParameter pSkillId_ pRoomSkillParameter_ =
  PutRoomSkillParameter'
    { _prspRoomARN = Nothing
    , _prspSkillId = pSkillId_
    , _prspRoomSkillParameter = pRoomSkillParameter_
    }


-- | The ARN of the room associated with the room skill parameter. Required.
prspRoomARN :: Lens' PutRoomSkillParameter (Maybe Text)
prspRoomARN = lens _prspRoomARN (\ s a -> s{_prspRoomARN = a})

-- | The ARN of the skill associated with the room skill parameter. Required.
prspSkillId :: Lens' PutRoomSkillParameter Text
prspSkillId = lens _prspSkillId (\ s a -> s{_prspSkillId = a})

-- | The updated room skill parameter. Required.
prspRoomSkillParameter :: Lens' PutRoomSkillParameter RoomSkillParameter
prspRoomSkillParameter = lens _prspRoomSkillParameter (\ s a -> s{_prspRoomSkillParameter = a})

instance AWSRequest PutRoomSkillParameter where
        type Rs PutRoomSkillParameter =
             PutRoomSkillParameterResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 PutRoomSkillParameterResponse' <$>
                   (pure (fromEnum s)))

instance Hashable PutRoomSkillParameter where

instance NFData PutRoomSkillParameter where

instance ToHeaders PutRoomSkillParameter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.PutRoomSkillParameter" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRoomSkillParameter where
        toJSON PutRoomSkillParameter'{..}
          = object
              (catMaybes
                 [("RoomArn" .=) <$> _prspRoomARN,
                  Just ("SkillId" .= _prspSkillId),
                  Just
                    ("RoomSkillParameter" .= _prspRoomSkillParameter)])

instance ToPath PutRoomSkillParameter where
        toPath = const "/"

instance ToQuery PutRoomSkillParameter where
        toQuery = const mempty

-- | /See:/ 'putRoomSkillParameterResponse' smart constructor.
newtype PutRoomSkillParameterResponse = PutRoomSkillParameterResponse'
  { _prsprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRoomSkillParameterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsprsResponseStatus' - -- | The response status code.
putRoomSkillParameterResponse
    :: Int -- ^ 'prsprsResponseStatus'
    -> PutRoomSkillParameterResponse
putRoomSkillParameterResponse pResponseStatus_ =
  PutRoomSkillParameterResponse' {_prsprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
prsprsResponseStatus :: Lens' PutRoomSkillParameterResponse Int
prsprsResponseStatus = lens _prsprsResponseStatus (\ s a -> s{_prsprsResponseStatus = a})

instance NFData PutRoomSkillParameterResponse where
