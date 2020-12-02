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
-- Module      : Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes room skill parameter details by room, skill, and parameter key ID.
--
--
module Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
    (
    -- * Creating a Request
      deleteRoomSkillParameter
    , DeleteRoomSkillParameter
    -- * Request Lenses
    , drspRoomARN
    , drspSkillId
    , drspParameterKey

    -- * Destructuring the Response
    , deleteRoomSkillParameterResponse
    , DeleteRoomSkillParameterResponse
    -- * Response Lenses
    , drsprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRoomSkillParameter' smart constructor.
data DeleteRoomSkillParameter = DeleteRoomSkillParameter'
  { _drspRoomARN      :: !(Maybe Text)
  , _drspSkillId      :: !Text
  , _drspParameterKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoomSkillParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drspRoomARN' - The ARN of the room from which to remove the room skill parameter details.
--
-- * 'drspSkillId' - The ID of the skill from which to remove the room skill parameter details.
--
-- * 'drspParameterKey' - The room skill parameter key for which to remove details.
deleteRoomSkillParameter
    :: Text -- ^ 'drspSkillId'
    -> Text -- ^ 'drspParameterKey'
    -> DeleteRoomSkillParameter
deleteRoomSkillParameter pSkillId_ pParameterKey_ =
  DeleteRoomSkillParameter'
    { _drspRoomARN = Nothing
    , _drspSkillId = pSkillId_
    , _drspParameterKey = pParameterKey_
    }


-- | The ARN of the room from which to remove the room skill parameter details.
drspRoomARN :: Lens' DeleteRoomSkillParameter (Maybe Text)
drspRoomARN = lens _drspRoomARN (\ s a -> s{_drspRoomARN = a})

-- | The ID of the skill from which to remove the room skill parameter details.
drspSkillId :: Lens' DeleteRoomSkillParameter Text
drspSkillId = lens _drspSkillId (\ s a -> s{_drspSkillId = a})

-- | The room skill parameter key for which to remove details.
drspParameterKey :: Lens' DeleteRoomSkillParameter Text
drspParameterKey = lens _drspParameterKey (\ s a -> s{_drspParameterKey = a})

instance AWSRequest DeleteRoomSkillParameter where
        type Rs DeleteRoomSkillParameter =
             DeleteRoomSkillParameterResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteRoomSkillParameterResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteRoomSkillParameter where

instance NFData DeleteRoomSkillParameter where

instance ToHeaders DeleteRoomSkillParameter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DeleteRoomSkillParameter" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRoomSkillParameter where
        toJSON DeleteRoomSkillParameter'{..}
          = object
              (catMaybes
                 [("RoomArn" .=) <$> _drspRoomARN,
                  Just ("SkillId" .= _drspSkillId),
                  Just ("ParameterKey" .= _drspParameterKey)])

instance ToPath DeleteRoomSkillParameter where
        toPath = const "/"

instance ToQuery DeleteRoomSkillParameter where
        toQuery = const mempty

-- | /See:/ 'deleteRoomSkillParameterResponse' smart constructor.
newtype DeleteRoomSkillParameterResponse = DeleteRoomSkillParameterResponse'
  { _drsprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoomSkillParameterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsprsResponseStatus' - -- | The response status code.
deleteRoomSkillParameterResponse
    :: Int -- ^ 'drsprsResponseStatus'
    -> DeleteRoomSkillParameterResponse
deleteRoomSkillParameterResponse pResponseStatus_ =
  DeleteRoomSkillParameterResponse' {_drsprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsprsResponseStatus :: Lens' DeleteRoomSkillParameterResponse Int
drsprsResponseStatus = lens _drsprsResponseStatus (\ s a -> s{_drsprsResponseStatus = a})

instance NFData DeleteRoomSkillParameterResponse
         where
