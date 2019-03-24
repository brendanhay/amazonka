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
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a third-party account from a skill.
--
--
module Network.AWS.AlexaBusiness.DeleteSkillAuthorization
    (
    -- * Creating a Request
      deleteSkillAuthorization
    , DeleteSkillAuthorization
    -- * Request Lenses
    , dsaRoomARN
    , dsaSkillId

    -- * Destructuring the Response
    , deleteSkillAuthorizationResponse
    , DeleteSkillAuthorizationResponse
    -- * Response Lenses
    , dsarsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSkillAuthorization' smart constructor.
data DeleteSkillAuthorization = DeleteSkillAuthorization'
  { _dsaRoomARN :: !(Maybe Text)
  , _dsaSkillId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSkillAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaRoomARN' - The room that the skill is authorized for.
--
-- * 'dsaSkillId' - The unique identifier of a skill.
deleteSkillAuthorization
    :: Text -- ^ 'dsaSkillId'
    -> DeleteSkillAuthorization
deleteSkillAuthorization pSkillId_ =
  DeleteSkillAuthorization' {_dsaRoomARN = Nothing, _dsaSkillId = pSkillId_}


-- | The room that the skill is authorized for.
dsaRoomARN :: Lens' DeleteSkillAuthorization (Maybe Text)
dsaRoomARN = lens _dsaRoomARN (\ s a -> s{_dsaRoomARN = a})

-- | The unique identifier of a skill.
dsaSkillId :: Lens' DeleteSkillAuthorization Text
dsaSkillId = lens _dsaSkillId (\ s a -> s{_dsaSkillId = a})

instance AWSRequest DeleteSkillAuthorization where
        type Rs DeleteSkillAuthorization =
             DeleteSkillAuthorizationResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteSkillAuthorizationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteSkillAuthorization where

instance NFData DeleteSkillAuthorization where

instance ToHeaders DeleteSkillAuthorization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DeleteSkillAuthorization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSkillAuthorization where
        toJSON DeleteSkillAuthorization'{..}
          = object
              (catMaybes
                 [("RoomArn" .=) <$> _dsaRoomARN,
                  Just ("SkillId" .= _dsaSkillId)])

instance ToPath DeleteSkillAuthorization where
        toPath = const "/"

instance ToQuery DeleteSkillAuthorization where
        toQuery = const mempty

-- | /See:/ 'deleteSkillAuthorizationResponse' smart constructor.
newtype DeleteSkillAuthorizationResponse = DeleteSkillAuthorizationResponse'
  { _dsarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSkillAuthorizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsResponseStatus' - -- | The response status code.
deleteSkillAuthorizationResponse
    :: Int -- ^ 'dsarsResponseStatus'
    -> DeleteSkillAuthorizationResponse
deleteSkillAuthorizationResponse pResponseStatus_ =
  DeleteSkillAuthorizationResponse' {_dsarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsarsResponseStatus :: Lens' DeleteSkillAuthorizationResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\ s a -> s{_dsarsResponseStatus = a})

instance NFData DeleteSkillAuthorizationResponse
         where
