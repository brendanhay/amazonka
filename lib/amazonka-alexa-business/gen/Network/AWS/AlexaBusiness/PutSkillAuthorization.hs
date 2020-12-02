{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutSkillAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links a user's account to a third-party skill provider. If this API operation is called by an assumed IAM role, the skill being linked must be a private skill. Also, the skill must be owned by the AWS account that assumed the IAM role.
module Network.AWS.AlexaBusiness.PutSkillAuthorization
  ( -- * Creating a Request
    putSkillAuthorization,
    PutSkillAuthorization,

    -- * Request Lenses
    psaRoomARN,
    psaAuthorizationResult,
    psaSkillId,

    -- * Destructuring the Response
    putSkillAuthorizationResponse,
    PutSkillAuthorizationResponse,

    -- * Response Lenses
    psarsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSkillAuthorization' smart constructor.
data PutSkillAuthorization = PutSkillAuthorization'
  { _psaRoomARN ::
      !(Maybe Text),
    _psaAuthorizationResult ::
      !(Sensitive (Map Text (Text))),
    _psaSkillId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutSkillAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psaRoomARN' - The room that the skill is authorized for.
--
-- * 'psaAuthorizationResult' - The authorization result specific to OAUTH code grant output. "Code” must be populated in the AuthorizationResult map to establish the authorization.
--
-- * 'psaSkillId' - The unique identifier of a skill.
putSkillAuthorization ::
  -- | 'psaSkillId'
  Text ->
  PutSkillAuthorization
putSkillAuthorization pSkillId_ =
  PutSkillAuthorization'
    { _psaRoomARN = Nothing,
      _psaAuthorizationResult = mempty,
      _psaSkillId = pSkillId_
    }

-- | The room that the skill is authorized for.
psaRoomARN :: Lens' PutSkillAuthorization (Maybe Text)
psaRoomARN = lens _psaRoomARN (\s a -> s {_psaRoomARN = a})

-- | The authorization result specific to OAUTH code grant output. "Code” must be populated in the AuthorizationResult map to establish the authorization.
psaAuthorizationResult :: Lens' PutSkillAuthorization (HashMap Text (Text))
psaAuthorizationResult = lens _psaAuthorizationResult (\s a -> s {_psaAuthorizationResult = a}) . _Sensitive . _Map

-- | The unique identifier of a skill.
psaSkillId :: Lens' PutSkillAuthorization Text
psaSkillId = lens _psaSkillId (\s a -> s {_psaSkillId = a})

instance AWSRequest PutSkillAuthorization where
  type Rs PutSkillAuthorization = PutSkillAuthorizationResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> PutSkillAuthorizationResponse' <$> (pure (fromEnum s)))

instance Hashable PutSkillAuthorization

instance NFData PutSkillAuthorization

instance ToHeaders PutSkillAuthorization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.PutSkillAuthorization" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutSkillAuthorization where
  toJSON PutSkillAuthorization' {..} =
    object
      ( catMaybes
          [ ("RoomArn" .=) <$> _psaRoomARN,
            Just ("AuthorizationResult" .= _psaAuthorizationResult),
            Just ("SkillId" .= _psaSkillId)
          ]
      )

instance ToPath PutSkillAuthorization where
  toPath = const "/"

instance ToQuery PutSkillAuthorization where
  toQuery = const mempty

-- | /See:/ 'putSkillAuthorizationResponse' smart constructor.
newtype PutSkillAuthorizationResponse = PutSkillAuthorizationResponse'
  { _psarsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutSkillAuthorizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psarsResponseStatus' - -- | The response status code.
putSkillAuthorizationResponse ::
  -- | 'psarsResponseStatus'
  Int ->
  PutSkillAuthorizationResponse
putSkillAuthorizationResponse pResponseStatus_ =
  PutSkillAuthorizationResponse'
    { _psarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
psarsResponseStatus :: Lens' PutSkillAuthorizationResponse Int
psarsResponseStatus = lens _psarsResponseStatus (\s a -> s {_psarsResponseStatus = a})

instance NFData PutSkillAuthorizationResponse
