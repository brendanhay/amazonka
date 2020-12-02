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
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill unavailable for enrolled users and prevents them from enabling it on their devices.
module Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
  ( -- * Creating a Request
    disassociateSkillFromUsers,
    DisassociateSkillFromUsers,

    -- * Request Lenses
    dsfuSkillId,

    -- * Destructuring the Response
    disassociateSkillFromUsersResponse,
    DisassociateSkillFromUsersResponse,

    -- * Response Lenses
    dsfursResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateSkillFromUsers' smart constructor.
newtype DisassociateSkillFromUsers = DisassociateSkillFromUsers'
  { _dsfuSkillId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateSkillFromUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfuSkillId' - The private skill ID you want to make unavailable for enrolled users.
disassociateSkillFromUsers ::
  -- | 'dsfuSkillId'
  Text ->
  DisassociateSkillFromUsers
disassociateSkillFromUsers pSkillId_ =
  DisassociateSkillFromUsers' {_dsfuSkillId = pSkillId_}

-- | The private skill ID you want to make unavailable for enrolled users.
dsfuSkillId :: Lens' DisassociateSkillFromUsers Text
dsfuSkillId = lens _dsfuSkillId (\s a -> s {_dsfuSkillId = a})

instance AWSRequest DisassociateSkillFromUsers where
  type
    Rs DisassociateSkillFromUsers =
      DisassociateSkillFromUsersResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      ( \s h x ->
          DisassociateSkillFromUsersResponse' <$> (pure (fromEnum s))
      )

instance Hashable DisassociateSkillFromUsers

instance NFData DisassociateSkillFromUsers

instance ToHeaders DisassociateSkillFromUsers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.DisassociateSkillFromUsers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisassociateSkillFromUsers where
  toJSON DisassociateSkillFromUsers' {..} =
    object (catMaybes [Just ("SkillId" .= _dsfuSkillId)])

instance ToPath DisassociateSkillFromUsers where
  toPath = const "/"

instance ToQuery DisassociateSkillFromUsers where
  toQuery = const mempty

-- | /See:/ 'disassociateSkillFromUsersResponse' smart constructor.
newtype DisassociateSkillFromUsersResponse = DisassociateSkillFromUsersResponse'
  { _dsfursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateSkillFromUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfursResponseStatus' - -- | The response status code.
disassociateSkillFromUsersResponse ::
  -- | 'dsfursResponseStatus'
  Int ->
  DisassociateSkillFromUsersResponse
disassociateSkillFromUsersResponse pResponseStatus_ =
  DisassociateSkillFromUsersResponse'
    { _dsfursResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dsfursResponseStatus :: Lens' DisassociateSkillFromUsersResponse Int
dsfursResponseStatus = lens _dsfursResponseStatus (\s a -> s {_dsfursResponseStatus = a})

instance NFData DisassociateSkillFromUsersResponse
