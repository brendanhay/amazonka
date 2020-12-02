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
-- Module      : Network.AWS.AlexaBusiness.ApproveSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with the organization under the customer's AWS account. If a skill is private, the user implicitly accepts access to this skill during enablement.
module Network.AWS.AlexaBusiness.ApproveSkill
  ( -- * Creating a Request
    approveSkill,
    ApproveSkill,

    -- * Request Lenses
    asSkillId,

    -- * Destructuring the Response
    approveSkillResponse,
    ApproveSkillResponse,

    -- * Response Lenses
    asrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'approveSkill' smart constructor.
newtype ApproveSkill = ApproveSkill' {_asSkillId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApproveSkill' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asSkillId' - The unique identifier of the skill.
approveSkill ::
  -- | 'asSkillId'
  Text ->
  ApproveSkill
approveSkill pSkillId_ = ApproveSkill' {_asSkillId = pSkillId_}

-- | The unique identifier of the skill.
asSkillId :: Lens' ApproveSkill Text
asSkillId = lens _asSkillId (\s a -> s {_asSkillId = a})

instance AWSRequest ApproveSkill where
  type Rs ApproveSkill = ApproveSkillResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> ApproveSkillResponse' <$> (pure (fromEnum s)))

instance Hashable ApproveSkill

instance NFData ApproveSkill

instance ToHeaders ApproveSkill where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AlexaForBusiness.ApproveSkill" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ApproveSkill where
  toJSON ApproveSkill' {..} =
    object (catMaybes [Just ("SkillId" .= _asSkillId)])

instance ToPath ApproveSkill where
  toPath = const "/"

instance ToQuery ApproveSkill where
  toQuery = const mempty

-- | /See:/ 'approveSkillResponse' smart constructor.
newtype ApproveSkillResponse = ApproveSkillResponse'
  { _asrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApproveSkillResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asrsResponseStatus' - -- | The response status code.
approveSkillResponse ::
  -- | 'asrsResponseStatus'
  Int ->
  ApproveSkillResponse
approveSkillResponse pResponseStatus_ =
  ApproveSkillResponse' {_asrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
asrsResponseStatus :: Lens' ApproveSkillResponse Int
asrsResponseStatus = lens _asrsResponseStatus (\s a -> s {_asrsResponseStatus = a})

instance NFData ApproveSkillResponse
