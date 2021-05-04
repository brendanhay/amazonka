{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.RejectSkill
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from the organization under a user\'s AWS account.
-- If the skill is a private skill, it moves to an AcceptStatus of PENDING.
-- Any private or public skill that is rejected can be added later by
-- calling the ApproveSkill API.
module Network.AWS.AlexaBusiness.RejectSkill
  ( -- * Creating a Request
    RejectSkill (..),
    newRejectSkill,

    -- * Request Lenses
    rejectSkill_skillId,

    -- * Destructuring the Response
    RejectSkillResponse (..),
    newRejectSkillResponse,

    -- * Response Lenses
    rejectSkillResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectSkill' smart constructor.
data RejectSkill = RejectSkill'
  { -- | The unique identifier of the skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectSkill' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillId', 'rejectSkill_skillId' - The unique identifier of the skill.
newRejectSkill ::
  -- | 'skillId'
  Prelude.Text ->
  RejectSkill
newRejectSkill pSkillId_ =
  RejectSkill' {skillId = pSkillId_}

-- | The unique identifier of the skill.
rejectSkill_skillId :: Lens.Lens' RejectSkill Prelude.Text
rejectSkill_skillId = Lens.lens (\RejectSkill' {skillId} -> skillId) (\s@RejectSkill' {} a -> s {skillId = a} :: RejectSkill)

instance Prelude.AWSRequest RejectSkill where
  type Rs RejectSkill = RejectSkillResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectSkillResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectSkill

instance Prelude.NFData RejectSkill

instance Prelude.ToHeaders RejectSkill where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.RejectSkill" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RejectSkill where
  toJSON RejectSkill' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SkillId" Prelude..= skillId)]
      )

instance Prelude.ToPath RejectSkill where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RejectSkill where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectSkillResponse' smart constructor.
data RejectSkillResponse = RejectSkillResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectSkillResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectSkillResponse_httpStatus' - The response's http status code.
newRejectSkillResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectSkillResponse
newRejectSkillResponse pHttpStatus_ =
  RejectSkillResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
rejectSkillResponse_httpStatus :: Lens.Lens' RejectSkillResponse Prelude.Int
rejectSkillResponse_httpStatus = Lens.lens (\RejectSkillResponse' {httpStatus} -> httpStatus) (\s@RejectSkillResponse' {} a -> s {httpStatus = a} :: RejectSkillResponse)

instance Prelude.NFData RejectSkillResponse
