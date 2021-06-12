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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectSkill' smart constructor.
data RejectSkill = RejectSkill'
  { -- | The unique identifier of the skill.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RejectSkill
newRejectSkill pSkillId_ =
  RejectSkill' {skillId = pSkillId_}

-- | The unique identifier of the skill.
rejectSkill_skillId :: Lens.Lens' RejectSkill Core.Text
rejectSkill_skillId = Lens.lens (\RejectSkill' {skillId} -> skillId) (\s@RejectSkill' {} a -> s {skillId = a} :: RejectSkill)

instance Core.AWSRequest RejectSkill where
  type AWSResponse RejectSkill = RejectSkillResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectSkillResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RejectSkill

instance Core.NFData RejectSkill

instance Core.ToHeaders RejectSkill where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.RejectSkill" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RejectSkill where
  toJSON RejectSkill' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SkillId" Core..= skillId)]
      )

instance Core.ToPath RejectSkill where
  toPath = Core.const "/"

instance Core.ToQuery RejectSkill where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRejectSkillResponse' smart constructor.
data RejectSkillResponse = RejectSkillResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RejectSkillResponse
newRejectSkillResponse pHttpStatus_ =
  RejectSkillResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
rejectSkillResponse_httpStatus :: Lens.Lens' RejectSkillResponse Core.Int
rejectSkillResponse_httpStatus = Lens.lens (\RejectSkillResponse' {httpStatus} -> httpStatus) (\s@RejectSkillResponse' {} a -> s {httpStatus = a} :: RejectSkillResponse)

instance Core.NFData RejectSkillResponse
