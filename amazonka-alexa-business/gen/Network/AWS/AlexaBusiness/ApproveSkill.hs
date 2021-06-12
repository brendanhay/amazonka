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
-- Module      : Network.AWS.AlexaBusiness.ApproveSkill
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with the organization under the customer\'s AWS
-- account. If a skill is private, the user implicitly accepts access to
-- this skill during enablement.
module Network.AWS.AlexaBusiness.ApproveSkill
  ( -- * Creating a Request
    ApproveSkill (..),
    newApproveSkill,

    -- * Request Lenses
    approveSkill_skillId,

    -- * Destructuring the Response
    ApproveSkillResponse (..),
    newApproveSkillResponse,

    -- * Response Lenses
    approveSkillResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newApproveSkill' smart constructor.
data ApproveSkill = ApproveSkill'
  { -- | The unique identifier of the skill.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApproveSkill' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillId', 'approveSkill_skillId' - The unique identifier of the skill.
newApproveSkill ::
  -- | 'skillId'
  Core.Text ->
  ApproveSkill
newApproveSkill pSkillId_ =
  ApproveSkill' {skillId = pSkillId_}

-- | The unique identifier of the skill.
approveSkill_skillId :: Lens.Lens' ApproveSkill Core.Text
approveSkill_skillId = Lens.lens (\ApproveSkill' {skillId} -> skillId) (\s@ApproveSkill' {} a -> s {skillId = a} :: ApproveSkill)

instance Core.AWSRequest ApproveSkill where
  type AWSResponse ApproveSkill = ApproveSkillResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ApproveSkillResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ApproveSkill

instance Core.NFData ApproveSkill

instance Core.ToHeaders ApproveSkill where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.ApproveSkill" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ApproveSkill where
  toJSON ApproveSkill' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SkillId" Core..= skillId)]
      )

instance Core.ToPath ApproveSkill where
  toPath = Core.const "/"

instance Core.ToQuery ApproveSkill where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newApproveSkillResponse' smart constructor.
data ApproveSkillResponse = ApproveSkillResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApproveSkillResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'approveSkillResponse_httpStatus' - The response's http status code.
newApproveSkillResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ApproveSkillResponse
newApproveSkillResponse pHttpStatus_ =
  ApproveSkillResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
approveSkillResponse_httpStatus :: Lens.Lens' ApproveSkillResponse Core.Int
approveSkillResponse_httpStatus = Lens.lens (\ApproveSkillResponse' {httpStatus} -> httpStatus) (\s@ApproveSkillResponse' {} a -> s {httpStatus = a} :: ApproveSkillResponse)

instance Core.NFData ApproveSkillResponse
