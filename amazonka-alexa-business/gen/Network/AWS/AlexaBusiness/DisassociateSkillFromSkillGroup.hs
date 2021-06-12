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
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from a skill group.
module Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
  ( -- * Creating a Request
    DisassociateSkillFromSkillGroup (..),
    newDisassociateSkillFromSkillGroup,

    -- * Request Lenses
    disassociateSkillFromSkillGroup_skillGroupArn,
    disassociateSkillFromSkillGroup_skillId,

    -- * Destructuring the Response
    DisassociateSkillFromSkillGroupResponse (..),
    newDisassociateSkillFromSkillGroupResponse,

    -- * Response Lenses
    disassociateSkillFromSkillGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateSkillFromSkillGroup' smart constructor.
data DisassociateSkillFromSkillGroup = DisassociateSkillFromSkillGroup'
  { -- | The unique identifier of a skill. Required.
    skillGroupArn :: Core.Maybe Core.Text,
    -- | The ARN of a skill group to associate to a skill.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateSkillFromSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'disassociateSkillFromSkillGroup_skillGroupArn' - The unique identifier of a skill. Required.
--
-- 'skillId', 'disassociateSkillFromSkillGroup_skillId' - The ARN of a skill group to associate to a skill.
newDisassociateSkillFromSkillGroup ::
  -- | 'skillId'
  Core.Text ->
  DisassociateSkillFromSkillGroup
newDisassociateSkillFromSkillGroup pSkillId_ =
  DisassociateSkillFromSkillGroup'
    { skillGroupArn =
        Core.Nothing,
      skillId = pSkillId_
    }

-- | The unique identifier of a skill. Required.
disassociateSkillFromSkillGroup_skillGroupArn :: Lens.Lens' DisassociateSkillFromSkillGroup (Core.Maybe Core.Text)
disassociateSkillFromSkillGroup_skillGroupArn = Lens.lens (\DisassociateSkillFromSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@DisassociateSkillFromSkillGroup' {} a -> s {skillGroupArn = a} :: DisassociateSkillFromSkillGroup)

-- | The ARN of a skill group to associate to a skill.
disassociateSkillFromSkillGroup_skillId :: Lens.Lens' DisassociateSkillFromSkillGroup Core.Text
disassociateSkillFromSkillGroup_skillId = Lens.lens (\DisassociateSkillFromSkillGroup' {skillId} -> skillId) (\s@DisassociateSkillFromSkillGroup' {} a -> s {skillId = a} :: DisassociateSkillFromSkillGroup)

instance
  Core.AWSRequest
    DisassociateSkillFromSkillGroup
  where
  type
    AWSResponse DisassociateSkillFromSkillGroup =
      DisassociateSkillFromSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromSkillGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateSkillFromSkillGroup

instance Core.NFData DisassociateSkillFromSkillGroup

instance
  Core.ToHeaders
    DisassociateSkillFromSkillGroup
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DisassociateSkillFromSkillGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateSkillFromSkillGroup where
  toJSON DisassociateSkillFromSkillGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SkillGroupArn" Core..=) Core.<$> skillGroupArn,
            Core.Just ("SkillId" Core..= skillId)
          ]
      )

instance Core.ToPath DisassociateSkillFromSkillGroup where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateSkillFromSkillGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateSkillFromSkillGroupResponse' smart constructor.
data DisassociateSkillFromSkillGroupResponse = DisassociateSkillFromSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateSkillFromSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateSkillFromSkillGroupResponse_httpStatus' - The response's http status code.
newDisassociateSkillFromSkillGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateSkillFromSkillGroupResponse
newDisassociateSkillFromSkillGroupResponse
  pHttpStatus_ =
    DisassociateSkillFromSkillGroupResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateSkillFromSkillGroupResponse_httpStatus :: Lens.Lens' DisassociateSkillFromSkillGroupResponse Core.Int
disassociateSkillFromSkillGroupResponse_httpStatus = Lens.lens (\DisassociateSkillFromSkillGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateSkillFromSkillGroupResponse' {} a -> s {httpStatus = a} :: DisassociateSkillFromSkillGroupResponse)

instance
  Core.NFData
    DisassociateSkillFromSkillGroupResponse
