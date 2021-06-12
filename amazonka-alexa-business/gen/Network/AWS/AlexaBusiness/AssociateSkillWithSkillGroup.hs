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
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with a skill group.
module Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
  ( -- * Creating a Request
    AssociateSkillWithSkillGroup (..),
    newAssociateSkillWithSkillGroup,

    -- * Request Lenses
    associateSkillWithSkillGroup_skillGroupArn,
    associateSkillWithSkillGroup_skillId,

    -- * Destructuring the Response
    AssociateSkillWithSkillGroupResponse (..),
    newAssociateSkillWithSkillGroupResponse,

    -- * Response Lenses
    associateSkillWithSkillGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSkillWithSkillGroup' smart constructor.
data AssociateSkillWithSkillGroup = AssociateSkillWithSkillGroup'
  { -- | The ARN of the skill group to associate the skill to. Required.
    skillGroupArn :: Core.Maybe Core.Text,
    -- | The unique identifier of the skill.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSkillWithSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'associateSkillWithSkillGroup_skillGroupArn' - The ARN of the skill group to associate the skill to. Required.
--
-- 'skillId', 'associateSkillWithSkillGroup_skillId' - The unique identifier of the skill.
newAssociateSkillWithSkillGroup ::
  -- | 'skillId'
  Core.Text ->
  AssociateSkillWithSkillGroup
newAssociateSkillWithSkillGroup pSkillId_ =
  AssociateSkillWithSkillGroup'
    { skillGroupArn =
        Core.Nothing,
      skillId = pSkillId_
    }

-- | The ARN of the skill group to associate the skill to. Required.
associateSkillWithSkillGroup_skillGroupArn :: Lens.Lens' AssociateSkillWithSkillGroup (Core.Maybe Core.Text)
associateSkillWithSkillGroup_skillGroupArn = Lens.lens (\AssociateSkillWithSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@AssociateSkillWithSkillGroup' {} a -> s {skillGroupArn = a} :: AssociateSkillWithSkillGroup)

-- | The unique identifier of the skill.
associateSkillWithSkillGroup_skillId :: Lens.Lens' AssociateSkillWithSkillGroup Core.Text
associateSkillWithSkillGroup_skillId = Lens.lens (\AssociateSkillWithSkillGroup' {skillId} -> skillId) (\s@AssociateSkillWithSkillGroup' {} a -> s {skillId = a} :: AssociateSkillWithSkillGroup)

instance Core.AWSRequest AssociateSkillWithSkillGroup where
  type
    AWSResponse AssociateSkillWithSkillGroup =
      AssociateSkillWithSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillWithSkillGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateSkillWithSkillGroup

instance Core.NFData AssociateSkillWithSkillGroup

instance Core.ToHeaders AssociateSkillWithSkillGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.AssociateSkillWithSkillGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateSkillWithSkillGroup where
  toJSON AssociateSkillWithSkillGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SkillGroupArn" Core..=) Core.<$> skillGroupArn,
            Core.Just ("SkillId" Core..= skillId)
          ]
      )

instance Core.ToPath AssociateSkillWithSkillGroup where
  toPath = Core.const "/"

instance Core.ToQuery AssociateSkillWithSkillGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateSkillWithSkillGroupResponse' smart constructor.
data AssociateSkillWithSkillGroupResponse = AssociateSkillWithSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSkillWithSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateSkillWithSkillGroupResponse_httpStatus' - The response's http status code.
newAssociateSkillWithSkillGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateSkillWithSkillGroupResponse
newAssociateSkillWithSkillGroupResponse pHttpStatus_ =
  AssociateSkillWithSkillGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateSkillWithSkillGroupResponse_httpStatus :: Lens.Lens' AssociateSkillWithSkillGroupResponse Core.Int
associateSkillWithSkillGroupResponse_httpStatus = Lens.lens (\AssociateSkillWithSkillGroupResponse' {httpStatus} -> httpStatus) (\s@AssociateSkillWithSkillGroupResponse' {} a -> s {httpStatus = a} :: AssociateSkillWithSkillGroupResponse)

instance
  Core.NFData
    AssociateSkillWithSkillGroupResponse
