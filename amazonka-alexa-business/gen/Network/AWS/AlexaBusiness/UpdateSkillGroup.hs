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
-- Module      : Network.AWS.AlexaBusiness.UpdateSkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates skill group details by skill group ARN.
module Network.AWS.AlexaBusiness.UpdateSkillGroup
  ( -- * Creating a Request
    UpdateSkillGroup (..),
    newUpdateSkillGroup,

    -- * Request Lenses
    updateSkillGroup_skillGroupName,
    updateSkillGroup_description,
    updateSkillGroup_skillGroupArn,

    -- * Destructuring the Response
    UpdateSkillGroupResponse (..),
    newUpdateSkillGroupResponse,

    -- * Response Lenses
    updateSkillGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSkillGroup' smart constructor.
data UpdateSkillGroup = UpdateSkillGroup'
  { -- | The updated name for the skill group.
    skillGroupName :: Core.Maybe Core.Text,
    -- | The updated description for the skill group.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the skill group to update.
    skillGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupName', 'updateSkillGroup_skillGroupName' - The updated name for the skill group.
--
-- 'description', 'updateSkillGroup_description' - The updated description for the skill group.
--
-- 'skillGroupArn', 'updateSkillGroup_skillGroupArn' - The ARN of the skill group to update.
newUpdateSkillGroup ::
  UpdateSkillGroup
newUpdateSkillGroup =
  UpdateSkillGroup'
    { skillGroupName = Core.Nothing,
      description = Core.Nothing,
      skillGroupArn = Core.Nothing
    }

-- | The updated name for the skill group.
updateSkillGroup_skillGroupName :: Lens.Lens' UpdateSkillGroup (Core.Maybe Core.Text)
updateSkillGroup_skillGroupName = Lens.lens (\UpdateSkillGroup' {skillGroupName} -> skillGroupName) (\s@UpdateSkillGroup' {} a -> s {skillGroupName = a} :: UpdateSkillGroup)

-- | The updated description for the skill group.
updateSkillGroup_description :: Lens.Lens' UpdateSkillGroup (Core.Maybe Core.Text)
updateSkillGroup_description = Lens.lens (\UpdateSkillGroup' {description} -> description) (\s@UpdateSkillGroup' {} a -> s {description = a} :: UpdateSkillGroup)

-- | The ARN of the skill group to update.
updateSkillGroup_skillGroupArn :: Lens.Lens' UpdateSkillGroup (Core.Maybe Core.Text)
updateSkillGroup_skillGroupArn = Lens.lens (\UpdateSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@UpdateSkillGroup' {} a -> s {skillGroupArn = a} :: UpdateSkillGroup)

instance Core.AWSRequest UpdateSkillGroup where
  type
    AWSResponse UpdateSkillGroup =
      UpdateSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSkillGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSkillGroup

instance Core.NFData UpdateSkillGroup

instance Core.ToHeaders UpdateSkillGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateSkillGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSkillGroup where
  toJSON UpdateSkillGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SkillGroupName" Core..=) Core.<$> skillGroupName,
            ("Description" Core..=) Core.<$> description,
            ("SkillGroupArn" Core..=) Core.<$> skillGroupArn
          ]
      )

instance Core.ToPath UpdateSkillGroup where
  toPath = Core.const "/"

instance Core.ToQuery UpdateSkillGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSkillGroupResponse' smart constructor.
data UpdateSkillGroupResponse = UpdateSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSkillGroupResponse_httpStatus' - The response's http status code.
newUpdateSkillGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateSkillGroupResponse
newUpdateSkillGroupResponse pHttpStatus_ =
  UpdateSkillGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSkillGroupResponse_httpStatus :: Lens.Lens' UpdateSkillGroupResponse Core.Int
updateSkillGroupResponse_httpStatus = Lens.lens (\UpdateSkillGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateSkillGroupResponse' {} a -> s {httpStatus = a} :: UpdateSkillGroupResponse)

instance Core.NFData UpdateSkillGroupResponse
