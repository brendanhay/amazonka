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
-- Module      : Network.AWS.AlexaBusiness.GetSkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets skill group details by skill group ARN.
module Network.AWS.AlexaBusiness.GetSkillGroup
  ( -- * Creating a Request
    GetSkillGroup (..),
    newGetSkillGroup,

    -- * Request Lenses
    getSkillGroup_skillGroupArn,

    -- * Destructuring the Response
    GetSkillGroupResponse (..),
    newGetSkillGroupResponse,

    -- * Response Lenses
    getSkillGroupResponse_skillGroup,
    getSkillGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSkillGroup' smart constructor.
data GetSkillGroup = GetSkillGroup'
  { -- | The ARN of the skill group for which to get details. Required.
    skillGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'getSkillGroup_skillGroupArn' - The ARN of the skill group for which to get details. Required.
newGetSkillGroup ::
  GetSkillGroup
newGetSkillGroup =
  GetSkillGroup' {skillGroupArn = Core.Nothing}

-- | The ARN of the skill group for which to get details. Required.
getSkillGroup_skillGroupArn :: Lens.Lens' GetSkillGroup (Core.Maybe Core.Text)
getSkillGroup_skillGroupArn = Lens.lens (\GetSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@GetSkillGroup' {} a -> s {skillGroupArn = a} :: GetSkillGroup)

instance Core.AWSRequest GetSkillGroup where
  type
    AWSResponse GetSkillGroup =
      GetSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSkillGroupResponse'
            Core.<$> (x Core..?> "SkillGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSkillGroup

instance Core.NFData GetSkillGroup

instance Core.ToHeaders GetSkillGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetSkillGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSkillGroup where
  toJSON GetSkillGroup' {..} =
    Core.object
      ( Core.catMaybes
          [("SkillGroupArn" Core..=) Core.<$> skillGroupArn]
      )

instance Core.ToPath GetSkillGroup where
  toPath = Core.const "/"

instance Core.ToQuery GetSkillGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSkillGroupResponse' smart constructor.
data GetSkillGroupResponse = GetSkillGroupResponse'
  { -- | The details of the skill group requested. Required.
    skillGroup :: Core.Maybe SkillGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroup', 'getSkillGroupResponse_skillGroup' - The details of the skill group requested. Required.
--
-- 'httpStatus', 'getSkillGroupResponse_httpStatus' - The response's http status code.
newGetSkillGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSkillGroupResponse
newGetSkillGroupResponse pHttpStatus_ =
  GetSkillGroupResponse'
    { skillGroup = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the skill group requested. Required.
getSkillGroupResponse_skillGroup :: Lens.Lens' GetSkillGroupResponse (Core.Maybe SkillGroup)
getSkillGroupResponse_skillGroup = Lens.lens (\GetSkillGroupResponse' {skillGroup} -> skillGroup) (\s@GetSkillGroupResponse' {} a -> s {skillGroup = a} :: GetSkillGroupResponse)

-- | The response's http status code.
getSkillGroupResponse_httpStatus :: Lens.Lens' GetSkillGroupResponse Core.Int
getSkillGroupResponse_httpStatus = Lens.lens (\GetSkillGroupResponse' {httpStatus} -> httpStatus) (\s@GetSkillGroupResponse' {} a -> s {httpStatus = a} :: GetSkillGroupResponse)

instance Core.NFData GetSkillGroupResponse
