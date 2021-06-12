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
-- Module      : Network.AWS.AlexaBusiness.CreateSkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a skill group with a specified name and description.
module Network.AWS.AlexaBusiness.CreateSkillGroup
  ( -- * Creating a Request
    CreateSkillGroup (..),
    newCreateSkillGroup,

    -- * Request Lenses
    createSkillGroup_tags,
    createSkillGroup_description,
    createSkillGroup_clientRequestToken,
    createSkillGroup_skillGroupName,

    -- * Destructuring the Response
    CreateSkillGroupResponse (..),
    newCreateSkillGroupResponse,

    -- * Response Lenses
    createSkillGroupResponse_skillGroupArn,
    createSkillGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSkillGroup' smart constructor.
data CreateSkillGroup = CreateSkillGroup'
  { -- | The tags for the skill group.
    tags :: Core.Maybe [Tag],
    -- | The description for the skill group.
    description :: Core.Maybe Core.Text,
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name for the skill group.
    skillGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSkillGroup_tags' - The tags for the skill group.
--
-- 'description', 'createSkillGroup_description' - The description for the skill group.
--
-- 'clientRequestToken', 'createSkillGroup_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'skillGroupName', 'createSkillGroup_skillGroupName' - The name for the skill group.
newCreateSkillGroup ::
  -- | 'skillGroupName'
  Core.Text ->
  CreateSkillGroup
newCreateSkillGroup pSkillGroupName_ =
  CreateSkillGroup'
    { tags = Core.Nothing,
      description = Core.Nothing,
      clientRequestToken = Core.Nothing,
      skillGroupName = pSkillGroupName_
    }

-- | The tags for the skill group.
createSkillGroup_tags :: Lens.Lens' CreateSkillGroup (Core.Maybe [Tag])
createSkillGroup_tags = Lens.lens (\CreateSkillGroup' {tags} -> tags) (\s@CreateSkillGroup' {} a -> s {tags = a} :: CreateSkillGroup) Core.. Lens.mapping Lens._Coerce

-- | The description for the skill group.
createSkillGroup_description :: Lens.Lens' CreateSkillGroup (Core.Maybe Core.Text)
createSkillGroup_description = Lens.lens (\CreateSkillGroup' {description} -> description) (\s@CreateSkillGroup' {} a -> s {description = a} :: CreateSkillGroup)

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createSkillGroup_clientRequestToken :: Lens.Lens' CreateSkillGroup (Core.Maybe Core.Text)
createSkillGroup_clientRequestToken = Lens.lens (\CreateSkillGroup' {clientRequestToken} -> clientRequestToken) (\s@CreateSkillGroup' {} a -> s {clientRequestToken = a} :: CreateSkillGroup)

-- | The name for the skill group.
createSkillGroup_skillGroupName :: Lens.Lens' CreateSkillGroup Core.Text
createSkillGroup_skillGroupName = Lens.lens (\CreateSkillGroup' {skillGroupName} -> skillGroupName) (\s@CreateSkillGroup' {} a -> s {skillGroupName = a} :: CreateSkillGroup)

instance Core.AWSRequest CreateSkillGroup where
  type
    AWSResponse CreateSkillGroup =
      CreateSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSkillGroupResponse'
            Core.<$> (x Core..?> "SkillGroupArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSkillGroup

instance Core.NFData CreateSkillGroup

instance Core.ToHeaders CreateSkillGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateSkillGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateSkillGroup where
  toJSON CreateSkillGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("SkillGroupName" Core..= skillGroupName)
          ]
      )

instance Core.ToPath CreateSkillGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateSkillGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSkillGroupResponse' smart constructor.
data CreateSkillGroupResponse = CreateSkillGroupResponse'
  { -- | The ARN of the newly created skill group in the response.
    skillGroupArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'createSkillGroupResponse_skillGroupArn' - The ARN of the newly created skill group in the response.
--
-- 'httpStatus', 'createSkillGroupResponse_httpStatus' - The response's http status code.
newCreateSkillGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSkillGroupResponse
newCreateSkillGroupResponse pHttpStatus_ =
  CreateSkillGroupResponse'
    { skillGroupArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created skill group in the response.
createSkillGroupResponse_skillGroupArn :: Lens.Lens' CreateSkillGroupResponse (Core.Maybe Core.Text)
createSkillGroupResponse_skillGroupArn = Lens.lens (\CreateSkillGroupResponse' {skillGroupArn} -> skillGroupArn) (\s@CreateSkillGroupResponse' {} a -> s {skillGroupArn = a} :: CreateSkillGroupResponse)

-- | The response's http status code.
createSkillGroupResponse_httpStatus :: Lens.Lens' CreateSkillGroupResponse Core.Int
createSkillGroupResponse_httpStatus = Lens.lens (\CreateSkillGroupResponse' {httpStatus} -> httpStatus) (\s@CreateSkillGroupResponse' {} a -> s {httpStatus = a} :: CreateSkillGroupResponse)

instance Core.NFData CreateSkillGroupResponse
