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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSkillGroup' smart constructor.
data CreateSkillGroup = CreateSkillGroup'
  { -- | The tags for the skill group.
    tags :: Prelude.Maybe [Tag],
    -- | The description for the skill group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name for the skill group.
    skillGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateSkillGroup
newCreateSkillGroup pSkillGroupName_ =
  CreateSkillGroup'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      skillGroupName = pSkillGroupName_
    }

-- | The tags for the skill group.
createSkillGroup_tags :: Lens.Lens' CreateSkillGroup (Prelude.Maybe [Tag])
createSkillGroup_tags = Lens.lens (\CreateSkillGroup' {tags} -> tags) (\s@CreateSkillGroup' {} a -> s {tags = a} :: CreateSkillGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The description for the skill group.
createSkillGroup_description :: Lens.Lens' CreateSkillGroup (Prelude.Maybe Prelude.Text)
createSkillGroup_description = Lens.lens (\CreateSkillGroup' {description} -> description) (\s@CreateSkillGroup' {} a -> s {description = a} :: CreateSkillGroup)

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createSkillGroup_clientRequestToken :: Lens.Lens' CreateSkillGroup (Prelude.Maybe Prelude.Text)
createSkillGroup_clientRequestToken = Lens.lens (\CreateSkillGroup' {clientRequestToken} -> clientRequestToken) (\s@CreateSkillGroup' {} a -> s {clientRequestToken = a} :: CreateSkillGroup)

-- | The name for the skill group.
createSkillGroup_skillGroupName :: Lens.Lens' CreateSkillGroup Prelude.Text
createSkillGroup_skillGroupName = Lens.lens (\CreateSkillGroup' {skillGroupName} -> skillGroupName) (\s@CreateSkillGroup' {} a -> s {skillGroupName = a} :: CreateSkillGroup)

instance Prelude.AWSRequest CreateSkillGroup where
  type Rs CreateSkillGroup = CreateSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSkillGroupResponse'
            Prelude.<$> (x Prelude..?> "SkillGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSkillGroup

instance Prelude.NFData CreateSkillGroup

instance Prelude.ToHeaders CreateSkillGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.CreateSkillGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateSkillGroup where
  toJSON CreateSkillGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            ("Description" Prelude..=) Prelude.<$> description,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just
              ("SkillGroupName" Prelude..= skillGroupName)
          ]
      )

instance Prelude.ToPath CreateSkillGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSkillGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSkillGroupResponse' smart constructor.
data CreateSkillGroupResponse = CreateSkillGroupResponse'
  { -- | The ARN of the newly created skill group in the response.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateSkillGroupResponse
newCreateSkillGroupResponse pHttpStatus_ =
  CreateSkillGroupResponse'
    { skillGroupArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created skill group in the response.
createSkillGroupResponse_skillGroupArn :: Lens.Lens' CreateSkillGroupResponse (Prelude.Maybe Prelude.Text)
createSkillGroupResponse_skillGroupArn = Lens.lens (\CreateSkillGroupResponse' {skillGroupArn} -> skillGroupArn) (\s@CreateSkillGroupResponse' {} a -> s {skillGroupArn = a} :: CreateSkillGroupResponse)

-- | The response's http status code.
createSkillGroupResponse_httpStatus :: Lens.Lens' CreateSkillGroupResponse Prelude.Int
createSkillGroupResponse_httpStatus = Lens.lens (\CreateSkillGroupResponse' {httpStatus} -> httpStatus) (\s@CreateSkillGroupResponse' {} a -> s {httpStatus = a} :: CreateSkillGroupResponse)

instance Prelude.NFData CreateSkillGroupResponse
