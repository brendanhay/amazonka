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
-- Module      : Amazonka.AlexaBusiness.CreateSkillGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a skill group with a specified name and description.
module Amazonka.AlexaBusiness.CreateSkillGroup
  ( -- * Creating a Request
    CreateSkillGroup (..),
    newCreateSkillGroup,

    -- * Request Lenses
    createSkillGroup_clientRequestToken,
    createSkillGroup_description,
    createSkillGroup_tags,
    createSkillGroup_skillGroupName,

    -- * Destructuring the Response
    CreateSkillGroupResponse (..),
    newCreateSkillGroupResponse,

    -- * Response Lenses
    createSkillGroupResponse_skillGroupArn,
    createSkillGroupResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSkillGroup' smart constructor.
data CreateSkillGroup = CreateSkillGroup'
  { -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The description for the skill group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the skill group.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the skill group.
    skillGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createSkillGroup_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'description', 'createSkillGroup_description' - The description for the skill group.
--
-- 'tags', 'createSkillGroup_tags' - The tags for the skill group.
--
-- 'skillGroupName', 'createSkillGroup_skillGroupName' - The name for the skill group.
newCreateSkillGroup ::
  -- | 'skillGroupName'
  Prelude.Text ->
  CreateSkillGroup
newCreateSkillGroup pSkillGroupName_ =
  CreateSkillGroup'
    { clientRequestToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      skillGroupName = pSkillGroupName_
    }

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createSkillGroup_clientRequestToken :: Lens.Lens' CreateSkillGroup (Prelude.Maybe Prelude.Text)
createSkillGroup_clientRequestToken = Lens.lens (\CreateSkillGroup' {clientRequestToken} -> clientRequestToken) (\s@CreateSkillGroup' {} a -> s {clientRequestToken = a} :: CreateSkillGroup)

-- | The description for the skill group.
createSkillGroup_description :: Lens.Lens' CreateSkillGroup (Prelude.Maybe Prelude.Text)
createSkillGroup_description = Lens.lens (\CreateSkillGroup' {description} -> description) (\s@CreateSkillGroup' {} a -> s {description = a} :: CreateSkillGroup)

-- | The tags for the skill group.
createSkillGroup_tags :: Lens.Lens' CreateSkillGroup (Prelude.Maybe [Tag])
createSkillGroup_tags = Lens.lens (\CreateSkillGroup' {tags} -> tags) (\s@CreateSkillGroup' {} a -> s {tags = a} :: CreateSkillGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name for the skill group.
createSkillGroup_skillGroupName :: Lens.Lens' CreateSkillGroup Prelude.Text
createSkillGroup_skillGroupName = Lens.lens (\CreateSkillGroup' {skillGroupName} -> skillGroupName) (\s@CreateSkillGroup' {} a -> s {skillGroupName = a} :: CreateSkillGroup)

instance Core.AWSRequest CreateSkillGroup where
  type
    AWSResponse CreateSkillGroup =
      CreateSkillGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSkillGroupResponse'
            Prelude.<$> (x Data..?> "SkillGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSkillGroup where
  hashWithSalt _salt CreateSkillGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` skillGroupName

instance Prelude.NFData CreateSkillGroup where
  rnf CreateSkillGroup' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf skillGroupName

instance Data.ToHeaders CreateSkillGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateSkillGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSkillGroup where
  toJSON CreateSkillGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("SkillGroupName" Data..= skillGroupName)
          ]
      )

instance Data.ToPath CreateSkillGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSkillGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSkillGroupResponse' smart constructor.
data CreateSkillGroupResponse = CreateSkillGroupResponse'
  { -- | The ARN of the newly created skill group in the response.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateSkillGroupResponse where
  rnf CreateSkillGroupResponse' {..} =
    Prelude.rnf skillGroupArn `Prelude.seq`
      Prelude.rnf httpStatus
