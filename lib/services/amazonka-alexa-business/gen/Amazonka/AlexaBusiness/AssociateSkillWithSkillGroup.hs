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
-- Module      : Amazonka.AlexaBusiness.AssociateSkillWithSkillGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with a skill group.
module Amazonka.AlexaBusiness.AssociateSkillWithSkillGroup
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSkillWithSkillGroup' smart constructor.
data AssociateSkillWithSkillGroup = AssociateSkillWithSkillGroup'
  { -- | The ARN of the skill group to associate the skill to. Required.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  AssociateSkillWithSkillGroup
newAssociateSkillWithSkillGroup pSkillId_ =
  AssociateSkillWithSkillGroup'
    { skillGroupArn =
        Prelude.Nothing,
      skillId = pSkillId_
    }

-- | The ARN of the skill group to associate the skill to. Required.
associateSkillWithSkillGroup_skillGroupArn :: Lens.Lens' AssociateSkillWithSkillGroup (Prelude.Maybe Prelude.Text)
associateSkillWithSkillGroup_skillGroupArn = Lens.lens (\AssociateSkillWithSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@AssociateSkillWithSkillGroup' {} a -> s {skillGroupArn = a} :: AssociateSkillWithSkillGroup)

-- | The unique identifier of the skill.
associateSkillWithSkillGroup_skillId :: Lens.Lens' AssociateSkillWithSkillGroup Prelude.Text
associateSkillWithSkillGroup_skillId = Lens.lens (\AssociateSkillWithSkillGroup' {skillId} -> skillId) (\s@AssociateSkillWithSkillGroup' {} a -> s {skillId = a} :: AssociateSkillWithSkillGroup)

instance Core.AWSRequest AssociateSkillWithSkillGroup where
  type
    AWSResponse AssociateSkillWithSkillGroup =
      AssociateSkillWithSkillGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillWithSkillGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateSkillWithSkillGroup
  where
  hashWithSalt _salt AssociateSkillWithSkillGroup' {..} =
    _salt `Prelude.hashWithSalt` skillGroupArn
      `Prelude.hashWithSalt` skillId

instance Prelude.NFData AssociateSkillWithSkillGroup where
  rnf AssociateSkillWithSkillGroup' {..} =
    Prelude.rnf skillGroupArn
      `Prelude.seq` Prelude.rnf skillId

instance Data.ToHeaders AssociateSkillWithSkillGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.AssociateSkillWithSkillGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateSkillWithSkillGroup where
  toJSON AssociateSkillWithSkillGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SkillGroupArn" Data..=) Prelude.<$> skillGroupArn,
            Prelude.Just ("SkillId" Data..= skillId)
          ]
      )

instance Data.ToPath AssociateSkillWithSkillGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateSkillWithSkillGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSkillWithSkillGroupResponse' smart constructor.
data AssociateSkillWithSkillGroupResponse = AssociateSkillWithSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateSkillWithSkillGroupResponse
newAssociateSkillWithSkillGroupResponse pHttpStatus_ =
  AssociateSkillWithSkillGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateSkillWithSkillGroupResponse_httpStatus :: Lens.Lens' AssociateSkillWithSkillGroupResponse Prelude.Int
associateSkillWithSkillGroupResponse_httpStatus = Lens.lens (\AssociateSkillWithSkillGroupResponse' {httpStatus} -> httpStatus) (\s@AssociateSkillWithSkillGroupResponse' {} a -> s {httpStatus = a} :: AssociateSkillWithSkillGroupResponse)

instance
  Prelude.NFData
    AssociateSkillWithSkillGroupResponse
  where
  rnf AssociateSkillWithSkillGroupResponse' {..} =
    Prelude.rnf httpStatus
