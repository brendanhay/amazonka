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
-- Module      : Amazonka.AlexaBusiness.DisassociateSkillFromSkillGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from a skill group.
module Amazonka.AlexaBusiness.DisassociateSkillFromSkillGroup
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateSkillFromSkillGroup' smart constructor.
data DisassociateSkillFromSkillGroup = DisassociateSkillFromSkillGroup'
  { -- | The unique identifier of a skill. Required.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a skill group to associate to a skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DisassociateSkillFromSkillGroup
newDisassociateSkillFromSkillGroup pSkillId_ =
  DisassociateSkillFromSkillGroup'
    { skillGroupArn =
        Prelude.Nothing,
      skillId = pSkillId_
    }

-- | The unique identifier of a skill. Required.
disassociateSkillFromSkillGroup_skillGroupArn :: Lens.Lens' DisassociateSkillFromSkillGroup (Prelude.Maybe Prelude.Text)
disassociateSkillFromSkillGroup_skillGroupArn = Lens.lens (\DisassociateSkillFromSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@DisassociateSkillFromSkillGroup' {} a -> s {skillGroupArn = a} :: DisassociateSkillFromSkillGroup)

-- | The ARN of a skill group to associate to a skill.
disassociateSkillFromSkillGroup_skillId :: Lens.Lens' DisassociateSkillFromSkillGroup Prelude.Text
disassociateSkillFromSkillGroup_skillId = Lens.lens (\DisassociateSkillFromSkillGroup' {skillId} -> skillId) (\s@DisassociateSkillFromSkillGroup' {} a -> s {skillId = a} :: DisassociateSkillFromSkillGroup)

instance
  Core.AWSRequest
    DisassociateSkillFromSkillGroup
  where
  type
    AWSResponse DisassociateSkillFromSkillGroup =
      DisassociateSkillFromSkillGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromSkillGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateSkillFromSkillGroup
  where
  hashWithSalt
    _salt
    DisassociateSkillFromSkillGroup' {..} =
      _salt
        `Prelude.hashWithSalt` skillGroupArn
        `Prelude.hashWithSalt` skillId

instance
  Prelude.NFData
    DisassociateSkillFromSkillGroup
  where
  rnf DisassociateSkillFromSkillGroup' {..} =
    Prelude.rnf skillGroupArn
      `Prelude.seq` Prelude.rnf skillId

instance
  Data.ToHeaders
    DisassociateSkillFromSkillGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DisassociateSkillFromSkillGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateSkillFromSkillGroup where
  toJSON DisassociateSkillFromSkillGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SkillGroupArn" Data..=) Prelude.<$> skillGroupArn,
            Prelude.Just ("SkillId" Data..= skillId)
          ]
      )

instance Data.ToPath DisassociateSkillFromSkillGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateSkillFromSkillGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSkillFromSkillGroupResponse' smart constructor.
data DisassociateSkillFromSkillGroupResponse = DisassociateSkillFromSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateSkillFromSkillGroupResponse
newDisassociateSkillFromSkillGroupResponse
  pHttpStatus_ =
    DisassociateSkillFromSkillGroupResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateSkillFromSkillGroupResponse_httpStatus :: Lens.Lens' DisassociateSkillFromSkillGroupResponse Prelude.Int
disassociateSkillFromSkillGroupResponse_httpStatus = Lens.lens (\DisassociateSkillFromSkillGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateSkillFromSkillGroupResponse' {} a -> s {httpStatus = a} :: DisassociateSkillFromSkillGroupResponse)

instance
  Prelude.NFData
    DisassociateSkillFromSkillGroupResponse
  where
  rnf DisassociateSkillFromSkillGroupResponse' {..} =
    Prelude.rnf httpStatus
