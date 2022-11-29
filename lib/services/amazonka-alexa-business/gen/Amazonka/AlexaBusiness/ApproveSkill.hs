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
-- Module      : Amazonka.AlexaBusiness.ApproveSkill
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with the organization under the customer\'s AWS
-- account. If a skill is private, the user implicitly accepts access to
-- this skill during enablement.
module Amazonka.AlexaBusiness.ApproveSkill
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newApproveSkill' smart constructor.
data ApproveSkill = ApproveSkill'
  { -- | The unique identifier of the skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ApproveSkill
newApproveSkill pSkillId_ =
  ApproveSkill' {skillId = pSkillId_}

-- | The unique identifier of the skill.
approveSkill_skillId :: Lens.Lens' ApproveSkill Prelude.Text
approveSkill_skillId = Lens.lens (\ApproveSkill' {skillId} -> skillId) (\s@ApproveSkill' {} a -> s {skillId = a} :: ApproveSkill)

instance Core.AWSRequest ApproveSkill where
  type AWSResponse ApproveSkill = ApproveSkillResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ApproveSkillResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ApproveSkill where
  hashWithSalt _salt ApproveSkill' {..} =
    _salt `Prelude.hashWithSalt` skillId

instance Prelude.NFData ApproveSkill where
  rnf ApproveSkill' {..} = Prelude.rnf skillId

instance Core.ToHeaders ApproveSkill where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ApproveSkill" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ApproveSkill where
  toJSON ApproveSkill' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SkillId" Core..= skillId)]
      )

instance Core.ToPath ApproveSkill where
  toPath = Prelude.const "/"

instance Core.ToQuery ApproveSkill where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newApproveSkillResponse' smart constructor.
data ApproveSkillResponse = ApproveSkillResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ApproveSkillResponse
newApproveSkillResponse pHttpStatus_ =
  ApproveSkillResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
approveSkillResponse_httpStatus :: Lens.Lens' ApproveSkillResponse Prelude.Int
approveSkillResponse_httpStatus = Lens.lens (\ApproveSkillResponse' {httpStatus} -> httpStatus) (\s@ApproveSkillResponse' {} a -> s {httpStatus = a} :: ApproveSkillResponse)

instance Prelude.NFData ApproveSkillResponse where
  rnf ApproveSkillResponse' {..} =
    Prelude.rnf httpStatus
