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
-- Module      : Amazonka.AlexaBusiness.RejectSkill
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from the organization under a user\'s AWS account.
-- If the skill is a private skill, it moves to an AcceptStatus of PENDING.
-- Any private or public skill that is rejected can be added later by
-- calling the ApproveSkill API.
module Amazonka.AlexaBusiness.RejectSkill
  ( -- * Creating a Request
    RejectSkill (..),
    newRejectSkill,

    -- * Request Lenses
    rejectSkill_skillId,

    -- * Destructuring the Response
    RejectSkillResponse (..),
    newRejectSkillResponse,

    -- * Response Lenses
    rejectSkillResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectSkill' smart constructor.
data RejectSkill = RejectSkill'
  { -- | The unique identifier of the skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectSkill' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillId', 'rejectSkill_skillId' - The unique identifier of the skill.
newRejectSkill ::
  -- | 'skillId'
  Prelude.Text ->
  RejectSkill
newRejectSkill pSkillId_ =
  RejectSkill' {skillId = pSkillId_}

-- | The unique identifier of the skill.
rejectSkill_skillId :: Lens.Lens' RejectSkill Prelude.Text
rejectSkill_skillId = Lens.lens (\RejectSkill' {skillId} -> skillId) (\s@RejectSkill' {} a -> s {skillId = a} :: RejectSkill)

instance Core.AWSRequest RejectSkill where
  type AWSResponse RejectSkill = RejectSkillResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectSkillResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectSkill where
  hashWithSalt _salt RejectSkill' {..} =
    _salt `Prelude.hashWithSalt` skillId

instance Prelude.NFData RejectSkill where
  rnf RejectSkill' {..} = Prelude.rnf skillId

instance Data.ToHeaders RejectSkill where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.RejectSkill" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RejectSkill where
  toJSON RejectSkill' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SkillId" Data..= skillId)]
      )

instance Data.ToPath RejectSkill where
  toPath = Prelude.const "/"

instance Data.ToQuery RejectSkill where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectSkillResponse' smart constructor.
data RejectSkillResponse = RejectSkillResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectSkillResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectSkillResponse_httpStatus' - The response's http status code.
newRejectSkillResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectSkillResponse
newRejectSkillResponse pHttpStatus_ =
  RejectSkillResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
rejectSkillResponse_httpStatus :: Lens.Lens' RejectSkillResponse Prelude.Int
rejectSkillResponse_httpStatus = Lens.lens (\RejectSkillResponse' {httpStatus} -> httpStatus) (\s@RejectSkillResponse' {} a -> s {httpStatus = a} :: RejectSkillResponse)

instance Prelude.NFData RejectSkillResponse where
  rnf RejectSkillResponse' {..} = Prelude.rnf httpStatus
