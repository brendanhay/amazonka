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
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from a skill group.
module Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateSkillFromSkillGroup' smart constructor.
data DisassociateSkillFromSkillGroup = DisassociateSkillFromSkillGroup'
  { -- | The unique identifier of a skill. Required.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a skill group to associate to a skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DisassociateSkillFromSkillGroup
  where
  type
    Rs DisassociateSkillFromSkillGroup =
      DisassociateSkillFromSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromSkillGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateSkillFromSkillGroup

instance
  Prelude.NFData
    DisassociateSkillFromSkillGroup

instance
  Prelude.ToHeaders
    DisassociateSkillFromSkillGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DisassociateSkillFromSkillGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DisassociateSkillFromSkillGroup
  where
  toJSON DisassociateSkillFromSkillGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SkillGroupArn" Prelude..=)
              Prelude.<$> skillGroupArn,
            Prelude.Just ("SkillId" Prelude..= skillId)
          ]
      )

instance
  Prelude.ToPath
    DisassociateSkillFromSkillGroup
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateSkillFromSkillGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSkillFromSkillGroupResponse' smart constructor.
data DisassociateSkillFromSkillGroupResponse = DisassociateSkillFromSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
