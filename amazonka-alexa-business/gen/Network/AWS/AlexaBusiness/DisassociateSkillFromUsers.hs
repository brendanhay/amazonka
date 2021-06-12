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
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill unavailable for enrolled users and prevents them
-- from enabling it on their devices.
module Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
  ( -- * Creating a Request
    DisassociateSkillFromUsers (..),
    newDisassociateSkillFromUsers,

    -- * Request Lenses
    disassociateSkillFromUsers_skillId,

    -- * Destructuring the Response
    DisassociateSkillFromUsersResponse (..),
    newDisassociateSkillFromUsersResponse,

    -- * Response Lenses
    disassociateSkillFromUsersResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateSkillFromUsers' smart constructor.
data DisassociateSkillFromUsers = DisassociateSkillFromUsers'
  { -- | The private skill ID you want to make unavailable for enrolled users.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateSkillFromUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillId', 'disassociateSkillFromUsers_skillId' - The private skill ID you want to make unavailable for enrolled users.
newDisassociateSkillFromUsers ::
  -- | 'skillId'
  Core.Text ->
  DisassociateSkillFromUsers
newDisassociateSkillFromUsers pSkillId_ =
  DisassociateSkillFromUsers' {skillId = pSkillId_}

-- | The private skill ID you want to make unavailable for enrolled users.
disassociateSkillFromUsers_skillId :: Lens.Lens' DisassociateSkillFromUsers Core.Text
disassociateSkillFromUsers_skillId = Lens.lens (\DisassociateSkillFromUsers' {skillId} -> skillId) (\s@DisassociateSkillFromUsers' {} a -> s {skillId = a} :: DisassociateSkillFromUsers)

instance Core.AWSRequest DisassociateSkillFromUsers where
  type
    AWSResponse DisassociateSkillFromUsers =
      DisassociateSkillFromUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromUsersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateSkillFromUsers

instance Core.NFData DisassociateSkillFromUsers

instance Core.ToHeaders DisassociateSkillFromUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DisassociateSkillFromUsers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateSkillFromUsers where
  toJSON DisassociateSkillFromUsers' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SkillId" Core..= skillId)]
      )

instance Core.ToPath DisassociateSkillFromUsers where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateSkillFromUsers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateSkillFromUsersResponse' smart constructor.
data DisassociateSkillFromUsersResponse = DisassociateSkillFromUsersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateSkillFromUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateSkillFromUsersResponse_httpStatus' - The response's http status code.
newDisassociateSkillFromUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateSkillFromUsersResponse
newDisassociateSkillFromUsersResponse pHttpStatus_ =
  DisassociateSkillFromUsersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateSkillFromUsersResponse_httpStatus :: Lens.Lens' DisassociateSkillFromUsersResponse Core.Int
disassociateSkillFromUsersResponse_httpStatus = Lens.lens (\DisassociateSkillFromUsersResponse' {httpStatus} -> httpStatus) (\s@DisassociateSkillFromUsersResponse' {} a -> s {httpStatus = a} :: DisassociateSkillFromUsersResponse)

instance
  Core.NFData
    DisassociateSkillFromUsersResponse
