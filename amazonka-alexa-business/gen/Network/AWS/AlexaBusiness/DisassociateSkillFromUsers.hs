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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateSkillFromUsers' smart constructor.
data DisassociateSkillFromUsers = DisassociateSkillFromUsers'
  { -- | The private skill ID you want to make unavailable for enrolled users.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DisassociateSkillFromUsers
newDisassociateSkillFromUsers pSkillId_ =
  DisassociateSkillFromUsers' {skillId = pSkillId_}

-- | The private skill ID you want to make unavailable for enrolled users.
disassociateSkillFromUsers_skillId :: Lens.Lens' DisassociateSkillFromUsers Prelude.Text
disassociateSkillFromUsers_skillId = Lens.lens (\DisassociateSkillFromUsers' {skillId} -> skillId) (\s@DisassociateSkillFromUsers' {} a -> s {skillId = a} :: DisassociateSkillFromUsers)

instance
  Prelude.AWSRequest
    DisassociateSkillFromUsers
  where
  type
    Rs DisassociateSkillFromUsers =
      DisassociateSkillFromUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromUsersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateSkillFromUsers

instance Prelude.NFData DisassociateSkillFromUsers

instance Prelude.ToHeaders DisassociateSkillFromUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DisassociateSkillFromUsers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateSkillFromUsers where
  toJSON DisassociateSkillFromUsers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SkillId" Prelude..= skillId)]
      )

instance Prelude.ToPath DisassociateSkillFromUsers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateSkillFromUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSkillFromUsersResponse' smart constructor.
data DisassociateSkillFromUsersResponse = DisassociateSkillFromUsersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateSkillFromUsersResponse
newDisassociateSkillFromUsersResponse pHttpStatus_ =
  DisassociateSkillFromUsersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateSkillFromUsersResponse_httpStatus :: Lens.Lens' DisassociateSkillFromUsersResponse Prelude.Int
disassociateSkillFromUsersResponse_httpStatus = Lens.lens (\DisassociateSkillFromUsersResponse' {httpStatus} -> httpStatus) (\s@DisassociateSkillFromUsersResponse' {} a -> s {httpStatus = a} :: DisassociateSkillFromUsersResponse)

instance
  Prelude.NFData
    DisassociateSkillFromUsersResponse
