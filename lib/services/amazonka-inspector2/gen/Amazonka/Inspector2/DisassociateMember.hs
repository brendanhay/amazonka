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
-- Module      : Amazonka.Inspector2.DisassociateMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a member account from an Amazon Inspector delegated
-- administrator.
module Amazonka.Inspector2.DisassociateMember
  ( -- * Creating a Request
    DisassociateMember (..),
    newDisassociateMember,

    -- * Request Lenses
    disassociateMember_accountId,

    -- * Destructuring the Response
    DisassociateMemberResponse (..),
    newDisassociateMemberResponse,

    -- * Response Lenses
    disassociateMemberResponse_httpStatus,
    disassociateMemberResponse_accountId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateMember' smart constructor.
data DisassociateMember = DisassociateMember'
  { -- | The Amazon Web Services account ID of the member account to
    -- disassociate.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'disassociateMember_accountId' - The Amazon Web Services account ID of the member account to
-- disassociate.
newDisassociateMember ::
  -- | 'accountId'
  Prelude.Text ->
  DisassociateMember
newDisassociateMember pAccountId_ =
  DisassociateMember' {accountId = pAccountId_}

-- | The Amazon Web Services account ID of the member account to
-- disassociate.
disassociateMember_accountId :: Lens.Lens' DisassociateMember Prelude.Text
disassociateMember_accountId = Lens.lens (\DisassociateMember' {accountId} -> accountId) (\s@DisassociateMember' {} a -> s {accountId = a} :: DisassociateMember)

instance Core.AWSRequest DisassociateMember where
  type
    AWSResponse DisassociateMember =
      DisassociateMemberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "accountId")
      )

instance Prelude.Hashable DisassociateMember where
  hashWithSalt _salt DisassociateMember' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData DisassociateMember where
  rnf DisassociateMember' {..} = Prelude.rnf accountId

instance Core.ToHeaders DisassociateMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateMember where
  toJSON DisassociateMember' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountId" Core..= accountId)]
      )

instance Core.ToPath DisassociateMember where
  toPath = Prelude.const "/members/disassociate"

instance Core.ToQuery DisassociateMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMemberResponse' smart constructor.
data DisassociateMemberResponse = DisassociateMemberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Web Services account ID of the successfully disassociated
    -- member.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateMemberResponse_httpStatus' - The response's http status code.
--
-- 'accountId', 'disassociateMemberResponse_accountId' - The Amazon Web Services account ID of the successfully disassociated
-- member.
newDisassociateMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accountId'
  Prelude.Text ->
  DisassociateMemberResponse
newDisassociateMemberResponse
  pHttpStatus_
  pAccountId_ =
    DisassociateMemberResponse'
      { httpStatus =
          pHttpStatus_,
        accountId = pAccountId_
      }

-- | The response's http status code.
disassociateMemberResponse_httpStatus :: Lens.Lens' DisassociateMemberResponse Prelude.Int
disassociateMemberResponse_httpStatus = Lens.lens (\DisassociateMemberResponse' {httpStatus} -> httpStatus) (\s@DisassociateMemberResponse' {} a -> s {httpStatus = a} :: DisassociateMemberResponse)

-- | The Amazon Web Services account ID of the successfully disassociated
-- member.
disassociateMemberResponse_accountId :: Lens.Lens' DisassociateMemberResponse Prelude.Text
disassociateMemberResponse_accountId = Lens.lens (\DisassociateMemberResponse' {accountId} -> accountId) (\s@DisassociateMemberResponse' {} a -> s {accountId = a} :: DisassociateMemberResponse)

instance Prelude.NFData DisassociateMemberResponse where
  rnf DisassociateMemberResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountId
