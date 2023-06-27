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
-- Module      : Amazonka.Inspector2.AssociateMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Amazon Web Services account with an Amazon Inspector
-- delegated administrator. An HTTP 200 response indicates the association
-- was successfully started, but doesn’t indicate whether it was completed.
-- You can check if the association completed by using
-- <https://docs.aws.amazon.com/inspector/v2/APIReference/API_ListMembers.html ListMembers>
-- for multiple accounts or
-- <https://docs.aws.amazon.com/inspector/v2/APIReference/API_GetMember.html GetMembers>
-- for a single account.
module Amazonka.Inspector2.AssociateMember
  ( -- * Creating a Request
    AssociateMember (..),
    newAssociateMember,

    -- * Request Lenses
    associateMember_accountId,

    -- * Destructuring the Response
    AssociateMemberResponse (..),
    newAssociateMemberResponse,

    -- * Response Lenses
    associateMemberResponse_httpStatus,
    associateMemberResponse_accountId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateMember' smart constructor.
data AssociateMember = AssociateMember'
  { -- | The Amazon Web Services account ID of the member account to be
    -- associated.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'associateMember_accountId' - The Amazon Web Services account ID of the member account to be
-- associated.
newAssociateMember ::
  -- | 'accountId'
  Prelude.Text ->
  AssociateMember
newAssociateMember pAccountId_ =
  AssociateMember' {accountId = pAccountId_}

-- | The Amazon Web Services account ID of the member account to be
-- associated.
associateMember_accountId :: Lens.Lens' AssociateMember Prelude.Text
associateMember_accountId = Lens.lens (\AssociateMember' {accountId} -> accountId) (\s@AssociateMember' {} a -> s {accountId = a} :: AssociateMember)

instance Core.AWSRequest AssociateMember where
  type
    AWSResponse AssociateMember =
      AssociateMemberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "accountId")
      )

instance Prelude.Hashable AssociateMember where
  hashWithSalt _salt AssociateMember' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData AssociateMember where
  rnf AssociateMember' {..} = Prelude.rnf accountId

instance Data.ToHeaders AssociateMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateMember where
  toJSON AssociateMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountId" Data..= accountId)]
      )

instance Data.ToPath AssociateMember where
  toPath = Prelude.const "/members/associate"

instance Data.ToQuery AssociateMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateMemberResponse' smart constructor.
data AssociateMemberResponse = AssociateMemberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Web Services account ID of the successfully associated member
    -- account.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateMemberResponse_httpStatus' - The response's http status code.
--
-- 'accountId', 'associateMemberResponse_accountId' - The Amazon Web Services account ID of the successfully associated member
-- account.
newAssociateMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accountId'
  Prelude.Text ->
  AssociateMemberResponse
newAssociateMemberResponse pHttpStatus_ pAccountId_ =
  AssociateMemberResponse'
    { httpStatus = pHttpStatus_,
      accountId = pAccountId_
    }

-- | The response's http status code.
associateMemberResponse_httpStatus :: Lens.Lens' AssociateMemberResponse Prelude.Int
associateMemberResponse_httpStatus = Lens.lens (\AssociateMemberResponse' {httpStatus} -> httpStatus) (\s@AssociateMemberResponse' {} a -> s {httpStatus = a} :: AssociateMemberResponse)

-- | The Amazon Web Services account ID of the successfully associated member
-- account.
associateMemberResponse_accountId :: Lens.Lens' AssociateMemberResponse Prelude.Text
associateMemberResponse_accountId = Lens.lens (\AssociateMemberResponse' {accountId} -> accountId) (\s@AssociateMemberResponse' {} a -> s {accountId = a} :: AssociateMemberResponse)

instance Prelude.NFData AssociateMemberResponse where
  rnf AssociateMemberResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountId
