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
-- Module      : Amazonka.Detective.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the Detective administrator account for the organization in
-- the current Region.
--
-- If the account does not have Detective enabled, then enables Detective
-- for that account and creates a new behavior graph.
--
-- Can only be called by the organization management account.
--
-- If the organization has a delegated administrator account in
-- Organizations, then the Detective administrator account must be either
-- the delegated administrator account or the organization management
-- account.
--
-- If the organization does not have a delegated administrator account in
-- Organizations, then you can choose any account in the organization. If
-- you choose an account other than the organization management account,
-- Detective calls Organizations to make that account the delegated
-- administrator account for Detective. The organization management account
-- cannot be the delegated administrator account.
module Amazonka.Detective.EnableOrganizationAdminAccount
  ( -- * Creating a Request
    EnableOrganizationAdminAccount (..),
    newEnableOrganizationAdminAccount,

    -- * Request Lenses
    enableOrganizationAdminAccount_accountId,

    -- * Destructuring the Response
    EnableOrganizationAdminAccountResponse (..),
    newEnableOrganizationAdminAccountResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableOrganizationAdminAccount' smart constructor.
data EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { -- | The Amazon Web Services account identifier of the account to designate
    -- as the Detective administrator account for the organization.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'enableOrganizationAdminAccount_accountId' - The Amazon Web Services account identifier of the account to designate
-- as the Detective administrator account for the organization.
newEnableOrganizationAdminAccount ::
  -- | 'accountId'
  Prelude.Text ->
  EnableOrganizationAdminAccount
newEnableOrganizationAdminAccount pAccountId_ =
  EnableOrganizationAdminAccount'
    { accountId =
        pAccountId_
    }

-- | The Amazon Web Services account identifier of the account to designate
-- as the Detective administrator account for the organization.
enableOrganizationAdminAccount_accountId :: Lens.Lens' EnableOrganizationAdminAccount Prelude.Text
enableOrganizationAdminAccount_accountId = Lens.lens (\EnableOrganizationAdminAccount' {accountId} -> accountId) (\s@EnableOrganizationAdminAccount' {} a -> s {accountId = a} :: EnableOrganizationAdminAccount)

instance
  Core.AWSRequest
    EnableOrganizationAdminAccount
  where
  type
    AWSResponse EnableOrganizationAdminAccount =
      EnableOrganizationAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      EnableOrganizationAdminAccountResponse'

instance
  Prelude.Hashable
    EnableOrganizationAdminAccount
  where
  hashWithSalt
    _salt
    EnableOrganizationAdminAccount' {..} =
      _salt `Prelude.hashWithSalt` accountId

instance
  Prelude.NFData
    EnableOrganizationAdminAccount
  where
  rnf EnableOrganizationAdminAccount' {..} =
    Prelude.rnf accountId

instance
  Data.ToHeaders
    EnableOrganizationAdminAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableOrganizationAdminAccount where
  toJSON EnableOrganizationAdminAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountId" Data..= accountId)]
      )

instance Data.ToPath EnableOrganizationAdminAccount where
  toPath = Prelude.const "/orgs/enableAdminAccount"

instance Data.ToQuery EnableOrganizationAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableOrganizationAdminAccountResponse' smart constructor.
data EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableOrganizationAdminAccountResponse ::
  EnableOrganizationAdminAccountResponse
newEnableOrganizationAdminAccountResponse =
  EnableOrganizationAdminAccountResponse'

instance
  Prelude.NFData
    EnableOrganizationAdminAccountResponse
  where
  rnf _ = ()
