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
-- Module      : Amazonka.Organizations.CloseAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes an Amazon Web Services member account within an organization. You
-- can close an account when
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features are enabled>
-- . You can\'t close the management account with this API. This is an
-- asynchronous request that Amazon Web Services performs in the
-- background. Because @CloseAccount@ operates asynchronously, it can
-- return a successful completion message even though account closure might
-- still be in progress. You need to wait a few minutes before the account
-- is fully closed. To check the status of the request, do one of the
-- following:
--
-- -   Use the @AccountId@ that you sent in the @CloseAccount@ request to
--     provide as a parameter to the DescribeAccount operation.
--
--     While the close account request is in progress, Account status will
--     indicate PENDING_CLOSURE. When the close account request completes,
--     the status will change to SUSPENDED.
--
-- -   Check the CloudTrail log for the @CloseAccountResult@ event that
--     gets published after the account closes successfully. For
--     information on using CloudTrail with Organizations, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_security_incident-response.html#orgs_cloudtrail-integration Logging and monitoring in Organizations>
--     in the /Organizations User Guide./
--
-- -   You can close only 10% of member accounts, between 10 and 200,
--     within a rolling 30 day period. This quota is not bound by a
--     calendar month, but starts when you close an account.
--
--     After you reach this limit, you can close additional accounts in the
--     Billing console. For more information, see
--     <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/close-account.html Closing an account>
--     in the Amazon Web Services Billing and Cost Management User Guide.
--
-- -   To reinstate a closed account, contact Amazon Web Services Support
--     within the 90-day grace period while the account is in SUSPENDED
--     status.
--
-- -   If the Amazon Web Services account you attempt to close is linked to
--     an Amazon Web Services GovCloud (US) account, the @CloseAccount@
--     request will close both accounts. To learn important pre-closure
--     details, see
--     <https://docs.aws.amazon.com/govcloud-us/latest/UserGuide/Closing-govcloud-account.html Closing an Amazon Web Services GovCloud (US) account>
--     in the /Amazon Web Services GovCloud User Guide/.
--
-- For more information about closing accounts, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_close.html Closing an Amazon Web Services account>
-- in the /Organizations User Guide./
module Amazonka.Organizations.CloseAccount
  ( -- * Creating a Request
    CloseAccount (..),
    newCloseAccount,

    -- * Request Lenses
    closeAccount_accountId,

    -- * Destructuring the Response
    CloseAccountResponse (..),
    newCloseAccountResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCloseAccount' smart constructor.
data CloseAccount = CloseAccount'
  { -- | Retrieves the Amazon Web Services account Id for the current
    -- @CloseAccount@ API request.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloseAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'closeAccount_accountId' - Retrieves the Amazon Web Services account Id for the current
-- @CloseAccount@ API request.
newCloseAccount ::
  -- | 'accountId'
  Prelude.Text ->
  CloseAccount
newCloseAccount pAccountId_ =
  CloseAccount' {accountId = pAccountId_}

-- | Retrieves the Amazon Web Services account Id for the current
-- @CloseAccount@ API request.
closeAccount_accountId :: Lens.Lens' CloseAccount Prelude.Text
closeAccount_accountId = Lens.lens (\CloseAccount' {accountId} -> accountId) (\s@CloseAccount' {} a -> s {accountId = a} :: CloseAccount)

instance Core.AWSRequest CloseAccount where
  type AWSResponse CloseAccount = CloseAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull CloseAccountResponse'

instance Prelude.Hashable CloseAccount where
  hashWithSalt _salt CloseAccount' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData CloseAccount where
  rnf CloseAccount' {..} = Prelude.rnf accountId

instance Data.ToHeaders CloseAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.CloseAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CloseAccount where
  toJSON CloseAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountId" Data..= accountId)]
      )

instance Data.ToPath CloseAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery CloseAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCloseAccountResponse' smart constructor.
data CloseAccountResponse = CloseAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloseAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCloseAccountResponse ::
  CloseAccountResponse
newCloseAccountResponse = CloseAccountResponse'

instance Prelude.NFData CloseAccountResponse where
  rnf _ = ()
