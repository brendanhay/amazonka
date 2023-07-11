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
-- Module      : Amazonka.Organizations.LeaveOrganization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member account from its parent organization. This version of
-- the operation is performed by the account that wants to leave. To remove
-- a member account as a user in the management account, use
-- RemoveAccountFromOrganization instead.
--
-- This operation can be called only from a member account in the
-- organization.
--
-- -   The management account in an organization with all features enabled
--     can set service control policies (SCPs) that can restrict what
--     administrators of member accounts can do. This includes preventing
--     them from successfully calling @LeaveOrganization@ and leaving the
--     organization.
--
-- -   You can leave an organization as a member account only if the
--     account is configured with the information required to operate as a
--     standalone account. When you create an account in an organization
--     using the Organizations console, API, or CLI commands, the
--     information required of standalone accounts is /not/ automatically
--     collected. For each account that you want to make standalone, you
--     must perform the following steps. If any of the steps are already
--     completed for this account, that step doesn\'t appear.
--
--     -   Choose a support plan
--
--     -   Provide and verify the required contact information
--
--     -   Provide a current payment method
--
--     Amazon Web Services uses the payment method to charge for any
--     billable (not free tier) Amazon Web Services activity that occurs
--     while the account isn\'t attached to an organization. Follow the
--     steps at
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided>
--     in the /Organizations User Guide./
--
-- -   The account that you want to leave must not be a delegated
--     administrator account for any Amazon Web Services service enabled
--     for your organization. If the account is a delegated administrator,
--     you must first change the delegated administrator account to another
--     account that is remaining in the organization.
--
-- -   You can leave an organization only after you enable IAM user access
--     to billing in your account. For more information, see
--     <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console>
--     in the /Amazon Web Services Billing and Cost Management User Guide./
--
-- -   After the account leaves the organization, all tags that were
--     attached to the account object in the organization are deleted.
--     Amazon Web Services accounts outside of an organization do not
--     support tags.
--
-- -   A newly created account has a waiting period before it can be
--     removed from its organization. If you get an error that indicates
--     that a wait period is required, then try again in a few days.
module Amazonka.Organizations.LeaveOrganization
  ( -- * Creating a Request
    LeaveOrganization (..),
    newLeaveOrganization,

    -- * Destructuring the Response
    LeaveOrganizationResponse (..),
    newLeaveOrganizationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newLeaveOrganization' smart constructor.
data LeaveOrganization = LeaveOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LeaveOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newLeaveOrganization ::
  LeaveOrganization
newLeaveOrganization = LeaveOrganization'

instance Core.AWSRequest LeaveOrganization where
  type
    AWSResponse LeaveOrganization =
      LeaveOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull LeaveOrganizationResponse'

instance Prelude.Hashable LeaveOrganization where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData LeaveOrganization where
  rnf _ = ()

instance Data.ToHeaders LeaveOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.LeaveOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON LeaveOrganization where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath LeaveOrganization where
  toPath = Prelude.const "/"

instance Data.ToQuery LeaveOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newLeaveOrganizationResponse' smart constructor.
data LeaveOrganizationResponse = LeaveOrganizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LeaveOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newLeaveOrganizationResponse ::
  LeaveOrganizationResponse
newLeaveOrganizationResponse =
  LeaveOrganizationResponse'

instance Prelude.NFData LeaveOrganizationResponse where
  rnf _ = ()
