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
-- Module      : Amazonka.Account.GetRegionOptStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the opt-in status of a particular Region.
module Amazonka.Account.GetRegionOptStatus
  ( -- * Creating a Request
    GetRegionOptStatus (..),
    newGetRegionOptStatus,

    -- * Request Lenses
    getRegionOptStatus_accountId,
    getRegionOptStatus_regionName,

    -- * Destructuring the Response
    GetRegionOptStatusResponse (..),
    newGetRegionOptStatusResponse,

    -- * Response Lenses
    getRegionOptStatusResponse_regionName,
    getRegionOptStatusResponse_regionOptStatus,
    getRegionOptStatusResponse_httpStatus,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRegionOptStatus' smart constructor.
data GetRegionOptStatus = GetRegionOptStatus'
  { -- | Specifies the 12-digit account ID number of the Amazon Web Services
    -- account that you want to access or modify with this operation. If you
    -- don\'t specify this parameter, it defaults to the Amazon Web Services
    -- account of the identity used to call the operation. To use this
    -- parameter, the caller must be an identity in the
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
    -- or a delegated administrator account. The specified account ID must also
    -- be a member account in the same organization. The organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
    -- and the organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
    -- enabled for the Account Management service, and optionally a
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
    -- account assigned.
    --
    -- The management account can\'t specify its own @AccountId@. It must call
    -- the operation in standalone context by not including the @AccountId@
    -- parameter.
    --
    -- To call this operation on an account that is not a member of an
    -- organization, don\'t specify this parameter. Instead, call the operation
    -- using an identity belonging to the account whose contacts you wish to
    -- retrieve or modify.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Region-code for a given Region name (for example,
    -- @af-south-1@). This function will return the status of whatever Region
    -- you pass into this parameter.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegionOptStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getRegionOptStatus_accountId' - Specifies the 12-digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation. If you
-- don\'t specify this parameter, it defaults to the Amazon Web Services
-- account of the identity used to call the operation. To use this
-- parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account. The specified account ID must also
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@. It must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, don\'t specify this parameter. Instead, call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
--
-- 'regionName', 'getRegionOptStatus_regionName' - Specifies the Region-code for a given Region name (for example,
-- @af-south-1@). This function will return the status of whatever Region
-- you pass into this parameter.
newGetRegionOptStatus ::
  -- | 'regionName'
  Prelude.Text ->
  GetRegionOptStatus
newGetRegionOptStatus pRegionName_ =
  GetRegionOptStatus'
    { accountId = Prelude.Nothing,
      regionName = pRegionName_
    }

-- | Specifies the 12-digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation. If you
-- don\'t specify this parameter, it defaults to the Amazon Web Services
-- account of the identity used to call the operation. To use this
-- parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account. The specified account ID must also
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@. It must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, don\'t specify this parameter. Instead, call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
getRegionOptStatus_accountId :: Lens.Lens' GetRegionOptStatus (Prelude.Maybe Prelude.Text)
getRegionOptStatus_accountId = Lens.lens (\GetRegionOptStatus' {accountId} -> accountId) (\s@GetRegionOptStatus' {} a -> s {accountId = a} :: GetRegionOptStatus)

-- | Specifies the Region-code for a given Region name (for example,
-- @af-south-1@). This function will return the status of whatever Region
-- you pass into this parameter.
getRegionOptStatus_regionName :: Lens.Lens' GetRegionOptStatus Prelude.Text
getRegionOptStatus_regionName = Lens.lens (\GetRegionOptStatus' {regionName} -> regionName) (\s@GetRegionOptStatus' {} a -> s {regionName = a} :: GetRegionOptStatus)

instance Core.AWSRequest GetRegionOptStatus where
  type
    AWSResponse GetRegionOptStatus =
      GetRegionOptStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegionOptStatusResponse'
            Prelude.<$> (x Data..?> "RegionName")
            Prelude.<*> (x Data..?> "RegionOptStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegionOptStatus where
  hashWithSalt _salt GetRegionOptStatus' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData GetRegionOptStatus where
  rnf GetRegionOptStatus' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf regionName

instance Data.ToHeaders GetRegionOptStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRegionOptStatus where
  toJSON GetRegionOptStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            Prelude.Just ("RegionName" Data..= regionName)
          ]
      )

instance Data.ToPath GetRegionOptStatus where
  toPath = Prelude.const "/getRegionOptStatus"

instance Data.ToQuery GetRegionOptStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegionOptStatusResponse' smart constructor.
data GetRegionOptStatusResponse = GetRegionOptStatusResponse'
  { -- | The Region code that was passed in.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | One of the potential statuses a Region can undergo (Enabled, Enabling,
    -- Disabled, Disabling, Enabled_By_Default).
    regionOptStatus :: Prelude.Maybe RegionOptStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegionOptStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'getRegionOptStatusResponse_regionName' - The Region code that was passed in.
--
-- 'regionOptStatus', 'getRegionOptStatusResponse_regionOptStatus' - One of the potential statuses a Region can undergo (Enabled, Enabling,
-- Disabled, Disabling, Enabled_By_Default).
--
-- 'httpStatus', 'getRegionOptStatusResponse_httpStatus' - The response's http status code.
newGetRegionOptStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegionOptStatusResponse
newGetRegionOptStatusResponse pHttpStatus_ =
  GetRegionOptStatusResponse'
    { regionName =
        Prelude.Nothing,
      regionOptStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Region code that was passed in.
getRegionOptStatusResponse_regionName :: Lens.Lens' GetRegionOptStatusResponse (Prelude.Maybe Prelude.Text)
getRegionOptStatusResponse_regionName = Lens.lens (\GetRegionOptStatusResponse' {regionName} -> regionName) (\s@GetRegionOptStatusResponse' {} a -> s {regionName = a} :: GetRegionOptStatusResponse)

-- | One of the potential statuses a Region can undergo (Enabled, Enabling,
-- Disabled, Disabling, Enabled_By_Default).
getRegionOptStatusResponse_regionOptStatus :: Lens.Lens' GetRegionOptStatusResponse (Prelude.Maybe RegionOptStatus)
getRegionOptStatusResponse_regionOptStatus = Lens.lens (\GetRegionOptStatusResponse' {regionOptStatus} -> regionOptStatus) (\s@GetRegionOptStatusResponse' {} a -> s {regionOptStatus = a} :: GetRegionOptStatusResponse)

-- | The response's http status code.
getRegionOptStatusResponse_httpStatus :: Lens.Lens' GetRegionOptStatusResponse Prelude.Int
getRegionOptStatusResponse_httpStatus = Lens.lens (\GetRegionOptStatusResponse' {httpStatus} -> httpStatus) (\s@GetRegionOptStatusResponse' {} a -> s {httpStatus = a} :: GetRegionOptStatusResponse)

instance Prelude.NFData GetRegionOptStatusResponse where
  rnf GetRegionOptStatusResponse' {..} =
    Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf regionOptStatus
      `Prelude.seq` Prelude.rnf httpStatus
