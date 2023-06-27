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
-- Module      : Amazonka.Account.EnableRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables (opts-in) a particular Region for an account.
module Amazonka.Account.EnableRegion
  ( -- * Creating a Request
    EnableRegion (..),
    newEnableRegion,

    -- * Request Lenses
    enableRegion_accountId,
    enableRegion_regionName,

    -- * Destructuring the Response
    EnableRegionResponse (..),
    newEnableRegionResponse,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableRegion' smart constructor.
data EnableRegion = EnableRegion'
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
    -- @af-south-1@). When you enable a Region, Amazon Web Services performs
    -- actions to prepare your account in that Region, such as distributing
    -- your IAM resources to the Region. This process takes a few minutes for
    -- most accounts, but it can take several hours. You cannot use the Region
    -- until this process is complete. Furthermore, you cannot disable the
    -- Region until the enabling process is fully completed.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'enableRegion_accountId' - Specifies the 12-digit account ID number of the Amazon Web Services
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
-- 'regionName', 'enableRegion_regionName' - Specifies the Region-code for a given Region name (for example,
-- @af-south-1@). When you enable a Region, Amazon Web Services performs
-- actions to prepare your account in that Region, such as distributing
-- your IAM resources to the Region. This process takes a few minutes for
-- most accounts, but it can take several hours. You cannot use the Region
-- until this process is complete. Furthermore, you cannot disable the
-- Region until the enabling process is fully completed.
newEnableRegion ::
  -- | 'regionName'
  Prelude.Text ->
  EnableRegion
newEnableRegion pRegionName_ =
  EnableRegion'
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
enableRegion_accountId :: Lens.Lens' EnableRegion (Prelude.Maybe Prelude.Text)
enableRegion_accountId = Lens.lens (\EnableRegion' {accountId} -> accountId) (\s@EnableRegion' {} a -> s {accountId = a} :: EnableRegion)

-- | Specifies the Region-code for a given Region name (for example,
-- @af-south-1@). When you enable a Region, Amazon Web Services performs
-- actions to prepare your account in that Region, such as distributing
-- your IAM resources to the Region. This process takes a few minutes for
-- most accounts, but it can take several hours. You cannot use the Region
-- until this process is complete. Furthermore, you cannot disable the
-- Region until the enabling process is fully completed.
enableRegion_regionName :: Lens.Lens' EnableRegion Prelude.Text
enableRegion_regionName = Lens.lens (\EnableRegion' {regionName} -> regionName) (\s@EnableRegion' {} a -> s {regionName = a} :: EnableRegion)

instance Core.AWSRequest EnableRegion where
  type AWSResponse EnableRegion = EnableRegionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull EnableRegionResponse'

instance Prelude.Hashable EnableRegion where
  hashWithSalt _salt EnableRegion' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData EnableRegion where
  rnf EnableRegion' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf regionName

instance Data.ToHeaders EnableRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableRegion where
  toJSON EnableRegion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            Prelude.Just ("RegionName" Data..= regionName)
          ]
      )

instance Data.ToPath EnableRegion where
  toPath = Prelude.const "/enableRegion"

instance Data.ToQuery EnableRegion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableRegionResponse' smart constructor.
data EnableRegionResponse = EnableRegionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableRegionResponse ::
  EnableRegionResponse
newEnableRegionResponse = EnableRegionResponse'

instance Prelude.NFData EnableRegionResponse where
  rnf _ = ()
