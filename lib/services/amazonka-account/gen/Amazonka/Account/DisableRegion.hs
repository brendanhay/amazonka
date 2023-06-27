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
-- Module      : Amazonka.Account.DisableRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables (opts-out) a particular Region for an account.
module Amazonka.Account.DisableRegion
  ( -- * Creating a Request
    DisableRegion (..),
    newDisableRegion,

    -- * Request Lenses
    disableRegion_accountId,
    disableRegion_regionName,

    -- * Destructuring the Response
    DisableRegionResponse (..),
    newDisableRegionResponse,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableRegion' smart constructor.
data DisableRegion = DisableRegion'
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
    -- @af-south-1@). When you disable a Region, Amazon Web Services performs
    -- actions to deactivate that Region in your account, such as destroying
    -- IAM resources in the Region. This process takes a few minutes for most
    -- accounts, but this can take several hours. You cannot enable the Region
    -- until the disabling process is fully completed.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'disableRegion_accountId' - Specifies the 12-digit account ID number of the Amazon Web Services
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
-- 'regionName', 'disableRegion_regionName' - Specifies the Region-code for a given Region name (for example,
-- @af-south-1@). When you disable a Region, Amazon Web Services performs
-- actions to deactivate that Region in your account, such as destroying
-- IAM resources in the Region. This process takes a few minutes for most
-- accounts, but this can take several hours. You cannot enable the Region
-- until the disabling process is fully completed.
newDisableRegion ::
  -- | 'regionName'
  Prelude.Text ->
  DisableRegion
newDisableRegion pRegionName_ =
  DisableRegion'
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
disableRegion_accountId :: Lens.Lens' DisableRegion (Prelude.Maybe Prelude.Text)
disableRegion_accountId = Lens.lens (\DisableRegion' {accountId} -> accountId) (\s@DisableRegion' {} a -> s {accountId = a} :: DisableRegion)

-- | Specifies the Region-code for a given Region name (for example,
-- @af-south-1@). When you disable a Region, Amazon Web Services performs
-- actions to deactivate that Region in your account, such as destroying
-- IAM resources in the Region. This process takes a few minutes for most
-- accounts, but this can take several hours. You cannot enable the Region
-- until the disabling process is fully completed.
disableRegion_regionName :: Lens.Lens' DisableRegion Prelude.Text
disableRegion_regionName = Lens.lens (\DisableRegion' {regionName} -> regionName) (\s@DisableRegion' {} a -> s {regionName = a} :: DisableRegion)

instance Core.AWSRequest DisableRegion where
  type
    AWSResponse DisableRegion =
      DisableRegionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DisableRegionResponse'

instance Prelude.Hashable DisableRegion where
  hashWithSalt _salt DisableRegion' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData DisableRegion where
  rnf DisableRegion' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf regionName

instance Data.ToHeaders DisableRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableRegion where
  toJSON DisableRegion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            Prelude.Just ("RegionName" Data..= regionName)
          ]
      )

instance Data.ToPath DisableRegion where
  toPath = Prelude.const "/disableRegion"

instance Data.ToQuery DisableRegion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableRegionResponse' smart constructor.
data DisableRegionResponse = DisableRegionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableRegionResponse ::
  DisableRegionResponse
newDisableRegionResponse = DisableRegionResponse'

instance Prelude.NFData DisableRegionResponse where
  rnf _ = ()
