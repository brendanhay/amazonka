{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.Types.OrganizationResourceDetailedStatusFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationResourceDetailedStatusFilters where

import Amazonka.Config.Types.OrganizationResourceDetailedStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Status filter object to filter results based on specific member account
-- ID or status type for an organization conformance pack.
--
-- /See:/ 'newOrganizationResourceDetailedStatusFilters' smart constructor.
data OrganizationResourceDetailedStatusFilters = OrganizationResourceDetailedStatusFilters'
  { -- | The 12-digit account ID of the member account within an organization.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Indicates deployment status for conformance pack in a member account.
    -- When management account calls @PutOrganizationConformancePack@ action
    -- for the first time, conformance pack status is created in the member
    -- account. When management account calls @PutOrganizationConformancePack@
    -- action for the second time, conformance pack status is updated in the
    -- member account. Conformance pack status is deleted when the management
    -- account deletes @OrganizationConformancePack@ and disables service
    -- access for @config-multiaccountsetup.amazonaws.com@.
    --
    -- Config sets the state of the conformance pack to:
    --
    -- -   @CREATE_SUCCESSFUL@ when conformance pack has been created in the
    --     member account.
    --
    -- -   @CREATE_IN_PROGRESS@ when conformance pack is being created in the
    --     member account.
    --
    -- -   @CREATE_FAILED@ when conformance pack creation has failed in the
    --     member account.
    --
    -- -   @DELETE_FAILED@ when conformance pack deletion has failed in the
    --     member account.
    --
    -- -   @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the
    --     member account.
    --
    -- -   @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the
    --     member account.
    --
    -- -   @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the
    --     member account.
    --
    -- -   @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the
    --     member account.
    --
    -- -   @UPDATE_FAILED@ when conformance pack deletion has failed in the
    --     member account.
    status :: Prelude.Maybe OrganizationResourceDetailedStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationResourceDetailedStatusFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'organizationResourceDetailedStatusFilters_accountId' - The 12-digit account ID of the member account within an organization.
--
-- 'status', 'organizationResourceDetailedStatusFilters_status' - Indicates deployment status for conformance pack in a member account.
-- When management account calls @PutOrganizationConformancePack@ action
-- for the first time, conformance pack status is created in the member
-- account. When management account calls @PutOrganizationConformancePack@
-- action for the second time, conformance pack status is updated in the
-- member account. Conformance pack status is deleted when the management
-- account deletes @OrganizationConformancePack@ and disables service
-- access for @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the conformance pack to:
--
-- -   @CREATE_SUCCESSFUL@ when conformance pack has been created in the
--     member account.
--
-- -   @CREATE_IN_PROGRESS@ when conformance pack is being created in the
--     member account.
--
-- -   @CREATE_FAILED@ when conformance pack creation has failed in the
--     member account.
--
-- -   @DELETE_FAILED@ when conformance pack deletion has failed in the
--     member account.
--
-- -   @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the
--     member account.
--
-- -   @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the
--     member account.
--
-- -   @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the
--     member account.
--
-- -   @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the
--     member account.
--
-- -   @UPDATE_FAILED@ when conformance pack deletion has failed in the
--     member account.
newOrganizationResourceDetailedStatusFilters ::
  OrganizationResourceDetailedStatusFilters
newOrganizationResourceDetailedStatusFilters =
  OrganizationResourceDetailedStatusFilters'
    { accountId =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The 12-digit account ID of the member account within an organization.
organizationResourceDetailedStatusFilters_accountId :: Lens.Lens' OrganizationResourceDetailedStatusFilters (Prelude.Maybe Prelude.Text)
organizationResourceDetailedStatusFilters_accountId = Lens.lens (\OrganizationResourceDetailedStatusFilters' {accountId} -> accountId) (\s@OrganizationResourceDetailedStatusFilters' {} a -> s {accountId = a} :: OrganizationResourceDetailedStatusFilters)

-- | Indicates deployment status for conformance pack in a member account.
-- When management account calls @PutOrganizationConformancePack@ action
-- for the first time, conformance pack status is created in the member
-- account. When management account calls @PutOrganizationConformancePack@
-- action for the second time, conformance pack status is updated in the
-- member account. Conformance pack status is deleted when the management
-- account deletes @OrganizationConformancePack@ and disables service
-- access for @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the conformance pack to:
--
-- -   @CREATE_SUCCESSFUL@ when conformance pack has been created in the
--     member account.
--
-- -   @CREATE_IN_PROGRESS@ when conformance pack is being created in the
--     member account.
--
-- -   @CREATE_FAILED@ when conformance pack creation has failed in the
--     member account.
--
-- -   @DELETE_FAILED@ when conformance pack deletion has failed in the
--     member account.
--
-- -   @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the
--     member account.
--
-- -   @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the
--     member account.
--
-- -   @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the
--     member account.
--
-- -   @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the
--     member account.
--
-- -   @UPDATE_FAILED@ when conformance pack deletion has failed in the
--     member account.
organizationResourceDetailedStatusFilters_status :: Lens.Lens' OrganizationResourceDetailedStatusFilters (Prelude.Maybe OrganizationResourceDetailedStatus)
organizationResourceDetailedStatusFilters_status = Lens.lens (\OrganizationResourceDetailedStatusFilters' {status} -> status) (\s@OrganizationResourceDetailedStatusFilters' {} a -> s {status = a} :: OrganizationResourceDetailedStatusFilters)

instance
  Prelude.Hashable
    OrganizationResourceDetailedStatusFilters
  where
  hashWithSalt
    _salt
    OrganizationResourceDetailedStatusFilters' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    OrganizationResourceDetailedStatusFilters
  where
  rnf OrganizationResourceDetailedStatusFilters' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf status

instance
  Data.ToJSON
    OrganizationResourceDetailedStatusFilters
  where
  toJSON OrganizationResourceDetailedStatusFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
