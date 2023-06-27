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
-- Module      : Amazonka.Config.Types.OrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationConformancePackDetailedStatus where

import Amazonka.Config.Types.OrganizationResourceDetailedStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Organization conformance pack creation or deletion status in each member
-- account. This includes the name of the conformance pack, the status,
-- error code and error message when the conformance pack creation or
-- deletion failed.
--
-- /See:/ 'newOrganizationConformancePackDetailedStatus' smart constructor.
data OrganizationConformancePackDetailedStatus = OrganizationConformancePackDetailedStatus'
  { -- | An error code that is returned when conformance pack creation or
    -- deletion failed in the member account.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An error message indicating that conformance pack account creation or
    -- deletion has failed due to an error in the member account.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last status update.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The 12-digit account ID of a member account.
    accountId :: Prelude.Text,
    -- | The name of conformance pack deployed in the member account.
    conformancePackName :: Prelude.Text,
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
    status :: OrganizationResourceDetailedStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationConformancePackDetailedStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'organizationConformancePackDetailedStatus_errorCode' - An error code that is returned when conformance pack creation or
-- deletion failed in the member account.
--
-- 'errorMessage', 'organizationConformancePackDetailedStatus_errorMessage' - An error message indicating that conformance pack account creation or
-- deletion has failed due to an error in the member account.
--
-- 'lastUpdateTime', 'organizationConformancePackDetailedStatus_lastUpdateTime' - The timestamp of the last status update.
--
-- 'accountId', 'organizationConformancePackDetailedStatus_accountId' - The 12-digit account ID of a member account.
--
-- 'conformancePackName', 'organizationConformancePackDetailedStatus_conformancePackName' - The name of conformance pack deployed in the member account.
--
-- 'status', 'organizationConformancePackDetailedStatus_status' - Indicates deployment status for conformance pack in a member account.
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
newOrganizationConformancePackDetailedStatus ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'conformancePackName'
  Prelude.Text ->
  -- | 'status'
  OrganizationResourceDetailedStatus ->
  OrganizationConformancePackDetailedStatus
newOrganizationConformancePackDetailedStatus
  pAccountId_
  pConformancePackName_
  pStatus_ =
    OrganizationConformancePackDetailedStatus'
      { errorCode =
          Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        lastUpdateTime = Prelude.Nothing,
        accountId = pAccountId_,
        conformancePackName =
          pConformancePackName_,
        status = pStatus_
      }

-- | An error code that is returned when conformance pack creation or
-- deletion failed in the member account.
organizationConformancePackDetailedStatus_errorCode :: Lens.Lens' OrganizationConformancePackDetailedStatus (Prelude.Maybe Prelude.Text)
organizationConformancePackDetailedStatus_errorCode = Lens.lens (\OrganizationConformancePackDetailedStatus' {errorCode} -> errorCode) (\s@OrganizationConformancePackDetailedStatus' {} a -> s {errorCode = a} :: OrganizationConformancePackDetailedStatus)

-- | An error message indicating that conformance pack account creation or
-- deletion has failed due to an error in the member account.
organizationConformancePackDetailedStatus_errorMessage :: Lens.Lens' OrganizationConformancePackDetailedStatus (Prelude.Maybe Prelude.Text)
organizationConformancePackDetailedStatus_errorMessage = Lens.lens (\OrganizationConformancePackDetailedStatus' {errorMessage} -> errorMessage) (\s@OrganizationConformancePackDetailedStatus' {} a -> s {errorMessage = a} :: OrganizationConformancePackDetailedStatus)

-- | The timestamp of the last status update.
organizationConformancePackDetailedStatus_lastUpdateTime :: Lens.Lens' OrganizationConformancePackDetailedStatus (Prelude.Maybe Prelude.UTCTime)
organizationConformancePackDetailedStatus_lastUpdateTime = Lens.lens (\OrganizationConformancePackDetailedStatus' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConformancePackDetailedStatus' {} a -> s {lastUpdateTime = a} :: OrganizationConformancePackDetailedStatus) Prelude.. Lens.mapping Data._Time

-- | The 12-digit account ID of a member account.
organizationConformancePackDetailedStatus_accountId :: Lens.Lens' OrganizationConformancePackDetailedStatus Prelude.Text
organizationConformancePackDetailedStatus_accountId = Lens.lens (\OrganizationConformancePackDetailedStatus' {accountId} -> accountId) (\s@OrganizationConformancePackDetailedStatus' {} a -> s {accountId = a} :: OrganizationConformancePackDetailedStatus)

-- | The name of conformance pack deployed in the member account.
organizationConformancePackDetailedStatus_conformancePackName :: Lens.Lens' OrganizationConformancePackDetailedStatus Prelude.Text
organizationConformancePackDetailedStatus_conformancePackName = Lens.lens (\OrganizationConformancePackDetailedStatus' {conformancePackName} -> conformancePackName) (\s@OrganizationConformancePackDetailedStatus' {} a -> s {conformancePackName = a} :: OrganizationConformancePackDetailedStatus)

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
organizationConformancePackDetailedStatus_status :: Lens.Lens' OrganizationConformancePackDetailedStatus OrganizationResourceDetailedStatus
organizationConformancePackDetailedStatus_status = Lens.lens (\OrganizationConformancePackDetailedStatus' {status} -> status) (\s@OrganizationConformancePackDetailedStatus' {} a -> s {status = a} :: OrganizationConformancePackDetailedStatus)

instance
  Data.FromJSON
    OrganizationConformancePackDetailedStatus
  where
  parseJSON =
    Data.withObject
      "OrganizationConformancePackDetailedStatus"
      ( \x ->
          OrganizationConformancePackDetailedStatus'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..: "AccountId")
            Prelude.<*> (x Data..: "ConformancePackName")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    OrganizationConformancePackDetailedStatus
  where
  hashWithSalt
    _salt
    OrganizationConformancePackDetailedStatus' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` lastUpdateTime
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` conformancePackName
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    OrganizationConformancePackDetailedStatus
  where
  rnf OrganizationConformancePackDetailedStatus' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf conformancePackName
      `Prelude.seq` Prelude.rnf status
