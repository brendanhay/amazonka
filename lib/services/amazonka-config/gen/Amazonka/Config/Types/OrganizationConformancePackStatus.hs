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
-- Module      : Amazonka.Config.Types.OrganizationConformancePackStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationConformancePackStatus where

import Amazonka.Config.Types.OrganizationResourceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the status for an organization conformance pack in an
-- organization.
--
-- /See:/ 'newOrganizationConformancePackStatus' smart constructor.
data OrganizationConformancePackStatus = OrganizationConformancePackStatus'
  { -- | An error code that is returned when organization conformance pack
    -- creation or deletion has failed in a member account.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An error message indicating that organization conformance pack creation
    -- or deletion failed due to an error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last update.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The name that you assign to organization conformance pack.
    organizationConformancePackName :: Prelude.Text,
    -- | Indicates deployment status of an organization conformance pack. When
    -- management account calls PutOrganizationConformancePack for the first
    -- time, conformance pack status is created in all the member accounts.
    -- When management account calls PutOrganizationConformancePack for the
    -- second time, conformance pack status is updated in all the member
    -- accounts. Additionally, conformance pack status is updated when one or
    -- more member accounts join or leave an organization. Conformance pack
    -- status is deleted when the management account deletes
    -- OrganizationConformancePack in all the member accounts and disables
    -- service access for @config-multiaccountsetup.amazonaws.com@.
    --
    -- Config sets the state of the conformance pack to:
    --
    -- -   @CREATE_SUCCESSFUL@ when an organization conformance pack has been
    --     successfully created in all the member accounts.
    --
    -- -   @CREATE_IN_PROGRESS@ when an organization conformance pack creation
    --     is in progress.
    --
    -- -   @CREATE_FAILED@ when an organization conformance pack creation
    --     failed in one or more member accounts within that organization.
    --
    -- -   @DELETE_FAILED@ when an organization conformance pack deletion
    --     failed in one or more member accounts within that organization.
    --
    -- -   @DELETE_IN_PROGRESS@ when an organization conformance pack deletion
    --     is in progress.
    --
    -- -   @DELETE_SUCCESSFUL@ when an organization conformance pack has been
    --     successfully deleted from all the member accounts.
    --
    -- -   @UPDATE_SUCCESSFUL@ when an organization conformance pack has been
    --     successfully updated in all the member accounts.
    --
    -- -   @UPDATE_IN_PROGRESS@ when an organization conformance pack update is
    --     in progress.
    --
    -- -   @UPDATE_FAILED@ when an organization conformance pack update failed
    --     in one or more member accounts within that organization.
    status :: OrganizationResourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationConformancePackStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'organizationConformancePackStatus_errorCode' - An error code that is returned when organization conformance pack
-- creation or deletion has failed in a member account.
--
-- 'errorMessage', 'organizationConformancePackStatus_errorMessage' - An error message indicating that organization conformance pack creation
-- or deletion failed due to an error.
--
-- 'lastUpdateTime', 'organizationConformancePackStatus_lastUpdateTime' - The timestamp of the last update.
--
-- 'organizationConformancePackName', 'organizationConformancePackStatus_organizationConformancePackName' - The name that you assign to organization conformance pack.
--
-- 'status', 'organizationConformancePackStatus_status' - Indicates deployment status of an organization conformance pack. When
-- management account calls PutOrganizationConformancePack for the first
-- time, conformance pack status is created in all the member accounts.
-- When management account calls PutOrganizationConformancePack for the
-- second time, conformance pack status is updated in all the member
-- accounts. Additionally, conformance pack status is updated when one or
-- more member accounts join or leave an organization. Conformance pack
-- status is deleted when the management account deletes
-- OrganizationConformancePack in all the member accounts and disables
-- service access for @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the conformance pack to:
--
-- -   @CREATE_SUCCESSFUL@ when an organization conformance pack has been
--     successfully created in all the member accounts.
--
-- -   @CREATE_IN_PROGRESS@ when an organization conformance pack creation
--     is in progress.
--
-- -   @CREATE_FAILED@ when an organization conformance pack creation
--     failed in one or more member accounts within that organization.
--
-- -   @DELETE_FAILED@ when an organization conformance pack deletion
--     failed in one or more member accounts within that organization.
--
-- -   @DELETE_IN_PROGRESS@ when an organization conformance pack deletion
--     is in progress.
--
-- -   @DELETE_SUCCESSFUL@ when an organization conformance pack has been
--     successfully deleted from all the member accounts.
--
-- -   @UPDATE_SUCCESSFUL@ when an organization conformance pack has been
--     successfully updated in all the member accounts.
--
-- -   @UPDATE_IN_PROGRESS@ when an organization conformance pack update is
--     in progress.
--
-- -   @UPDATE_FAILED@ when an organization conformance pack update failed
--     in one or more member accounts within that organization.
newOrganizationConformancePackStatus ::
  -- | 'organizationConformancePackName'
  Prelude.Text ->
  -- | 'status'
  OrganizationResourceStatus ->
  OrganizationConformancePackStatus
newOrganizationConformancePackStatus
  pOrganizationConformancePackName_
  pStatus_ =
    OrganizationConformancePackStatus'
      { errorCode =
          Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        lastUpdateTime = Prelude.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_,
        status = pStatus_
      }

-- | An error code that is returned when organization conformance pack
-- creation or deletion has failed in a member account.
organizationConformancePackStatus_errorCode :: Lens.Lens' OrganizationConformancePackStatus (Prelude.Maybe Prelude.Text)
organizationConformancePackStatus_errorCode = Lens.lens (\OrganizationConformancePackStatus' {errorCode} -> errorCode) (\s@OrganizationConformancePackStatus' {} a -> s {errorCode = a} :: OrganizationConformancePackStatus)

-- | An error message indicating that organization conformance pack creation
-- or deletion failed due to an error.
organizationConformancePackStatus_errorMessage :: Lens.Lens' OrganizationConformancePackStatus (Prelude.Maybe Prelude.Text)
organizationConformancePackStatus_errorMessage = Lens.lens (\OrganizationConformancePackStatus' {errorMessage} -> errorMessage) (\s@OrganizationConformancePackStatus' {} a -> s {errorMessage = a} :: OrganizationConformancePackStatus)

-- | The timestamp of the last update.
organizationConformancePackStatus_lastUpdateTime :: Lens.Lens' OrganizationConformancePackStatus (Prelude.Maybe Prelude.UTCTime)
organizationConformancePackStatus_lastUpdateTime = Lens.lens (\OrganizationConformancePackStatus' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConformancePackStatus' {} a -> s {lastUpdateTime = a} :: OrganizationConformancePackStatus) Prelude.. Lens.mapping Data._Time

-- | The name that you assign to organization conformance pack.
organizationConformancePackStatus_organizationConformancePackName :: Lens.Lens' OrganizationConformancePackStatus Prelude.Text
organizationConformancePackStatus_organizationConformancePackName = Lens.lens (\OrganizationConformancePackStatus' {organizationConformancePackName} -> organizationConformancePackName) (\s@OrganizationConformancePackStatus' {} a -> s {organizationConformancePackName = a} :: OrganizationConformancePackStatus)

-- | Indicates deployment status of an organization conformance pack. When
-- management account calls PutOrganizationConformancePack for the first
-- time, conformance pack status is created in all the member accounts.
-- When management account calls PutOrganizationConformancePack for the
-- second time, conformance pack status is updated in all the member
-- accounts. Additionally, conformance pack status is updated when one or
-- more member accounts join or leave an organization. Conformance pack
-- status is deleted when the management account deletes
-- OrganizationConformancePack in all the member accounts and disables
-- service access for @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the conformance pack to:
--
-- -   @CREATE_SUCCESSFUL@ when an organization conformance pack has been
--     successfully created in all the member accounts.
--
-- -   @CREATE_IN_PROGRESS@ when an organization conformance pack creation
--     is in progress.
--
-- -   @CREATE_FAILED@ when an organization conformance pack creation
--     failed in one or more member accounts within that organization.
--
-- -   @DELETE_FAILED@ when an organization conformance pack deletion
--     failed in one or more member accounts within that organization.
--
-- -   @DELETE_IN_PROGRESS@ when an organization conformance pack deletion
--     is in progress.
--
-- -   @DELETE_SUCCESSFUL@ when an organization conformance pack has been
--     successfully deleted from all the member accounts.
--
-- -   @UPDATE_SUCCESSFUL@ when an organization conformance pack has been
--     successfully updated in all the member accounts.
--
-- -   @UPDATE_IN_PROGRESS@ when an organization conformance pack update is
--     in progress.
--
-- -   @UPDATE_FAILED@ when an organization conformance pack update failed
--     in one or more member accounts within that organization.
organizationConformancePackStatus_status :: Lens.Lens' OrganizationConformancePackStatus OrganizationResourceStatus
organizationConformancePackStatus_status = Lens.lens (\OrganizationConformancePackStatus' {status} -> status) (\s@OrganizationConformancePackStatus' {} a -> s {status = a} :: OrganizationConformancePackStatus)

instance
  Data.FromJSON
    OrganizationConformancePackStatus
  where
  parseJSON =
    Data.withObject
      "OrganizationConformancePackStatus"
      ( \x ->
          OrganizationConformancePackStatus'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..: "OrganizationConformancePackName")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    OrganizationConformancePackStatus
  where
  hashWithSalt
    _salt
    OrganizationConformancePackStatus' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` lastUpdateTime
        `Prelude.hashWithSalt` organizationConformancePackName
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    OrganizationConformancePackStatus
  where
  rnf OrganizationConformancePackStatus' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf organizationConformancePackName
      `Prelude.seq` Prelude.rnf status
