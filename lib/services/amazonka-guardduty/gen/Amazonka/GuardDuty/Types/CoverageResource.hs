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
-- Module      : Amazonka.GuardDuty.Types.CoverageResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CoverageResourceDetails
import Amazonka.GuardDuty.Types.CoverageStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the resource of the GuardDuty account.
--
-- /See:/ 'newCoverageResource' smart constructor.
data CoverageResource = CoverageResource'
  { -- | The unique ID of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Represents the status of the EKS cluster coverage.
    coverageStatus :: Prelude.Maybe CoverageStatus,
    -- | The unique ID of the GuardDuty detector associated with the resource.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | Represents the reason why a coverage status was @UNHEALTHY@ for the EKS
    -- cluster.
    issue :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource for which the coverage statistics are
    -- retrieved.
    resourceDetails :: Prelude.Maybe CoverageResourceDetails,
    -- | The unique ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the coverage details for the resource were last
    -- updated. This is in UTC format.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'coverageResource_accountId' - The unique ID of the Amazon Web Services account.
--
-- 'coverageStatus', 'coverageResource_coverageStatus' - Represents the status of the EKS cluster coverage.
--
-- 'detectorId', 'coverageResource_detectorId' - The unique ID of the GuardDuty detector associated with the resource.
--
-- 'issue', 'coverageResource_issue' - Represents the reason why a coverage status was @UNHEALTHY@ for the EKS
-- cluster.
--
-- 'resourceDetails', 'coverageResource_resourceDetails' - Information about the resource for which the coverage statistics are
-- retrieved.
--
-- 'resourceId', 'coverageResource_resourceId' - The unique ID of the resource.
--
-- 'updatedAt', 'coverageResource_updatedAt' - The timestamp at which the coverage details for the resource were last
-- updated. This is in UTC format.
newCoverageResource ::
  CoverageResource
newCoverageResource =
  CoverageResource'
    { accountId = Prelude.Nothing,
      coverageStatus = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      issue = Prelude.Nothing,
      resourceDetails = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The unique ID of the Amazon Web Services account.
coverageResource_accountId :: Lens.Lens' CoverageResource (Prelude.Maybe Prelude.Text)
coverageResource_accountId = Lens.lens (\CoverageResource' {accountId} -> accountId) (\s@CoverageResource' {} a -> s {accountId = a} :: CoverageResource)

-- | Represents the status of the EKS cluster coverage.
coverageResource_coverageStatus :: Lens.Lens' CoverageResource (Prelude.Maybe CoverageStatus)
coverageResource_coverageStatus = Lens.lens (\CoverageResource' {coverageStatus} -> coverageStatus) (\s@CoverageResource' {} a -> s {coverageStatus = a} :: CoverageResource)

-- | The unique ID of the GuardDuty detector associated with the resource.
coverageResource_detectorId :: Lens.Lens' CoverageResource (Prelude.Maybe Prelude.Text)
coverageResource_detectorId = Lens.lens (\CoverageResource' {detectorId} -> detectorId) (\s@CoverageResource' {} a -> s {detectorId = a} :: CoverageResource)

-- | Represents the reason why a coverage status was @UNHEALTHY@ for the EKS
-- cluster.
coverageResource_issue :: Lens.Lens' CoverageResource (Prelude.Maybe Prelude.Text)
coverageResource_issue = Lens.lens (\CoverageResource' {issue} -> issue) (\s@CoverageResource' {} a -> s {issue = a} :: CoverageResource)

-- | Information about the resource for which the coverage statistics are
-- retrieved.
coverageResource_resourceDetails :: Lens.Lens' CoverageResource (Prelude.Maybe CoverageResourceDetails)
coverageResource_resourceDetails = Lens.lens (\CoverageResource' {resourceDetails} -> resourceDetails) (\s@CoverageResource' {} a -> s {resourceDetails = a} :: CoverageResource)

-- | The unique ID of the resource.
coverageResource_resourceId :: Lens.Lens' CoverageResource (Prelude.Maybe Prelude.Text)
coverageResource_resourceId = Lens.lens (\CoverageResource' {resourceId} -> resourceId) (\s@CoverageResource' {} a -> s {resourceId = a} :: CoverageResource)

-- | The timestamp at which the coverage details for the resource were last
-- updated. This is in UTC format.
coverageResource_updatedAt :: Lens.Lens' CoverageResource (Prelude.Maybe Prelude.UTCTime)
coverageResource_updatedAt = Lens.lens (\CoverageResource' {updatedAt} -> updatedAt) (\s@CoverageResource' {} a -> s {updatedAt = a} :: CoverageResource) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CoverageResource where
  parseJSON =
    Data.withObject
      "CoverageResource"
      ( \x ->
          CoverageResource'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "coverageStatus")
            Prelude.<*> (x Data..:? "detectorId")
            Prelude.<*> (x Data..:? "issue")
            Prelude.<*> (x Data..:? "resourceDetails")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable CoverageResource where
  hashWithSalt _salt CoverageResource' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` coverageStatus
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` issue
      `Prelude.hashWithSalt` resourceDetails
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData CoverageResource where
  rnf CoverageResource' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf coverageStatus
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf issue
      `Prelude.seq` Prelude.rnf resourceDetails
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf updatedAt
