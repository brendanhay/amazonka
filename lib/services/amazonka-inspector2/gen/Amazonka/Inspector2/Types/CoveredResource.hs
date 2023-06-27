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
-- Module      : Amazonka.Inspector2.Types.CoveredResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CoveredResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CoverageResourceType
import Amazonka.Inspector2.Types.ResourceScanMetadata
import Amazonka.Inspector2.Types.ScanStatus
import Amazonka.Inspector2.Types.ScanType
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about a resource covered by Amazon
-- Inspector.
--
-- /See:/ 'newCoveredResource' smart constructor.
data CoveredResource = CoveredResource'
  { -- | The date and time the resource was last checked for vulnerabilities.
    lastScannedAt :: Prelude.Maybe Data.POSIX,
    -- | An object that contains details about the metadata.
    resourceMetadata :: Prelude.Maybe ResourceScanMetadata,
    -- | The status of the scan covering the resource.
    scanStatus :: Prelude.Maybe ScanStatus,
    -- | The Amazon Web Services account ID of the covered resource.
    accountId :: Prelude.Text,
    -- | The ID of the covered resource.
    resourceId :: Prelude.Text,
    -- | The type of the covered resource.
    resourceType :: CoverageResourceType,
    -- | The Amazon Inspector scan type covering the resource.
    scanType :: ScanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoveredResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastScannedAt', 'coveredResource_lastScannedAt' - The date and time the resource was last checked for vulnerabilities.
--
-- 'resourceMetadata', 'coveredResource_resourceMetadata' - An object that contains details about the metadata.
--
-- 'scanStatus', 'coveredResource_scanStatus' - The status of the scan covering the resource.
--
-- 'accountId', 'coveredResource_accountId' - The Amazon Web Services account ID of the covered resource.
--
-- 'resourceId', 'coveredResource_resourceId' - The ID of the covered resource.
--
-- 'resourceType', 'coveredResource_resourceType' - The type of the covered resource.
--
-- 'scanType', 'coveredResource_scanType' - The Amazon Inspector scan type covering the resource.
newCoveredResource ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  CoverageResourceType ->
  -- | 'scanType'
  ScanType ->
  CoveredResource
newCoveredResource
  pAccountId_
  pResourceId_
  pResourceType_
  pScanType_ =
    CoveredResource'
      { lastScannedAt = Prelude.Nothing,
        resourceMetadata = Prelude.Nothing,
        scanStatus = Prelude.Nothing,
        accountId = pAccountId_,
        resourceId = pResourceId_,
        resourceType = pResourceType_,
        scanType = pScanType_
      }

-- | The date and time the resource was last checked for vulnerabilities.
coveredResource_lastScannedAt :: Lens.Lens' CoveredResource (Prelude.Maybe Prelude.UTCTime)
coveredResource_lastScannedAt = Lens.lens (\CoveredResource' {lastScannedAt} -> lastScannedAt) (\s@CoveredResource' {} a -> s {lastScannedAt = a} :: CoveredResource) Prelude.. Lens.mapping Data._Time

-- | An object that contains details about the metadata.
coveredResource_resourceMetadata :: Lens.Lens' CoveredResource (Prelude.Maybe ResourceScanMetadata)
coveredResource_resourceMetadata = Lens.lens (\CoveredResource' {resourceMetadata} -> resourceMetadata) (\s@CoveredResource' {} a -> s {resourceMetadata = a} :: CoveredResource)

-- | The status of the scan covering the resource.
coveredResource_scanStatus :: Lens.Lens' CoveredResource (Prelude.Maybe ScanStatus)
coveredResource_scanStatus = Lens.lens (\CoveredResource' {scanStatus} -> scanStatus) (\s@CoveredResource' {} a -> s {scanStatus = a} :: CoveredResource)

-- | The Amazon Web Services account ID of the covered resource.
coveredResource_accountId :: Lens.Lens' CoveredResource Prelude.Text
coveredResource_accountId = Lens.lens (\CoveredResource' {accountId} -> accountId) (\s@CoveredResource' {} a -> s {accountId = a} :: CoveredResource)

-- | The ID of the covered resource.
coveredResource_resourceId :: Lens.Lens' CoveredResource Prelude.Text
coveredResource_resourceId = Lens.lens (\CoveredResource' {resourceId} -> resourceId) (\s@CoveredResource' {} a -> s {resourceId = a} :: CoveredResource)

-- | The type of the covered resource.
coveredResource_resourceType :: Lens.Lens' CoveredResource CoverageResourceType
coveredResource_resourceType = Lens.lens (\CoveredResource' {resourceType} -> resourceType) (\s@CoveredResource' {} a -> s {resourceType = a} :: CoveredResource)

-- | The Amazon Inspector scan type covering the resource.
coveredResource_scanType :: Lens.Lens' CoveredResource ScanType
coveredResource_scanType = Lens.lens (\CoveredResource' {scanType} -> scanType) (\s@CoveredResource' {} a -> s {scanType = a} :: CoveredResource)

instance Data.FromJSON CoveredResource where
  parseJSON =
    Data.withObject
      "CoveredResource"
      ( \x ->
          CoveredResource'
            Prelude.<$> (x Data..:? "lastScannedAt")
            Prelude.<*> (x Data..:? "resourceMetadata")
            Prelude.<*> (x Data..:? "scanStatus")
            Prelude.<*> (x Data..: "accountId")
            Prelude.<*> (x Data..: "resourceId")
            Prelude.<*> (x Data..: "resourceType")
            Prelude.<*> (x Data..: "scanType")
      )

instance Prelude.Hashable CoveredResource where
  hashWithSalt _salt CoveredResource' {..} =
    _salt
      `Prelude.hashWithSalt` lastScannedAt
      `Prelude.hashWithSalt` resourceMetadata
      `Prelude.hashWithSalt` scanStatus
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` scanType

instance Prelude.NFData CoveredResource where
  rnf CoveredResource' {..} =
    Prelude.rnf lastScannedAt
      `Prelude.seq` Prelude.rnf resourceMetadata
      `Prelude.seq` Prelude.rnf scanStatus
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf scanType
