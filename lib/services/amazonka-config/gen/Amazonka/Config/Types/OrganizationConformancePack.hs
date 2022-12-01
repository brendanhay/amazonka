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
-- Module      : Amazonka.Config.Types.OrganizationConformancePack
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationConformancePack where

import Amazonka.Config.Types.ConformancePackInputParameter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An organization conformance pack that has information about conformance
-- packs that Config creates in member accounts.
--
-- /See:/ 'newOrganizationConformancePack' smart constructor.
data OrganizationConformancePack = OrganizationConformancePack'
  { -- | A comma-separated list of accounts excluded from organization
    -- conformance pack.
    excludedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Prelude.Maybe [ConformancePackInputParameter],
    -- | The name of the Amazon S3 bucket where Config stores conformance pack
    -- templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | Any folder structure you want to add to an Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name you assign to an organization conformance pack.
    organizationConformancePackName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of organization conformance pack.
    organizationConformancePackArn :: Prelude.Text,
    -- | Last time when organization conformation pack was updated.
    lastUpdateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludedAccounts', 'organizationConformancePack_excludedAccounts' - A comma-separated list of accounts excluded from organization
-- conformance pack.
--
-- 'conformancePackInputParameters', 'organizationConformancePack_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'deliveryS3Bucket', 'organizationConformancePack_deliveryS3Bucket' - The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional.
--
-- 'deliveryS3KeyPrefix', 'organizationConformancePack_deliveryS3KeyPrefix' - Any folder structure you want to add to an Amazon S3 bucket.
--
-- This field is optional.
--
-- 'organizationConformancePackName', 'organizationConformancePack_organizationConformancePackName' - The name you assign to an organization conformance pack.
--
-- 'organizationConformancePackArn', 'organizationConformancePack_organizationConformancePackArn' - Amazon Resource Name (ARN) of organization conformance pack.
--
-- 'lastUpdateTime', 'organizationConformancePack_lastUpdateTime' - Last time when organization conformation pack was updated.
newOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Prelude.Text ->
  -- | 'organizationConformancePackArn'
  Prelude.Text ->
  -- | 'lastUpdateTime'
  Prelude.UTCTime ->
  OrganizationConformancePack
newOrganizationConformancePack
  pOrganizationConformancePackName_
  pOrganizationConformancePackArn_
  pLastUpdateTime_ =
    OrganizationConformancePack'
      { excludedAccounts =
          Prelude.Nothing,
        conformancePackInputParameters =
          Prelude.Nothing,
        deliveryS3Bucket = Prelude.Nothing,
        deliveryS3KeyPrefix = Prelude.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_,
        organizationConformancePackArn =
          pOrganizationConformancePackArn_,
        lastUpdateTime =
          Core._Time Lens.# pLastUpdateTime_
      }

-- | A comma-separated list of accounts excluded from organization
-- conformance pack.
organizationConformancePack_excludedAccounts :: Lens.Lens' OrganizationConformancePack (Prelude.Maybe [Prelude.Text])
organizationConformancePack_excludedAccounts = Lens.lens (\OrganizationConformancePack' {excludedAccounts} -> excludedAccounts) (\s@OrganizationConformancePack' {} a -> s {excludedAccounts = a} :: OrganizationConformancePack) Prelude.. Lens.mapping Lens.coerced

-- | A list of @ConformancePackInputParameter@ objects.
organizationConformancePack_conformancePackInputParameters :: Lens.Lens' OrganizationConformancePack (Prelude.Maybe [ConformancePackInputParameter])
organizationConformancePack_conformancePackInputParameters = Lens.lens (\OrganizationConformancePack' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@OrganizationConformancePack' {} a -> s {conformancePackInputParameters = a} :: OrganizationConformancePack) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional.
organizationConformancePack_deliveryS3Bucket :: Lens.Lens' OrganizationConformancePack (Prelude.Maybe Prelude.Text)
organizationConformancePack_deliveryS3Bucket = Lens.lens (\OrganizationConformancePack' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@OrganizationConformancePack' {} a -> s {deliveryS3Bucket = a} :: OrganizationConformancePack)

-- | Any folder structure you want to add to an Amazon S3 bucket.
--
-- This field is optional.
organizationConformancePack_deliveryS3KeyPrefix :: Lens.Lens' OrganizationConformancePack (Prelude.Maybe Prelude.Text)
organizationConformancePack_deliveryS3KeyPrefix = Lens.lens (\OrganizationConformancePack' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@OrganizationConformancePack' {} a -> s {deliveryS3KeyPrefix = a} :: OrganizationConformancePack)

-- | The name you assign to an organization conformance pack.
organizationConformancePack_organizationConformancePackName :: Lens.Lens' OrganizationConformancePack Prelude.Text
organizationConformancePack_organizationConformancePackName = Lens.lens (\OrganizationConformancePack' {organizationConformancePackName} -> organizationConformancePackName) (\s@OrganizationConformancePack' {} a -> s {organizationConformancePackName = a} :: OrganizationConformancePack)

-- | Amazon Resource Name (ARN) of organization conformance pack.
organizationConformancePack_organizationConformancePackArn :: Lens.Lens' OrganizationConformancePack Prelude.Text
organizationConformancePack_organizationConformancePackArn = Lens.lens (\OrganizationConformancePack' {organizationConformancePackArn} -> organizationConformancePackArn) (\s@OrganizationConformancePack' {} a -> s {organizationConformancePackArn = a} :: OrganizationConformancePack)

-- | Last time when organization conformation pack was updated.
organizationConformancePack_lastUpdateTime :: Lens.Lens' OrganizationConformancePack Prelude.UTCTime
organizationConformancePack_lastUpdateTime = Lens.lens (\OrganizationConformancePack' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConformancePack' {} a -> s {lastUpdateTime = a} :: OrganizationConformancePack) Prelude.. Core._Time

instance Core.FromJSON OrganizationConformancePack where
  parseJSON =
    Core.withObject
      "OrganizationConformancePack"
      ( \x ->
          OrganizationConformancePack'
            Prelude.<$> ( x Core..:? "ExcludedAccounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ConformancePackInputParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DeliveryS3Bucket")
            Prelude.<*> (x Core..:? "DeliveryS3KeyPrefix")
            Prelude.<*> (x Core..: "OrganizationConformancePackName")
            Prelude.<*> (x Core..: "OrganizationConformancePackArn")
            Prelude.<*> (x Core..: "LastUpdateTime")
      )

instance Prelude.Hashable OrganizationConformancePack where
  hashWithSalt _salt OrganizationConformancePack' {..} =
    _salt `Prelude.hashWithSalt` excludedAccounts
      `Prelude.hashWithSalt` conformancePackInputParameters
      `Prelude.hashWithSalt` deliveryS3Bucket
      `Prelude.hashWithSalt` deliveryS3KeyPrefix
      `Prelude.hashWithSalt` organizationConformancePackName
      `Prelude.hashWithSalt` organizationConformancePackArn
      `Prelude.hashWithSalt` lastUpdateTime

instance Prelude.NFData OrganizationConformancePack where
  rnf OrganizationConformancePack' {..} =
    Prelude.rnf excludedAccounts
      `Prelude.seq` Prelude.rnf conformancePackInputParameters
      `Prelude.seq` Prelude.rnf deliveryS3Bucket
      `Prelude.seq` Prelude.rnf deliveryS3KeyPrefix
      `Prelude.seq` Prelude.rnf organizationConformancePackName
      `Prelude.seq` Prelude.rnf organizationConformancePackArn
      `Prelude.seq` Prelude.rnf lastUpdateTime
