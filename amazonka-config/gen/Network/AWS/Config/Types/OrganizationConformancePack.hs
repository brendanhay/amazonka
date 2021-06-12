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
-- Module      : Network.AWS.Config.Types.OrganizationConformancePack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePack where

import Network.AWS.Config.Types.ConformancePackInputParameter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An organization conformance pack that has information about conformance
-- packs that AWS Config creates in member accounts.
--
-- /See:/ 'newOrganizationConformancePack' smart constructor.
data OrganizationConformancePack = OrganizationConformancePack'
  { -- | Amazon S3 bucket where AWS Config stores conformance pack templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Core.Maybe Core.Text,
    -- | Any folder structure you want to add to an Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Core.Maybe Core.Text,
    -- | A comma-separated list of accounts excluded from organization
    -- conformance pack.
    excludedAccounts :: Core.Maybe [Core.Text],
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Core.Maybe [ConformancePackInputParameter],
    -- | The name you assign to an organization conformance pack.
    organizationConformancePackName :: Core.Text,
    -- | Amazon Resource Name (ARN) of organization conformance pack.
    organizationConformancePackArn :: Core.Text,
    -- | Last time when organization conformation pack was updated.
    lastUpdateTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrganizationConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryS3Bucket', 'organizationConformancePack_deliveryS3Bucket' - Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
--
-- 'deliveryS3KeyPrefix', 'organizationConformancePack_deliveryS3KeyPrefix' - Any folder structure you want to add to an Amazon S3 bucket.
--
-- This field is optional.
--
-- 'excludedAccounts', 'organizationConformancePack_excludedAccounts' - A comma-separated list of accounts excluded from organization
-- conformance pack.
--
-- 'conformancePackInputParameters', 'organizationConformancePack_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'organizationConformancePackName', 'organizationConformancePack_organizationConformancePackName' - The name you assign to an organization conformance pack.
--
-- 'organizationConformancePackArn', 'organizationConformancePack_organizationConformancePackArn' - Amazon Resource Name (ARN) of organization conformance pack.
--
-- 'lastUpdateTime', 'organizationConformancePack_lastUpdateTime' - Last time when organization conformation pack was updated.
newOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Core.Text ->
  -- | 'organizationConformancePackArn'
  Core.Text ->
  -- | 'lastUpdateTime'
  Core.UTCTime ->
  OrganizationConformancePack
newOrganizationConformancePack
  pOrganizationConformancePackName_
  pOrganizationConformancePackArn_
  pLastUpdateTime_ =
    OrganizationConformancePack'
      { deliveryS3Bucket =
          Core.Nothing,
        deliveryS3KeyPrefix = Core.Nothing,
        excludedAccounts = Core.Nothing,
        conformancePackInputParameters = Core.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_,
        organizationConformancePackArn =
          pOrganizationConformancePackArn_,
        lastUpdateTime =
          Core._Time Lens.# pLastUpdateTime_
      }

-- | Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
organizationConformancePack_deliveryS3Bucket :: Lens.Lens' OrganizationConformancePack (Core.Maybe Core.Text)
organizationConformancePack_deliveryS3Bucket = Lens.lens (\OrganizationConformancePack' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@OrganizationConformancePack' {} a -> s {deliveryS3Bucket = a} :: OrganizationConformancePack)

-- | Any folder structure you want to add to an Amazon S3 bucket.
--
-- This field is optional.
organizationConformancePack_deliveryS3KeyPrefix :: Lens.Lens' OrganizationConformancePack (Core.Maybe Core.Text)
organizationConformancePack_deliveryS3KeyPrefix = Lens.lens (\OrganizationConformancePack' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@OrganizationConformancePack' {} a -> s {deliveryS3KeyPrefix = a} :: OrganizationConformancePack)

-- | A comma-separated list of accounts excluded from organization
-- conformance pack.
organizationConformancePack_excludedAccounts :: Lens.Lens' OrganizationConformancePack (Core.Maybe [Core.Text])
organizationConformancePack_excludedAccounts = Lens.lens (\OrganizationConformancePack' {excludedAccounts} -> excludedAccounts) (\s@OrganizationConformancePack' {} a -> s {excludedAccounts = a} :: OrganizationConformancePack) Core.. Lens.mapping Lens._Coerce

-- | A list of @ConformancePackInputParameter@ objects.
organizationConformancePack_conformancePackInputParameters :: Lens.Lens' OrganizationConformancePack (Core.Maybe [ConformancePackInputParameter])
organizationConformancePack_conformancePackInputParameters = Lens.lens (\OrganizationConformancePack' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@OrganizationConformancePack' {} a -> s {conformancePackInputParameters = a} :: OrganizationConformancePack) Core.. Lens.mapping Lens._Coerce

-- | The name you assign to an organization conformance pack.
organizationConformancePack_organizationConformancePackName :: Lens.Lens' OrganizationConformancePack Core.Text
organizationConformancePack_organizationConformancePackName = Lens.lens (\OrganizationConformancePack' {organizationConformancePackName} -> organizationConformancePackName) (\s@OrganizationConformancePack' {} a -> s {organizationConformancePackName = a} :: OrganizationConformancePack)

-- | Amazon Resource Name (ARN) of organization conformance pack.
organizationConformancePack_organizationConformancePackArn :: Lens.Lens' OrganizationConformancePack Core.Text
organizationConformancePack_organizationConformancePackArn = Lens.lens (\OrganizationConformancePack' {organizationConformancePackArn} -> organizationConformancePackArn) (\s@OrganizationConformancePack' {} a -> s {organizationConformancePackArn = a} :: OrganizationConformancePack)

-- | Last time when organization conformation pack was updated.
organizationConformancePack_lastUpdateTime :: Lens.Lens' OrganizationConformancePack Core.UTCTime
organizationConformancePack_lastUpdateTime = Lens.lens (\OrganizationConformancePack' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConformancePack' {} a -> s {lastUpdateTime = a} :: OrganizationConformancePack) Core.. Core._Time

instance Core.FromJSON OrganizationConformancePack where
  parseJSON =
    Core.withObject
      "OrganizationConformancePack"
      ( \x ->
          OrganizationConformancePack'
            Core.<$> (x Core..:? "DeliveryS3Bucket")
            Core.<*> (x Core..:? "DeliveryS3KeyPrefix")
            Core.<*> (x Core..:? "ExcludedAccounts" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "ConformancePackInputParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..: "OrganizationConformancePackName")
            Core.<*> (x Core..: "OrganizationConformancePackArn")
            Core.<*> (x Core..: "LastUpdateTime")
      )

instance Core.Hashable OrganizationConformancePack

instance Core.NFData OrganizationConformancePack
