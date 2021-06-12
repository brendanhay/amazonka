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
-- Module      : Network.AWS.Config.Types.ConformancePackDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackDetail where

import Network.AWS.Config.Types.ConformancePackInputParameter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns details of a conformance pack. A conformance pack is a
-- collection of AWS Config rules and remediation actions that can be
-- easily deployed in an account and a region.
--
-- /See:/ 'newConformancePackDetail' smart constructor.
data ConformancePackDetail = ConformancePackDetail'
  { -- | Last time when conformation pack update was requested.
    lastUpdateRequestedTime :: Core.Maybe Core.POSIX,
    -- | Amazon S3 bucket where AWS Config stores conformance pack templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Core.Maybe Core.Text,
    -- | The prefix for the Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Core.Maybe Core.Text,
    -- | AWS service that created the conformance pack.
    createdBy :: Core.Maybe Core.Text,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Core.Maybe [ConformancePackInputParameter],
    -- | Name of the conformance pack.
    conformancePackName :: Core.Text,
    -- | Amazon Resource Name (ARN) of the conformance pack.
    conformancePackArn :: Core.Text,
    -- | ID of the conformance pack.
    conformancePackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConformancePackDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateRequestedTime', 'conformancePackDetail_lastUpdateRequestedTime' - Last time when conformation pack update was requested.
--
-- 'deliveryS3Bucket', 'conformancePackDetail_deliveryS3Bucket' - Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
--
-- 'deliveryS3KeyPrefix', 'conformancePackDetail_deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- This field is optional.
--
-- 'createdBy', 'conformancePackDetail_createdBy' - AWS service that created the conformance pack.
--
-- 'conformancePackInputParameters', 'conformancePackDetail_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'conformancePackName', 'conformancePackDetail_conformancePackName' - Name of the conformance pack.
--
-- 'conformancePackArn', 'conformancePackDetail_conformancePackArn' - Amazon Resource Name (ARN) of the conformance pack.
--
-- 'conformancePackId', 'conformancePackDetail_conformancePackId' - ID of the conformance pack.
newConformancePackDetail ::
  -- | 'conformancePackName'
  Core.Text ->
  -- | 'conformancePackArn'
  Core.Text ->
  -- | 'conformancePackId'
  Core.Text ->
  ConformancePackDetail
newConformancePackDetail
  pConformancePackName_
  pConformancePackArn_
  pConformancePackId_ =
    ConformancePackDetail'
      { lastUpdateRequestedTime =
          Core.Nothing,
        deliveryS3Bucket = Core.Nothing,
        deliveryS3KeyPrefix = Core.Nothing,
        createdBy = Core.Nothing,
        conformancePackInputParameters = Core.Nothing,
        conformancePackName = pConformancePackName_,
        conformancePackArn = pConformancePackArn_,
        conformancePackId = pConformancePackId_
      }

-- | Last time when conformation pack update was requested.
conformancePackDetail_lastUpdateRequestedTime :: Lens.Lens' ConformancePackDetail (Core.Maybe Core.UTCTime)
conformancePackDetail_lastUpdateRequestedTime = Lens.lens (\ConformancePackDetail' {lastUpdateRequestedTime} -> lastUpdateRequestedTime) (\s@ConformancePackDetail' {} a -> s {lastUpdateRequestedTime = a} :: ConformancePackDetail) Core.. Lens.mapping Core._Time

-- | Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
conformancePackDetail_deliveryS3Bucket :: Lens.Lens' ConformancePackDetail (Core.Maybe Core.Text)
conformancePackDetail_deliveryS3Bucket = Lens.lens (\ConformancePackDetail' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@ConformancePackDetail' {} a -> s {deliveryS3Bucket = a} :: ConformancePackDetail)

-- | The prefix for the Amazon S3 bucket.
--
-- This field is optional.
conformancePackDetail_deliveryS3KeyPrefix :: Lens.Lens' ConformancePackDetail (Core.Maybe Core.Text)
conformancePackDetail_deliveryS3KeyPrefix = Lens.lens (\ConformancePackDetail' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@ConformancePackDetail' {} a -> s {deliveryS3KeyPrefix = a} :: ConformancePackDetail)

-- | AWS service that created the conformance pack.
conformancePackDetail_createdBy :: Lens.Lens' ConformancePackDetail (Core.Maybe Core.Text)
conformancePackDetail_createdBy = Lens.lens (\ConformancePackDetail' {createdBy} -> createdBy) (\s@ConformancePackDetail' {} a -> s {createdBy = a} :: ConformancePackDetail)

-- | A list of @ConformancePackInputParameter@ objects.
conformancePackDetail_conformancePackInputParameters :: Lens.Lens' ConformancePackDetail (Core.Maybe [ConformancePackInputParameter])
conformancePackDetail_conformancePackInputParameters = Lens.lens (\ConformancePackDetail' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@ConformancePackDetail' {} a -> s {conformancePackInputParameters = a} :: ConformancePackDetail) Core.. Lens.mapping Lens._Coerce

-- | Name of the conformance pack.
conformancePackDetail_conformancePackName :: Lens.Lens' ConformancePackDetail Core.Text
conformancePackDetail_conformancePackName = Lens.lens (\ConformancePackDetail' {conformancePackName} -> conformancePackName) (\s@ConformancePackDetail' {} a -> s {conformancePackName = a} :: ConformancePackDetail)

-- | Amazon Resource Name (ARN) of the conformance pack.
conformancePackDetail_conformancePackArn :: Lens.Lens' ConformancePackDetail Core.Text
conformancePackDetail_conformancePackArn = Lens.lens (\ConformancePackDetail' {conformancePackArn} -> conformancePackArn) (\s@ConformancePackDetail' {} a -> s {conformancePackArn = a} :: ConformancePackDetail)

-- | ID of the conformance pack.
conformancePackDetail_conformancePackId :: Lens.Lens' ConformancePackDetail Core.Text
conformancePackDetail_conformancePackId = Lens.lens (\ConformancePackDetail' {conformancePackId} -> conformancePackId) (\s@ConformancePackDetail' {} a -> s {conformancePackId = a} :: ConformancePackDetail)

instance Core.FromJSON ConformancePackDetail where
  parseJSON =
    Core.withObject
      "ConformancePackDetail"
      ( \x ->
          ConformancePackDetail'
            Core.<$> (x Core..:? "LastUpdateRequestedTime")
            Core.<*> (x Core..:? "DeliveryS3Bucket")
            Core.<*> (x Core..:? "DeliveryS3KeyPrefix")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> ( x Core..:? "ConformancePackInputParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..: "ConformancePackName")
            Core.<*> (x Core..: "ConformancePackArn")
            Core.<*> (x Core..: "ConformancePackId")
      )

instance Core.Hashable ConformancePackDetail

instance Core.NFData ConformancePackDetail
