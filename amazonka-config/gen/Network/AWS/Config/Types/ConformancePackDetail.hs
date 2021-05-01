{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns details of a conformance pack. A conformance pack is a
-- collection of AWS Config rules and remediation actions that can be
-- easily deployed in an account and a region.
--
-- /See:/ 'newConformancePackDetail' smart constructor.
data ConformancePackDetail = ConformancePackDetail'
  { -- | Last time when conformation pack update was requested.
    lastUpdateRequestedTime :: Prelude.Maybe Prelude.POSIX,
    -- | Amazon S3 bucket where AWS Config stores conformance pack templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The prefix for the Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | AWS service that created the conformance pack.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Prelude.Maybe [ConformancePackInputParameter],
    -- | Name of the conformance pack.
    conformancePackName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of the conformance pack.
    conformancePackArn :: Prelude.Text,
    -- | ID of the conformance pack.
    conformancePackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'conformancePackArn'
  Prelude.Text ->
  -- | 'conformancePackId'
  Prelude.Text ->
  ConformancePackDetail
newConformancePackDetail
  pConformancePackName_
  pConformancePackArn_
  pConformancePackId_ =
    ConformancePackDetail'
      { lastUpdateRequestedTime =
          Prelude.Nothing,
        deliveryS3Bucket = Prelude.Nothing,
        deliveryS3KeyPrefix = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        conformancePackInputParameters = Prelude.Nothing,
        conformancePackName = pConformancePackName_,
        conformancePackArn = pConformancePackArn_,
        conformancePackId = pConformancePackId_
      }

-- | Last time when conformation pack update was requested.
conformancePackDetail_lastUpdateRequestedTime :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.UTCTime)
conformancePackDetail_lastUpdateRequestedTime = Lens.lens (\ConformancePackDetail' {lastUpdateRequestedTime} -> lastUpdateRequestedTime) (\s@ConformancePackDetail' {} a -> s {lastUpdateRequestedTime = a} :: ConformancePackDetail) Prelude.. Lens.mapping Prelude._Time

-- | Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
conformancePackDetail_deliveryS3Bucket :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.Text)
conformancePackDetail_deliveryS3Bucket = Lens.lens (\ConformancePackDetail' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@ConformancePackDetail' {} a -> s {deliveryS3Bucket = a} :: ConformancePackDetail)

-- | The prefix for the Amazon S3 bucket.
--
-- This field is optional.
conformancePackDetail_deliveryS3KeyPrefix :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.Text)
conformancePackDetail_deliveryS3KeyPrefix = Lens.lens (\ConformancePackDetail' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@ConformancePackDetail' {} a -> s {deliveryS3KeyPrefix = a} :: ConformancePackDetail)

-- | AWS service that created the conformance pack.
conformancePackDetail_createdBy :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.Text)
conformancePackDetail_createdBy = Lens.lens (\ConformancePackDetail' {createdBy} -> createdBy) (\s@ConformancePackDetail' {} a -> s {createdBy = a} :: ConformancePackDetail)

-- | A list of @ConformancePackInputParameter@ objects.
conformancePackDetail_conformancePackInputParameters :: Lens.Lens' ConformancePackDetail (Prelude.Maybe [ConformancePackInputParameter])
conformancePackDetail_conformancePackInputParameters = Lens.lens (\ConformancePackDetail' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@ConformancePackDetail' {} a -> s {conformancePackInputParameters = a} :: ConformancePackDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | Name of the conformance pack.
conformancePackDetail_conformancePackName :: Lens.Lens' ConformancePackDetail Prelude.Text
conformancePackDetail_conformancePackName = Lens.lens (\ConformancePackDetail' {conformancePackName} -> conformancePackName) (\s@ConformancePackDetail' {} a -> s {conformancePackName = a} :: ConformancePackDetail)

-- | Amazon Resource Name (ARN) of the conformance pack.
conformancePackDetail_conformancePackArn :: Lens.Lens' ConformancePackDetail Prelude.Text
conformancePackDetail_conformancePackArn = Lens.lens (\ConformancePackDetail' {conformancePackArn} -> conformancePackArn) (\s@ConformancePackDetail' {} a -> s {conformancePackArn = a} :: ConformancePackDetail)

-- | ID of the conformance pack.
conformancePackDetail_conformancePackId :: Lens.Lens' ConformancePackDetail Prelude.Text
conformancePackDetail_conformancePackId = Lens.lens (\ConformancePackDetail' {conformancePackId} -> conformancePackId) (\s@ConformancePackDetail' {} a -> s {conformancePackId = a} :: ConformancePackDetail)

instance Prelude.FromJSON ConformancePackDetail where
  parseJSON =
    Prelude.withObject
      "ConformancePackDetail"
      ( \x ->
          ConformancePackDetail'
            Prelude.<$> (x Prelude..:? "LastUpdateRequestedTime")
            Prelude.<*> (x Prelude..:? "DeliveryS3Bucket")
            Prelude.<*> (x Prelude..:? "DeliveryS3KeyPrefix")
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> ( x Prelude..:? "ConformancePackInputParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "ConformancePackName")
            Prelude.<*> (x Prelude..: "ConformancePackArn")
            Prelude.<*> (x Prelude..: "ConformancePackId")
      )

instance Prelude.Hashable ConformancePackDetail

instance Prelude.NFData ConformancePackDetail
