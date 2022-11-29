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
-- Module      : Amazonka.Config.Types.ConformancePackDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackDetail where

import Amazonka.Config.Types.ConformancePackInputParameter
import Amazonka.Config.Types.TemplateSSMDocumentDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns details of a conformance pack. A conformance pack is a
-- collection of Config rules and remediation actions that can be easily
-- deployed in an account and a region.
--
-- /See:/ 'newConformancePackDetail' smart constructor.
data ConformancePackDetail = ConformancePackDetail'
  { -- | The last time a conformation pack update was requested.
    lastUpdateRequestedTime :: Prelude.Maybe Core.POSIX,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Prelude.Maybe [ConformancePackInputParameter],
    -- | The name of the Amazon S3 bucket where Config stores conformance pack
    -- templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the name or Amazon Resource Name (ARN) of the
    -- Amazon Web Services Systems Manager document (SSM document) and the
    -- version of the SSM document that is used to create a conformance pack.
    templateSSMDocumentDetails :: Prelude.Maybe TemplateSSMDocumentDetails,
    -- | The Amazon Web Services service that created the conformance pack.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The prefix for the Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Name of the conformance pack.
    conformancePackName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of the conformance pack.
    conformancePackArn :: Prelude.Text,
    -- | ID of the conformance pack.
    conformancePackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateRequestedTime', 'conformancePackDetail_lastUpdateRequestedTime' - The last time a conformation pack update was requested.
--
-- 'conformancePackInputParameters', 'conformancePackDetail_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'deliveryS3Bucket', 'conformancePackDetail_deliveryS3Bucket' - The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional.
--
-- 'templateSSMDocumentDetails', 'conformancePackDetail_templateSSMDocumentDetails' - An object that contains the name or Amazon Resource Name (ARN) of the
-- Amazon Web Services Systems Manager document (SSM document) and the
-- version of the SSM document that is used to create a conformance pack.
--
-- 'createdBy', 'conformancePackDetail_createdBy' - The Amazon Web Services service that created the conformance pack.
--
-- 'deliveryS3KeyPrefix', 'conformancePackDetail_deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- This field is optional.
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
        conformancePackInputParameters = Prelude.Nothing,
        deliveryS3Bucket = Prelude.Nothing,
        templateSSMDocumentDetails = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        deliveryS3KeyPrefix = Prelude.Nothing,
        conformancePackName = pConformancePackName_,
        conformancePackArn = pConformancePackArn_,
        conformancePackId = pConformancePackId_
      }

-- | The last time a conformation pack update was requested.
conformancePackDetail_lastUpdateRequestedTime :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.UTCTime)
conformancePackDetail_lastUpdateRequestedTime = Lens.lens (\ConformancePackDetail' {lastUpdateRequestedTime} -> lastUpdateRequestedTime) (\s@ConformancePackDetail' {} a -> s {lastUpdateRequestedTime = a} :: ConformancePackDetail) Prelude.. Lens.mapping Core._Time

-- | A list of @ConformancePackInputParameter@ objects.
conformancePackDetail_conformancePackInputParameters :: Lens.Lens' ConformancePackDetail (Prelude.Maybe [ConformancePackInputParameter])
conformancePackDetail_conformancePackInputParameters = Lens.lens (\ConformancePackDetail' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@ConformancePackDetail' {} a -> s {conformancePackInputParameters = a} :: ConformancePackDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional.
conformancePackDetail_deliveryS3Bucket :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.Text)
conformancePackDetail_deliveryS3Bucket = Lens.lens (\ConformancePackDetail' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@ConformancePackDetail' {} a -> s {deliveryS3Bucket = a} :: ConformancePackDetail)

-- | An object that contains the name or Amazon Resource Name (ARN) of the
-- Amazon Web Services Systems Manager document (SSM document) and the
-- version of the SSM document that is used to create a conformance pack.
conformancePackDetail_templateSSMDocumentDetails :: Lens.Lens' ConformancePackDetail (Prelude.Maybe TemplateSSMDocumentDetails)
conformancePackDetail_templateSSMDocumentDetails = Lens.lens (\ConformancePackDetail' {templateSSMDocumentDetails} -> templateSSMDocumentDetails) (\s@ConformancePackDetail' {} a -> s {templateSSMDocumentDetails = a} :: ConformancePackDetail)

-- | The Amazon Web Services service that created the conformance pack.
conformancePackDetail_createdBy :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.Text)
conformancePackDetail_createdBy = Lens.lens (\ConformancePackDetail' {createdBy} -> createdBy) (\s@ConformancePackDetail' {} a -> s {createdBy = a} :: ConformancePackDetail)

-- | The prefix for the Amazon S3 bucket.
--
-- This field is optional.
conformancePackDetail_deliveryS3KeyPrefix :: Lens.Lens' ConformancePackDetail (Prelude.Maybe Prelude.Text)
conformancePackDetail_deliveryS3KeyPrefix = Lens.lens (\ConformancePackDetail' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@ConformancePackDetail' {} a -> s {deliveryS3KeyPrefix = a} :: ConformancePackDetail)

-- | Name of the conformance pack.
conformancePackDetail_conformancePackName :: Lens.Lens' ConformancePackDetail Prelude.Text
conformancePackDetail_conformancePackName = Lens.lens (\ConformancePackDetail' {conformancePackName} -> conformancePackName) (\s@ConformancePackDetail' {} a -> s {conformancePackName = a} :: ConformancePackDetail)

-- | Amazon Resource Name (ARN) of the conformance pack.
conformancePackDetail_conformancePackArn :: Lens.Lens' ConformancePackDetail Prelude.Text
conformancePackDetail_conformancePackArn = Lens.lens (\ConformancePackDetail' {conformancePackArn} -> conformancePackArn) (\s@ConformancePackDetail' {} a -> s {conformancePackArn = a} :: ConformancePackDetail)

-- | ID of the conformance pack.
conformancePackDetail_conformancePackId :: Lens.Lens' ConformancePackDetail Prelude.Text
conformancePackDetail_conformancePackId = Lens.lens (\ConformancePackDetail' {conformancePackId} -> conformancePackId) (\s@ConformancePackDetail' {} a -> s {conformancePackId = a} :: ConformancePackDetail)

instance Core.FromJSON ConformancePackDetail where
  parseJSON =
    Core.withObject
      "ConformancePackDetail"
      ( \x ->
          ConformancePackDetail'
            Prelude.<$> (x Core..:? "LastUpdateRequestedTime")
            Prelude.<*> ( x Core..:? "ConformancePackInputParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DeliveryS3Bucket")
            Prelude.<*> (x Core..:? "TemplateSSMDocumentDetails")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "DeliveryS3KeyPrefix")
            Prelude.<*> (x Core..: "ConformancePackName")
            Prelude.<*> (x Core..: "ConformancePackArn")
            Prelude.<*> (x Core..: "ConformancePackId")
      )

instance Prelude.Hashable ConformancePackDetail where
  hashWithSalt _salt ConformancePackDetail' {..} =
    _salt
      `Prelude.hashWithSalt` lastUpdateRequestedTime
      `Prelude.hashWithSalt` conformancePackInputParameters
      `Prelude.hashWithSalt` deliveryS3Bucket
      `Prelude.hashWithSalt` templateSSMDocumentDetails
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` deliveryS3KeyPrefix
      `Prelude.hashWithSalt` conformancePackName
      `Prelude.hashWithSalt` conformancePackArn
      `Prelude.hashWithSalt` conformancePackId

instance Prelude.NFData ConformancePackDetail where
  rnf ConformancePackDetail' {..} =
    Prelude.rnf lastUpdateRequestedTime
      `Prelude.seq` Prelude.rnf conformancePackInputParameters
      `Prelude.seq` Prelude.rnf deliveryS3Bucket
      `Prelude.seq` Prelude.rnf templateSSMDocumentDetails
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf deliveryS3KeyPrefix
      `Prelude.seq` Prelude.rnf conformancePackName
      `Prelude.seq` Prelude.rnf conformancePackArn
      `Prelude.seq` Prelude.rnf conformancePackId
