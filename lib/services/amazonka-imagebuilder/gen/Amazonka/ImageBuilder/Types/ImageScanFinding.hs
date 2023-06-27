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
-- Module      : Amazonka.ImageBuilder.Types.ImageScanFinding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageScanFinding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.InspectorScoreDetails
import Amazonka.ImageBuilder.Types.PackageVulnerabilityDetails
import Amazonka.ImageBuilder.Types.Remediation
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a vulnerability scan finding.
--
-- /See:/ 'newImageScanFinding' smart constructor.
data ImageScanFinding = ImageScanFinding'
  { -- | The Amazon Web Services account ID that\'s associated with the finding.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The description of the finding.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the finding was first observed.
    firstObservedAt :: Prelude.Maybe Data.POSIX,
    -- | Details about whether a fix is available for any of the packages that
    -- are identified in the finding through a version update.
    fixAvailable :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image build version that\'s
    -- associated with the finding.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image pipeline that\'s associated
    -- with the finding.
    imagePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The score that Amazon Inspector assigned for the finding.
    inspectorScore :: Prelude.Maybe Prelude.Double,
    -- | An object that contains details of the Amazon Inspector score.
    inspectorScoreDetails :: Prelude.Maybe InspectorScoreDetails,
    -- | An object that contains the details of a package vulnerability finding.
    packageVulnerabilityDetails :: Prelude.Maybe PackageVulnerabilityDetails,
    -- | An object that contains the details about how to remediate the finding.
    remediation :: Prelude.Maybe Remediation,
    -- | The severity of the finding.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The title of the finding.
    title :: Prelude.Maybe Prelude.Text,
    -- | The type of the finding. Image Builder looks for findings of the type
    -- @PACKAGE_VULNERABILITY@ that apply to output images, and excludes other
    -- types.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the finding was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'imageScanFinding_awsAccountId' - The Amazon Web Services account ID that\'s associated with the finding.
--
-- 'description', 'imageScanFinding_description' - The description of the finding.
--
-- 'firstObservedAt', 'imageScanFinding_firstObservedAt' - The date and time when the finding was first observed.
--
-- 'fixAvailable', 'imageScanFinding_fixAvailable' - Details about whether a fix is available for any of the packages that
-- are identified in the finding through a version update.
--
-- 'imageBuildVersionArn', 'imageScanFinding_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the image build version that\'s
-- associated with the finding.
--
-- 'imagePipelineArn', 'imageScanFinding_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that\'s associated
-- with the finding.
--
-- 'inspectorScore', 'imageScanFinding_inspectorScore' - The score that Amazon Inspector assigned for the finding.
--
-- 'inspectorScoreDetails', 'imageScanFinding_inspectorScoreDetails' - An object that contains details of the Amazon Inspector score.
--
-- 'packageVulnerabilityDetails', 'imageScanFinding_packageVulnerabilityDetails' - An object that contains the details of a package vulnerability finding.
--
-- 'remediation', 'imageScanFinding_remediation' - An object that contains the details about how to remediate the finding.
--
-- 'severity', 'imageScanFinding_severity' - The severity of the finding.
--
-- 'title', 'imageScanFinding_title' - The title of the finding.
--
-- 'type'', 'imageScanFinding_type' - The type of the finding. Image Builder looks for findings of the type
-- @PACKAGE_VULNERABILITY@ that apply to output images, and excludes other
-- types.
--
-- 'updatedAt', 'imageScanFinding_updatedAt' - The timestamp when the finding was last updated.
newImageScanFinding ::
  ImageScanFinding
newImageScanFinding =
  ImageScanFinding'
    { awsAccountId = Prelude.Nothing,
      description = Prelude.Nothing,
      firstObservedAt = Prelude.Nothing,
      fixAvailable = Prelude.Nothing,
      imageBuildVersionArn = Prelude.Nothing,
      imagePipelineArn = Prelude.Nothing,
      inspectorScore = Prelude.Nothing,
      inspectorScoreDetails = Prelude.Nothing,
      packageVulnerabilityDetails = Prelude.Nothing,
      remediation = Prelude.Nothing,
      severity = Prelude.Nothing,
      title = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Web Services account ID that\'s associated with the finding.
imageScanFinding_awsAccountId :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_awsAccountId = Lens.lens (\ImageScanFinding' {awsAccountId} -> awsAccountId) (\s@ImageScanFinding' {} a -> s {awsAccountId = a} :: ImageScanFinding)

-- | The description of the finding.
imageScanFinding_description :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_description = Lens.lens (\ImageScanFinding' {description} -> description) (\s@ImageScanFinding' {} a -> s {description = a} :: ImageScanFinding)

-- | The date and time when the finding was first observed.
imageScanFinding_firstObservedAt :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.UTCTime)
imageScanFinding_firstObservedAt = Lens.lens (\ImageScanFinding' {firstObservedAt} -> firstObservedAt) (\s@ImageScanFinding' {} a -> s {firstObservedAt = a} :: ImageScanFinding) Prelude.. Lens.mapping Data._Time

-- | Details about whether a fix is available for any of the packages that
-- are identified in the finding through a version update.
imageScanFinding_fixAvailable :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_fixAvailable = Lens.lens (\ImageScanFinding' {fixAvailable} -> fixAvailable) (\s@ImageScanFinding' {} a -> s {fixAvailable = a} :: ImageScanFinding)

-- | The Amazon Resource Name (ARN) of the image build version that\'s
-- associated with the finding.
imageScanFinding_imageBuildVersionArn :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_imageBuildVersionArn = Lens.lens (\ImageScanFinding' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@ImageScanFinding' {} a -> s {imageBuildVersionArn = a} :: ImageScanFinding)

-- | The Amazon Resource Name (ARN) of the image pipeline that\'s associated
-- with the finding.
imageScanFinding_imagePipelineArn :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_imagePipelineArn = Lens.lens (\ImageScanFinding' {imagePipelineArn} -> imagePipelineArn) (\s@ImageScanFinding' {} a -> s {imagePipelineArn = a} :: ImageScanFinding)

-- | The score that Amazon Inspector assigned for the finding.
imageScanFinding_inspectorScore :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Double)
imageScanFinding_inspectorScore = Lens.lens (\ImageScanFinding' {inspectorScore} -> inspectorScore) (\s@ImageScanFinding' {} a -> s {inspectorScore = a} :: ImageScanFinding)

-- | An object that contains details of the Amazon Inspector score.
imageScanFinding_inspectorScoreDetails :: Lens.Lens' ImageScanFinding (Prelude.Maybe InspectorScoreDetails)
imageScanFinding_inspectorScoreDetails = Lens.lens (\ImageScanFinding' {inspectorScoreDetails} -> inspectorScoreDetails) (\s@ImageScanFinding' {} a -> s {inspectorScoreDetails = a} :: ImageScanFinding)

-- | An object that contains the details of a package vulnerability finding.
imageScanFinding_packageVulnerabilityDetails :: Lens.Lens' ImageScanFinding (Prelude.Maybe PackageVulnerabilityDetails)
imageScanFinding_packageVulnerabilityDetails = Lens.lens (\ImageScanFinding' {packageVulnerabilityDetails} -> packageVulnerabilityDetails) (\s@ImageScanFinding' {} a -> s {packageVulnerabilityDetails = a} :: ImageScanFinding)

-- | An object that contains the details about how to remediate the finding.
imageScanFinding_remediation :: Lens.Lens' ImageScanFinding (Prelude.Maybe Remediation)
imageScanFinding_remediation = Lens.lens (\ImageScanFinding' {remediation} -> remediation) (\s@ImageScanFinding' {} a -> s {remediation = a} :: ImageScanFinding)

-- | The severity of the finding.
imageScanFinding_severity :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_severity = Lens.lens (\ImageScanFinding' {severity} -> severity) (\s@ImageScanFinding' {} a -> s {severity = a} :: ImageScanFinding)

-- | The title of the finding.
imageScanFinding_title :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_title = Lens.lens (\ImageScanFinding' {title} -> title) (\s@ImageScanFinding' {} a -> s {title = a} :: ImageScanFinding)

-- | The type of the finding. Image Builder looks for findings of the type
-- @PACKAGE_VULNERABILITY@ that apply to output images, and excludes other
-- types.
imageScanFinding_type :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_type = Lens.lens (\ImageScanFinding' {type'} -> type') (\s@ImageScanFinding' {} a -> s {type' = a} :: ImageScanFinding)

-- | The timestamp when the finding was last updated.
imageScanFinding_updatedAt :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.UTCTime)
imageScanFinding_updatedAt = Lens.lens (\ImageScanFinding' {updatedAt} -> updatedAt) (\s@ImageScanFinding' {} a -> s {updatedAt = a} :: ImageScanFinding) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ImageScanFinding where
  parseJSON =
    Data.withObject
      "ImageScanFinding"
      ( \x ->
          ImageScanFinding'
            Prelude.<$> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "firstObservedAt")
            Prelude.<*> (x Data..:? "fixAvailable")
            Prelude.<*> (x Data..:? "imageBuildVersionArn")
            Prelude.<*> (x Data..:? "imagePipelineArn")
            Prelude.<*> (x Data..:? "inspectorScore")
            Prelude.<*> (x Data..:? "inspectorScoreDetails")
            Prelude.<*> (x Data..:? "packageVulnerabilityDetails")
            Prelude.<*> (x Data..:? "remediation")
            Prelude.<*> (x Data..:? "severity")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable ImageScanFinding where
  hashWithSalt _salt ImageScanFinding' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` firstObservedAt
      `Prelude.hashWithSalt` fixAvailable
      `Prelude.hashWithSalt` imageBuildVersionArn
      `Prelude.hashWithSalt` imagePipelineArn
      `Prelude.hashWithSalt` inspectorScore
      `Prelude.hashWithSalt` inspectorScoreDetails
      `Prelude.hashWithSalt` packageVulnerabilityDetails
      `Prelude.hashWithSalt` remediation
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ImageScanFinding where
  rnf ImageScanFinding' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf firstObservedAt
      `Prelude.seq` Prelude.rnf fixAvailable
      `Prelude.seq` Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf imagePipelineArn
      `Prelude.seq` Prelude.rnf inspectorScore
      `Prelude.seq` Prelude.rnf inspectorScoreDetails
      `Prelude.seq` Prelude.rnf packageVulnerabilityDetails
      `Prelude.seq` Prelude.rnf remediation
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updatedAt
