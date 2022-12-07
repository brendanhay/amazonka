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
-- Module      : Amazonka.ECR.Types.EnhancedImageScanFinding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.EnhancedImageScanFinding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.PackageVulnerabilityDetails
import Amazonka.ECR.Types.Remediation
import Amazonka.ECR.Types.Resource
import Amazonka.ECR.Types.ScoreDetails
import qualified Amazonka.Prelude as Prelude

-- | The details of an enhanced image scan. This is returned when enhanced
-- scanning is enabled for your private registry.
--
-- /See:/ 'newEnhancedImageScanFinding' smart constructor.
data EnhancedImageScanFinding = EnhancedImageScanFinding'
  { -- | The Amazon Web Services account ID associated with the image.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The severity of the finding.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The type of the finding.
    type' :: Prelude.Maybe Prelude.Text,
    -- | An object that contains details of the Amazon Inspector score.
    scoreDetails :: Prelude.Maybe ScoreDetails,
    -- | An object that contains the details about how to remediate a finding.
    remediation :: Prelude.Maybe Remediation,
    -- | The Amazon Inspector score given to the finding.
    score :: Prelude.Maybe Prelude.Double,
    -- | The status of the finding.
    status :: Prelude.Maybe Prelude.Text,
    -- | The description of the finding.
    description :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the details of a package vulnerability finding.
    packageVulnerabilityDetails :: Prelude.Maybe PackageVulnerabilityDetails,
    -- | The title of the finding.
    title :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the finding was first observed.
    firstObservedAt :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the finding was last observed.
    lastObservedAt :: Prelude.Maybe Data.POSIX,
    -- | Contains information on the resources involved in a finding.
    resources :: Prelude.Maybe [Resource],
    -- | The Amazon Resource Number (ARN) of the finding.
    findingArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the finding was last updated at.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnhancedImageScanFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'enhancedImageScanFinding_awsAccountId' - The Amazon Web Services account ID associated with the image.
--
-- 'severity', 'enhancedImageScanFinding_severity' - The severity of the finding.
--
-- 'type'', 'enhancedImageScanFinding_type' - The type of the finding.
--
-- 'scoreDetails', 'enhancedImageScanFinding_scoreDetails' - An object that contains details of the Amazon Inspector score.
--
-- 'remediation', 'enhancedImageScanFinding_remediation' - An object that contains the details about how to remediate a finding.
--
-- 'score', 'enhancedImageScanFinding_score' - The Amazon Inspector score given to the finding.
--
-- 'status', 'enhancedImageScanFinding_status' - The status of the finding.
--
-- 'description', 'enhancedImageScanFinding_description' - The description of the finding.
--
-- 'packageVulnerabilityDetails', 'enhancedImageScanFinding_packageVulnerabilityDetails' - An object that contains the details of a package vulnerability finding.
--
-- 'title', 'enhancedImageScanFinding_title' - The title of the finding.
--
-- 'firstObservedAt', 'enhancedImageScanFinding_firstObservedAt' - The date and time that the finding was first observed.
--
-- 'lastObservedAt', 'enhancedImageScanFinding_lastObservedAt' - The date and time that the finding was last observed.
--
-- 'resources', 'enhancedImageScanFinding_resources' - Contains information on the resources involved in a finding.
--
-- 'findingArn', 'enhancedImageScanFinding_findingArn' - The Amazon Resource Number (ARN) of the finding.
--
-- 'updatedAt', 'enhancedImageScanFinding_updatedAt' - The date and time the finding was last updated at.
newEnhancedImageScanFinding ::
  EnhancedImageScanFinding
newEnhancedImageScanFinding =
  EnhancedImageScanFinding'
    { awsAccountId =
        Prelude.Nothing,
      severity = Prelude.Nothing,
      type' = Prelude.Nothing,
      scoreDetails = Prelude.Nothing,
      remediation = Prelude.Nothing,
      score = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      packageVulnerabilityDetails = Prelude.Nothing,
      title = Prelude.Nothing,
      firstObservedAt = Prelude.Nothing,
      lastObservedAt = Prelude.Nothing,
      resources = Prelude.Nothing,
      findingArn = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Web Services account ID associated with the image.
enhancedImageScanFinding_awsAccountId :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Text)
enhancedImageScanFinding_awsAccountId = Lens.lens (\EnhancedImageScanFinding' {awsAccountId} -> awsAccountId) (\s@EnhancedImageScanFinding' {} a -> s {awsAccountId = a} :: EnhancedImageScanFinding)

-- | The severity of the finding.
enhancedImageScanFinding_severity :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Text)
enhancedImageScanFinding_severity = Lens.lens (\EnhancedImageScanFinding' {severity} -> severity) (\s@EnhancedImageScanFinding' {} a -> s {severity = a} :: EnhancedImageScanFinding)

-- | The type of the finding.
enhancedImageScanFinding_type :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Text)
enhancedImageScanFinding_type = Lens.lens (\EnhancedImageScanFinding' {type'} -> type') (\s@EnhancedImageScanFinding' {} a -> s {type' = a} :: EnhancedImageScanFinding)

-- | An object that contains details of the Amazon Inspector score.
enhancedImageScanFinding_scoreDetails :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe ScoreDetails)
enhancedImageScanFinding_scoreDetails = Lens.lens (\EnhancedImageScanFinding' {scoreDetails} -> scoreDetails) (\s@EnhancedImageScanFinding' {} a -> s {scoreDetails = a} :: EnhancedImageScanFinding)

-- | An object that contains the details about how to remediate a finding.
enhancedImageScanFinding_remediation :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Remediation)
enhancedImageScanFinding_remediation = Lens.lens (\EnhancedImageScanFinding' {remediation} -> remediation) (\s@EnhancedImageScanFinding' {} a -> s {remediation = a} :: EnhancedImageScanFinding)

-- | The Amazon Inspector score given to the finding.
enhancedImageScanFinding_score :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Double)
enhancedImageScanFinding_score = Lens.lens (\EnhancedImageScanFinding' {score} -> score) (\s@EnhancedImageScanFinding' {} a -> s {score = a} :: EnhancedImageScanFinding)

-- | The status of the finding.
enhancedImageScanFinding_status :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Text)
enhancedImageScanFinding_status = Lens.lens (\EnhancedImageScanFinding' {status} -> status) (\s@EnhancedImageScanFinding' {} a -> s {status = a} :: EnhancedImageScanFinding)

-- | The description of the finding.
enhancedImageScanFinding_description :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Text)
enhancedImageScanFinding_description = Lens.lens (\EnhancedImageScanFinding' {description} -> description) (\s@EnhancedImageScanFinding' {} a -> s {description = a} :: EnhancedImageScanFinding)

-- | An object that contains the details of a package vulnerability finding.
enhancedImageScanFinding_packageVulnerabilityDetails :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe PackageVulnerabilityDetails)
enhancedImageScanFinding_packageVulnerabilityDetails = Lens.lens (\EnhancedImageScanFinding' {packageVulnerabilityDetails} -> packageVulnerabilityDetails) (\s@EnhancedImageScanFinding' {} a -> s {packageVulnerabilityDetails = a} :: EnhancedImageScanFinding)

-- | The title of the finding.
enhancedImageScanFinding_title :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Text)
enhancedImageScanFinding_title = Lens.lens (\EnhancedImageScanFinding' {title} -> title) (\s@EnhancedImageScanFinding' {} a -> s {title = a} :: EnhancedImageScanFinding)

-- | The date and time that the finding was first observed.
enhancedImageScanFinding_firstObservedAt :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.UTCTime)
enhancedImageScanFinding_firstObservedAt = Lens.lens (\EnhancedImageScanFinding' {firstObservedAt} -> firstObservedAt) (\s@EnhancedImageScanFinding' {} a -> s {firstObservedAt = a} :: EnhancedImageScanFinding) Prelude.. Lens.mapping Data._Time

-- | The date and time that the finding was last observed.
enhancedImageScanFinding_lastObservedAt :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.UTCTime)
enhancedImageScanFinding_lastObservedAt = Lens.lens (\EnhancedImageScanFinding' {lastObservedAt} -> lastObservedAt) (\s@EnhancedImageScanFinding' {} a -> s {lastObservedAt = a} :: EnhancedImageScanFinding) Prelude.. Lens.mapping Data._Time

-- | Contains information on the resources involved in a finding.
enhancedImageScanFinding_resources :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe [Resource])
enhancedImageScanFinding_resources = Lens.lens (\EnhancedImageScanFinding' {resources} -> resources) (\s@EnhancedImageScanFinding' {} a -> s {resources = a} :: EnhancedImageScanFinding) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Number (ARN) of the finding.
enhancedImageScanFinding_findingArn :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.Text)
enhancedImageScanFinding_findingArn = Lens.lens (\EnhancedImageScanFinding' {findingArn} -> findingArn) (\s@EnhancedImageScanFinding' {} a -> s {findingArn = a} :: EnhancedImageScanFinding)

-- | The date and time the finding was last updated at.
enhancedImageScanFinding_updatedAt :: Lens.Lens' EnhancedImageScanFinding (Prelude.Maybe Prelude.UTCTime)
enhancedImageScanFinding_updatedAt = Lens.lens (\EnhancedImageScanFinding' {updatedAt} -> updatedAt) (\s@EnhancedImageScanFinding' {} a -> s {updatedAt = a} :: EnhancedImageScanFinding) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON EnhancedImageScanFinding where
  parseJSON =
    Data.withObject
      "EnhancedImageScanFinding"
      ( \x ->
          EnhancedImageScanFinding'
            Prelude.<$> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "severity")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "scoreDetails")
            Prelude.<*> (x Data..:? "remediation")
            Prelude.<*> (x Data..:? "score")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "packageVulnerabilityDetails")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..:? "firstObservedAt")
            Prelude.<*> (x Data..:? "lastObservedAt")
            Prelude.<*> (x Data..:? "resources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "findingArn")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable EnhancedImageScanFinding where
  hashWithSalt _salt EnhancedImageScanFinding' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` scoreDetails
      `Prelude.hashWithSalt` remediation
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` packageVulnerabilityDetails
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` firstObservedAt
      `Prelude.hashWithSalt` lastObservedAt
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` findingArn
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData EnhancedImageScanFinding where
  rnf EnhancedImageScanFinding' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf scoreDetails
      `Prelude.seq` Prelude.rnf remediation
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf packageVulnerabilityDetails
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf firstObservedAt
      `Prelude.seq` Prelude.rnf lastObservedAt
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf findingArn
      `Prelude.seq` Prelude.rnf updatedAt
