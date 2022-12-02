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
-- Module      : Amazonka.Inspector2.Types.Finding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Finding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.FindingStatus
import Amazonka.Inspector2.Types.FindingType
import Amazonka.Inspector2.Types.FixAvailable
import Amazonka.Inspector2.Types.InspectorScoreDetails
import Amazonka.Inspector2.Types.NetworkReachabilityDetails
import Amazonka.Inspector2.Types.PackageVulnerabilityDetails
import Amazonka.Inspector2.Types.Remediation
import Amazonka.Inspector2.Types.Resource
import Amazonka.Inspector2.Types.Severity
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon Inspector finding.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | The Amazon Inspector score given to the finding.
    inspectorScore :: Prelude.Maybe Prelude.Double,
    -- | An object that contains the details of a package vulnerability finding.
    packageVulnerabilityDetails :: Prelude.Maybe PackageVulnerabilityDetails,
    -- | The title of the finding.
    title :: Prelude.Maybe Prelude.Text,
    -- | An object that contains details of the Amazon Inspector score.
    inspectorScoreDetails :: Prelude.Maybe InspectorScoreDetails,
    -- | An object that contains the details of a network reachability finding.
    networkReachabilityDetails :: Prelude.Maybe NetworkReachabilityDetails,
    -- | Details on whether a fix is available through a version update. This
    -- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
    -- but not all, of the packages identified in the finding have fixes
    -- available through updated versions.
    fixAvailable :: Prelude.Maybe FixAvailable,
    -- | The date and time the finding was last updated at.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Web Services account ID associated with the finding.
    awsAccountId :: Prelude.Text,
    -- | The description of the finding.
    description :: Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the finding.
    findingArn :: Prelude.Text,
    -- | The date and time that the finding was first observed.
    firstObservedAt :: Data.POSIX,
    -- | The date and time that the finding was last observed.
    lastObservedAt :: Data.POSIX,
    -- | An object that contains the details about how to remediate a finding.
    remediation :: Remediation,
    -- | Contains information on the resources involved in a finding.
    resources :: Prelude.NonEmpty Resource,
    -- | The severity of the finding.
    severity :: Severity,
    -- | The status of the finding.
    status :: FindingStatus,
    -- | The type of the finding.
    type' :: FindingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Finding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inspectorScore', 'finding_inspectorScore' - The Amazon Inspector score given to the finding.
--
-- 'packageVulnerabilityDetails', 'finding_packageVulnerabilityDetails' - An object that contains the details of a package vulnerability finding.
--
-- 'title', 'finding_title' - The title of the finding.
--
-- 'inspectorScoreDetails', 'finding_inspectorScoreDetails' - An object that contains details of the Amazon Inspector score.
--
-- 'networkReachabilityDetails', 'finding_networkReachabilityDetails' - An object that contains the details of a network reachability finding.
--
-- 'fixAvailable', 'finding_fixAvailable' - Details on whether a fix is available through a version update. This
-- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
-- but not all, of the packages identified in the finding have fixes
-- available through updated versions.
--
-- 'updatedAt', 'finding_updatedAt' - The date and time the finding was last updated at.
--
-- 'awsAccountId', 'finding_awsAccountId' - The Amazon Web Services account ID associated with the finding.
--
-- 'description', 'finding_description' - The description of the finding.
--
-- 'findingArn', 'finding_findingArn' - The Amazon Resource Number (ARN) of the finding.
--
-- 'firstObservedAt', 'finding_firstObservedAt' - The date and time that the finding was first observed.
--
-- 'lastObservedAt', 'finding_lastObservedAt' - The date and time that the finding was last observed.
--
-- 'remediation', 'finding_remediation' - An object that contains the details about how to remediate a finding.
--
-- 'resources', 'finding_resources' - Contains information on the resources involved in a finding.
--
-- 'severity', 'finding_severity' - The severity of the finding.
--
-- 'status', 'finding_status' - The status of the finding.
--
-- 'type'', 'finding_type' - The type of the finding.
newFinding ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'findingArn'
  Prelude.Text ->
  -- | 'firstObservedAt'
  Prelude.UTCTime ->
  -- | 'lastObservedAt'
  Prelude.UTCTime ->
  -- | 'remediation'
  Remediation ->
  -- | 'resources'
  Prelude.NonEmpty Resource ->
  -- | 'severity'
  Severity ->
  -- | 'status'
  FindingStatus ->
  -- | 'type''
  FindingType ->
  Finding
newFinding
  pAwsAccountId_
  pDescription_
  pFindingArn_
  pFirstObservedAt_
  pLastObservedAt_
  pRemediation_
  pResources_
  pSeverity_
  pStatus_
  pType_ =
    Finding'
      { inspectorScore = Prelude.Nothing,
        packageVulnerabilityDetails = Prelude.Nothing,
        title = Prelude.Nothing,
        inspectorScoreDetails = Prelude.Nothing,
        networkReachabilityDetails = Prelude.Nothing,
        fixAvailable = Prelude.Nothing,
        updatedAt = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        description = pDescription_,
        findingArn = pFindingArn_,
        firstObservedAt =
          Data._Time Lens.# pFirstObservedAt_,
        lastObservedAt = Data._Time Lens.# pLastObservedAt_,
        remediation = pRemediation_,
        resources = Lens.coerced Lens.# pResources_,
        severity = pSeverity_,
        status = pStatus_,
        type' = pType_
      }

-- | The Amazon Inspector score given to the finding.
finding_inspectorScore :: Lens.Lens' Finding (Prelude.Maybe Prelude.Double)
finding_inspectorScore = Lens.lens (\Finding' {inspectorScore} -> inspectorScore) (\s@Finding' {} a -> s {inspectorScore = a} :: Finding)

-- | An object that contains the details of a package vulnerability finding.
finding_packageVulnerabilityDetails :: Lens.Lens' Finding (Prelude.Maybe PackageVulnerabilityDetails)
finding_packageVulnerabilityDetails = Lens.lens (\Finding' {packageVulnerabilityDetails} -> packageVulnerabilityDetails) (\s@Finding' {} a -> s {packageVulnerabilityDetails = a} :: Finding)

-- | The title of the finding.
finding_title :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_title = Lens.lens (\Finding' {title} -> title) (\s@Finding' {} a -> s {title = a} :: Finding)

-- | An object that contains details of the Amazon Inspector score.
finding_inspectorScoreDetails :: Lens.Lens' Finding (Prelude.Maybe InspectorScoreDetails)
finding_inspectorScoreDetails = Lens.lens (\Finding' {inspectorScoreDetails} -> inspectorScoreDetails) (\s@Finding' {} a -> s {inspectorScoreDetails = a} :: Finding)

-- | An object that contains the details of a network reachability finding.
finding_networkReachabilityDetails :: Lens.Lens' Finding (Prelude.Maybe NetworkReachabilityDetails)
finding_networkReachabilityDetails = Lens.lens (\Finding' {networkReachabilityDetails} -> networkReachabilityDetails) (\s@Finding' {} a -> s {networkReachabilityDetails = a} :: Finding)

-- | Details on whether a fix is available through a version update. This
-- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
-- but not all, of the packages identified in the finding have fixes
-- available through updated versions.
finding_fixAvailable :: Lens.Lens' Finding (Prelude.Maybe FixAvailable)
finding_fixAvailable = Lens.lens (\Finding' {fixAvailable} -> fixAvailable) (\s@Finding' {} a -> s {fixAvailable = a} :: Finding)

-- | The date and time the finding was last updated at.
finding_updatedAt :: Lens.Lens' Finding (Prelude.Maybe Prelude.UTCTime)
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services account ID associated with the finding.
finding_awsAccountId :: Lens.Lens' Finding Prelude.Text
finding_awsAccountId = Lens.lens (\Finding' {awsAccountId} -> awsAccountId) (\s@Finding' {} a -> s {awsAccountId = a} :: Finding)

-- | The description of the finding.
finding_description :: Lens.Lens' Finding Prelude.Text
finding_description = Lens.lens (\Finding' {description} -> description) (\s@Finding' {} a -> s {description = a} :: Finding)

-- | The Amazon Resource Number (ARN) of the finding.
finding_findingArn :: Lens.Lens' Finding Prelude.Text
finding_findingArn = Lens.lens (\Finding' {findingArn} -> findingArn) (\s@Finding' {} a -> s {findingArn = a} :: Finding)

-- | The date and time that the finding was first observed.
finding_firstObservedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_firstObservedAt = Lens.lens (\Finding' {firstObservedAt} -> firstObservedAt) (\s@Finding' {} a -> s {firstObservedAt = a} :: Finding) Prelude.. Data._Time

-- | The date and time that the finding was last observed.
finding_lastObservedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_lastObservedAt = Lens.lens (\Finding' {lastObservedAt} -> lastObservedAt) (\s@Finding' {} a -> s {lastObservedAt = a} :: Finding) Prelude.. Data._Time

-- | An object that contains the details about how to remediate a finding.
finding_remediation :: Lens.Lens' Finding Remediation
finding_remediation = Lens.lens (\Finding' {remediation} -> remediation) (\s@Finding' {} a -> s {remediation = a} :: Finding)

-- | Contains information on the resources involved in a finding.
finding_resources :: Lens.Lens' Finding (Prelude.NonEmpty Resource)
finding_resources = Lens.lens (\Finding' {resources} -> resources) (\s@Finding' {} a -> s {resources = a} :: Finding) Prelude.. Lens.coerced

-- | The severity of the finding.
finding_severity :: Lens.Lens' Finding Severity
finding_severity = Lens.lens (\Finding' {severity} -> severity) (\s@Finding' {} a -> s {severity = a} :: Finding)

-- | The status of the finding.
finding_status :: Lens.Lens' Finding FindingStatus
finding_status = Lens.lens (\Finding' {status} -> status) (\s@Finding' {} a -> s {status = a} :: Finding)

-- | The type of the finding.
finding_type :: Lens.Lens' Finding FindingType
finding_type = Lens.lens (\Finding' {type'} -> type') (\s@Finding' {} a -> s {type' = a} :: Finding)

instance Data.FromJSON Finding where
  parseJSON =
    Data.withObject
      "Finding"
      ( \x ->
          Finding'
            Prelude.<$> (x Data..:? "inspectorScore")
            Prelude.<*> (x Data..:? "packageVulnerabilityDetails")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..:? "inspectorScoreDetails")
            Prelude.<*> (x Data..:? "networkReachabilityDetails")
            Prelude.<*> (x Data..:? "fixAvailable")
            Prelude.<*> (x Data..:? "updatedAt")
            Prelude.<*> (x Data..: "awsAccountId")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "findingArn")
            Prelude.<*> (x Data..: "firstObservedAt")
            Prelude.<*> (x Data..: "lastObservedAt")
            Prelude.<*> (x Data..: "remediation")
            Prelude.<*> (x Data..: "resources")
            Prelude.<*> (x Data..: "severity")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Finding where
  hashWithSalt _salt Finding' {..} =
    _salt `Prelude.hashWithSalt` inspectorScore
      `Prelude.hashWithSalt` packageVulnerabilityDetails
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` inspectorScoreDetails
      `Prelude.hashWithSalt` networkReachabilityDetails
      `Prelude.hashWithSalt` fixAvailable
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` findingArn
      `Prelude.hashWithSalt` firstObservedAt
      `Prelude.hashWithSalt` lastObservedAt
      `Prelude.hashWithSalt` remediation
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Finding where
  rnf Finding' {..} =
    Prelude.rnf inspectorScore
      `Prelude.seq` Prelude.rnf packageVulnerabilityDetails
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf inspectorScoreDetails
      `Prelude.seq` Prelude.rnf networkReachabilityDetails
      `Prelude.seq` Prelude.rnf fixAvailable
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf findingArn
      `Prelude.seq` Prelude.rnf firstObservedAt
      `Prelude.seq` Prelude.rnf lastObservedAt
      `Prelude.seq` Prelude.rnf remediation
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
