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
-- Module      : Network.AWS.SSM.Types.AssociationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.AssociationStatus
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Describes the parameters for a document.
--
-- /See:/ 'newAssociationDescription' smart constructor.
data AssociationDescription = AssociationDescription'
  { -- | The number of errors that are allowed before the system stops sending
    -- requests to run the association on additional targets. You can specify
    -- either an absolute number of errors, for example 10, or a percentage of
    -- the target set, for example 10%. If you specify 3, for example, the
    -- system stops sending requests when the fourth error is received. If you
    -- specify 0, then the system stops sending requests after the first error
    -- is returned. If you run an association on 50 instances and set MaxError
    -- to 10%, then the system stops sending the request when the sixth error
    -- is received.
    --
    -- Executions that are already running an association when MaxErrors is
    -- reached are allowed to complete, but some of these executions may fail
    -- as well. If you need to ensure that there won\'t be more than max-errors
    -- failed executions, set MaxConcurrency to 1 so that executions proceed
    -- one at a time.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The association status.
    status :: Prelude.Maybe AssociationStatus,
    -- | The date on which the association was last run.
    lastExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The date when the association was last updated.
    lastUpdateAssociationDate :: Prelude.Maybe Core.POSIX,
    -- | The severity level that is assigned to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
    -- | Information about the association.
    overview :: Prelude.Maybe AssociationOverview,
    -- | Specify the target for the association. This target is required for
    -- associations that use an Automation document and target resources by
    -- using rate controls.
    automationTargetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The instances targeted by the request.
    targets :: Prelude.Maybe [Target],
    -- | The combination of AWS Regions and AWS accounts where you want to run
    -- the association.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | A cron expression that specifies a schedule when the association runs.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of the Systems Manager document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The date when the association was made.
    date :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of targets allowed to run the association at the same
    -- time. You can specify a number, for example 10, or a percentage of the
    -- target set, for example 10%. The default value is 100%, which means all
    -- targets run the association at the same time.
    --
    -- If a new instance starts and attempts to run an association while
    -- Systems Manager is running MaxConcurrency associations, the association
    -- is allowed to run. During the next association interval, the new
    -- instance will process its association within the limit specified for
    -- MaxConcurrency.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The association name.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The last date on which the association was successfully run.
    lastSuccessfulExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameters for a document.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | An S3 bucket where you want to store the output details of the request.
    outputLocation :: Prelude.Maybe InstanceAssociationOutputLocation,
    -- | By default, when you create a new associations, the system runs it
    -- immediately after it is created and then according to the schedule you
    -- specified. Specify this option if you don\'t want an association to run
    -- immediately after you create it. This parameter is not supported for
    -- rate expressions.
    applyOnlyAtCronInterval :: Prelude.Maybe Prelude.Bool,
    -- | The mode for generating association compliance. You can specify @AUTO@
    -- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
    -- association execution to determine the compliance status. If the
    -- association execution runs successfully, then the association is
    -- @COMPLIANT@. If the association execution doesn\'t run successfully, the
    -- association is @NON-COMPLIANT@.
    --
    -- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
    -- for the PutComplianceItems API action. In this case, compliance data is
    -- not managed by State Manager. It is managed by your direct call to the
    -- PutComplianceItems API action.
    --
    -- By default, all associations use @AUTO@ mode.
    syncCompliance :: Prelude.Maybe AssociationSyncCompliance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'associationDescription_maxErrors' - The number of errors that are allowed before the system stops sending
-- requests to run the association on additional targets. You can specify
-- either an absolute number of errors, for example 10, or a percentage of
-- the target set, for example 10%. If you specify 3, for example, the
-- system stops sending requests when the fourth error is received. If you
-- specify 0, then the system stops sending requests after the first error
-- is returned. If you run an association on 50 instances and set MaxError
-- to 10%, then the system stops sending the request when the sixth error
-- is received.
--
-- Executions that are already running an association when MaxErrors is
-- reached are allowed to complete, but some of these executions may fail
-- as well. If you need to ensure that there won\'t be more than max-errors
-- failed executions, set MaxConcurrency to 1 so that executions proceed
-- one at a time.
--
-- 'status', 'associationDescription_status' - The association status.
--
-- 'lastExecutionDate', 'associationDescription_lastExecutionDate' - The date on which the association was last run.
--
-- 'instanceId', 'associationDescription_instanceId' - The ID of the instance.
--
-- 'lastUpdateAssociationDate', 'associationDescription_lastUpdateAssociationDate' - The date when the association was last updated.
--
-- 'complianceSeverity', 'associationDescription_complianceSeverity' - The severity level that is assigned to the association.
--
-- 'overview', 'associationDescription_overview' - Information about the association.
--
-- 'automationTargetParameterName', 'associationDescription_automationTargetParameterName' - Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
--
-- 'targets', 'associationDescription_targets' - The instances targeted by the request.
--
-- 'targetLocations', 'associationDescription_targetLocations' - The combination of AWS Regions and AWS accounts where you want to run
-- the association.
--
-- 'scheduleExpression', 'associationDescription_scheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- 'name', 'associationDescription_name' - The name of the Systems Manager document.
--
-- 'associationId', 'associationDescription_associationId' - The association ID.
--
-- 'date', 'associationDescription_date' - The date when the association was made.
--
-- 'maxConcurrency', 'associationDescription_maxConcurrency' - The maximum number of targets allowed to run the association at the same
-- time. You can specify a number, for example 10, or a percentage of the
-- target set, for example 10%. The default value is 100%, which means all
-- targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while
-- Systems Manager is running MaxConcurrency associations, the association
-- is allowed to run. During the next association interval, the new
-- instance will process its association within the limit specified for
-- MaxConcurrency.
--
-- 'associationName', 'associationDescription_associationName' - The association name.
--
-- 'associationVersion', 'associationDescription_associationVersion' - The association version.
--
-- 'lastSuccessfulExecutionDate', 'associationDescription_lastSuccessfulExecutionDate' - The last date on which the association was successfully run.
--
-- 'documentVersion', 'associationDescription_documentVersion' - The document version.
--
-- 'parameters', 'associationDescription_parameters' - A description of the parameters for a document.
--
-- 'outputLocation', 'associationDescription_outputLocation' - An S3 bucket where you want to store the output details of the request.
--
-- 'applyOnlyAtCronInterval', 'associationDescription_applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter is not supported for
-- rate expressions.
--
-- 'syncCompliance', 'associationDescription_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
-- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
-- association execution to determine the compliance status. If the
-- association execution runs successfully, then the association is
-- @COMPLIANT@. If the association execution doesn\'t run successfully, the
-- association is @NON-COMPLIANT@.
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
-- for the PutComplianceItems API action. In this case, compliance data is
-- not managed by State Manager. It is managed by your direct call to the
-- PutComplianceItems API action.
--
-- By default, all associations use @AUTO@ mode.
newAssociationDescription ::
  AssociationDescription
newAssociationDescription =
  AssociationDescription'
    { maxErrors =
        Prelude.Nothing,
      status = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      lastUpdateAssociationDate = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      overview = Prelude.Nothing,
      automationTargetParameterName = Prelude.Nothing,
      targets = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      name = Prelude.Nothing,
      associationId = Prelude.Nothing,
      date = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      associationName = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      lastSuccessfulExecutionDate = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      applyOnlyAtCronInterval = Prelude.Nothing,
      syncCompliance = Prelude.Nothing
    }

-- | The number of errors that are allowed before the system stops sending
-- requests to run the association on additional targets. You can specify
-- either an absolute number of errors, for example 10, or a percentage of
-- the target set, for example 10%. If you specify 3, for example, the
-- system stops sending requests when the fourth error is received. If you
-- specify 0, then the system stops sending requests after the first error
-- is returned. If you run an association on 50 instances and set MaxError
-- to 10%, then the system stops sending the request when the sixth error
-- is received.
--
-- Executions that are already running an association when MaxErrors is
-- reached are allowed to complete, but some of these executions may fail
-- as well. If you need to ensure that there won\'t be more than max-errors
-- failed executions, set MaxConcurrency to 1 so that executions proceed
-- one at a time.
associationDescription_maxErrors :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_maxErrors = Lens.lens (\AssociationDescription' {maxErrors} -> maxErrors) (\s@AssociationDescription' {} a -> s {maxErrors = a} :: AssociationDescription)

-- | The association status.
associationDescription_status :: Lens.Lens' AssociationDescription (Prelude.Maybe AssociationStatus)
associationDescription_status = Lens.lens (\AssociationDescription' {status} -> status) (\s@AssociationDescription' {} a -> s {status = a} :: AssociationDescription)

-- | The date on which the association was last run.
associationDescription_lastExecutionDate :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.UTCTime)
associationDescription_lastExecutionDate = Lens.lens (\AssociationDescription' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationDescription' {} a -> s {lastExecutionDate = a} :: AssociationDescription) Prelude.. Lens.mapping Core._Time

-- | The ID of the instance.
associationDescription_instanceId :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_instanceId = Lens.lens (\AssociationDescription' {instanceId} -> instanceId) (\s@AssociationDescription' {} a -> s {instanceId = a} :: AssociationDescription)

-- | The date when the association was last updated.
associationDescription_lastUpdateAssociationDate :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.UTCTime)
associationDescription_lastUpdateAssociationDate = Lens.lens (\AssociationDescription' {lastUpdateAssociationDate} -> lastUpdateAssociationDate) (\s@AssociationDescription' {} a -> s {lastUpdateAssociationDate = a} :: AssociationDescription) Prelude.. Lens.mapping Core._Time

-- | The severity level that is assigned to the association.
associationDescription_complianceSeverity :: Lens.Lens' AssociationDescription (Prelude.Maybe AssociationComplianceSeverity)
associationDescription_complianceSeverity = Lens.lens (\AssociationDescription' {complianceSeverity} -> complianceSeverity) (\s@AssociationDescription' {} a -> s {complianceSeverity = a} :: AssociationDescription)

-- | Information about the association.
associationDescription_overview :: Lens.Lens' AssociationDescription (Prelude.Maybe AssociationOverview)
associationDescription_overview = Lens.lens (\AssociationDescription' {overview} -> overview) (\s@AssociationDescription' {} a -> s {overview = a} :: AssociationDescription)

-- | Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
associationDescription_automationTargetParameterName :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_automationTargetParameterName = Lens.lens (\AssociationDescription' {automationTargetParameterName} -> automationTargetParameterName) (\s@AssociationDescription' {} a -> s {automationTargetParameterName = a} :: AssociationDescription)

-- | The instances targeted by the request.
associationDescription_targets :: Lens.Lens' AssociationDescription (Prelude.Maybe [Target])
associationDescription_targets = Lens.lens (\AssociationDescription' {targets} -> targets) (\s@AssociationDescription' {} a -> s {targets = a} :: AssociationDescription) Prelude.. Lens.mapping Lens._Coerce

-- | The combination of AWS Regions and AWS accounts where you want to run
-- the association.
associationDescription_targetLocations :: Lens.Lens' AssociationDescription (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
associationDescription_targetLocations = Lens.lens (\AssociationDescription' {targetLocations} -> targetLocations) (\s@AssociationDescription' {} a -> s {targetLocations = a} :: AssociationDescription) Prelude.. Lens.mapping Lens._Coerce

-- | A cron expression that specifies a schedule when the association runs.
associationDescription_scheduleExpression :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_scheduleExpression = Lens.lens (\AssociationDescription' {scheduleExpression} -> scheduleExpression) (\s@AssociationDescription' {} a -> s {scheduleExpression = a} :: AssociationDescription)

-- | The name of the Systems Manager document.
associationDescription_name :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_name = Lens.lens (\AssociationDescription' {name} -> name) (\s@AssociationDescription' {} a -> s {name = a} :: AssociationDescription)

-- | The association ID.
associationDescription_associationId :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_associationId = Lens.lens (\AssociationDescription' {associationId} -> associationId) (\s@AssociationDescription' {} a -> s {associationId = a} :: AssociationDescription)

-- | The date when the association was made.
associationDescription_date :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.UTCTime)
associationDescription_date = Lens.lens (\AssociationDescription' {date} -> date) (\s@AssociationDescription' {} a -> s {date = a} :: AssociationDescription) Prelude.. Lens.mapping Core._Time

-- | The maximum number of targets allowed to run the association at the same
-- time. You can specify a number, for example 10, or a percentage of the
-- target set, for example 10%. The default value is 100%, which means all
-- targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while
-- Systems Manager is running MaxConcurrency associations, the association
-- is allowed to run. During the next association interval, the new
-- instance will process its association within the limit specified for
-- MaxConcurrency.
associationDescription_maxConcurrency :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_maxConcurrency = Lens.lens (\AssociationDescription' {maxConcurrency} -> maxConcurrency) (\s@AssociationDescription' {} a -> s {maxConcurrency = a} :: AssociationDescription)

-- | The association name.
associationDescription_associationName :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_associationName = Lens.lens (\AssociationDescription' {associationName} -> associationName) (\s@AssociationDescription' {} a -> s {associationName = a} :: AssociationDescription)

-- | The association version.
associationDescription_associationVersion :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_associationVersion = Lens.lens (\AssociationDescription' {associationVersion} -> associationVersion) (\s@AssociationDescription' {} a -> s {associationVersion = a} :: AssociationDescription)

-- | The last date on which the association was successfully run.
associationDescription_lastSuccessfulExecutionDate :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.UTCTime)
associationDescription_lastSuccessfulExecutionDate = Lens.lens (\AssociationDescription' {lastSuccessfulExecutionDate} -> lastSuccessfulExecutionDate) (\s@AssociationDescription' {} a -> s {lastSuccessfulExecutionDate = a} :: AssociationDescription) Prelude.. Lens.mapping Core._Time

-- | The document version.
associationDescription_documentVersion :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Text)
associationDescription_documentVersion = Lens.lens (\AssociationDescription' {documentVersion} -> documentVersion) (\s@AssociationDescription' {} a -> s {documentVersion = a} :: AssociationDescription)

-- | A description of the parameters for a document.
associationDescription_parameters :: Lens.Lens' AssociationDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
associationDescription_parameters = Lens.lens (\AssociationDescription' {parameters} -> parameters) (\s@AssociationDescription' {} a -> s {parameters = a} :: AssociationDescription) Prelude.. Lens.mapping Lens._Coerce

-- | An S3 bucket where you want to store the output details of the request.
associationDescription_outputLocation :: Lens.Lens' AssociationDescription (Prelude.Maybe InstanceAssociationOutputLocation)
associationDescription_outputLocation = Lens.lens (\AssociationDescription' {outputLocation} -> outputLocation) (\s@AssociationDescription' {} a -> s {outputLocation = a} :: AssociationDescription)

-- | By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter is not supported for
-- rate expressions.
associationDescription_applyOnlyAtCronInterval :: Lens.Lens' AssociationDescription (Prelude.Maybe Prelude.Bool)
associationDescription_applyOnlyAtCronInterval = Lens.lens (\AssociationDescription' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@AssociationDescription' {} a -> s {applyOnlyAtCronInterval = a} :: AssociationDescription)

-- | The mode for generating association compliance. You can specify @AUTO@
-- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
-- association execution to determine the compliance status. If the
-- association execution runs successfully, then the association is
-- @COMPLIANT@. If the association execution doesn\'t run successfully, the
-- association is @NON-COMPLIANT@.
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
-- for the PutComplianceItems API action. In this case, compliance data is
-- not managed by State Manager. It is managed by your direct call to the
-- PutComplianceItems API action.
--
-- By default, all associations use @AUTO@ mode.
associationDescription_syncCompliance :: Lens.Lens' AssociationDescription (Prelude.Maybe AssociationSyncCompliance)
associationDescription_syncCompliance = Lens.lens (\AssociationDescription' {syncCompliance} -> syncCompliance) (\s@AssociationDescription' {} a -> s {syncCompliance = a} :: AssociationDescription)

instance Core.FromJSON AssociationDescription where
  parseJSON =
    Core.withObject
      "AssociationDescription"
      ( \x ->
          AssociationDescription'
            Prelude.<$> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastExecutionDate")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "LastUpdateAssociationDate")
            Prelude.<*> (x Core..:? "ComplianceSeverity")
            Prelude.<*> (x Core..:? "Overview")
            Prelude.<*> (x Core..:? "AutomationTargetParameterName")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TargetLocations")
            Prelude.<*> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "Date")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "AssociationName")
            Prelude.<*> (x Core..:? "AssociationVersion")
            Prelude.<*> (x Core..:? "LastSuccessfulExecutionDate")
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OutputLocation")
            Prelude.<*> (x Core..:? "ApplyOnlyAtCronInterval")
            Prelude.<*> (x Core..:? "SyncCompliance")
      )

instance Prelude.Hashable AssociationDescription

instance Prelude.NFData AssociationDescription
