{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationDescription
  ( AssociationDescription (..),

    -- * Smart constructor
    mkAssociationDescription,

    -- * Lenses
    adApplyOnlyAtCronInterval,
    adAssociationId,
    adAssociationName,
    adAssociationVersion,
    adAutomationTargetParameterName,
    adComplianceSeverity,
    adDate,
    adDocumentVersion,
    adInstanceId,
    adLastExecutionDate,
    adLastSuccessfulExecutionDate,
    adLastUpdateAssociationDate,
    adMaxConcurrency,
    adMaxErrors,
    adName,
    adOutputLocation,
    adOverview,
    adParameters,
    adScheduleExpression,
    adStatus,
    adSyncCompliance,
    adTargets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationComplianceSeverity as Types
import qualified Network.AWS.SSM.Types.AssociationId as Types
import qualified Network.AWS.SSM.Types.AssociationName as Types
import qualified Network.AWS.SSM.Types.AssociationOverview as Types
import qualified Network.AWS.SSM.Types.AssociationStatus as Types
import qualified Network.AWS.SSM.Types.AssociationSyncCompliance as Types
import qualified Network.AWS.SSM.Types.AssociationVersion as Types
import qualified Network.AWS.SSM.Types.AutomationTargetParameterName as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.InstanceAssociationOutputLocation as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types
import qualified Network.AWS.SSM.Types.MaxConcurrency as Types
import qualified Network.AWS.SSM.Types.MaxErrors as Types
import qualified Network.AWS.SSM.Types.Name as Types
import qualified Network.AWS.SSM.Types.ParameterName as Types
import qualified Network.AWS.SSM.Types.ParameterValue as Types
import qualified Network.AWS.SSM.Types.ScheduleExpression as Types
import qualified Network.AWS.SSM.Types.Target as Types

-- | Describes the parameters for a document.
--
-- /See:/ 'mkAssociationDescription' smart constructor.
data AssociationDescription = AssociationDescription'
  { -- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
    applyOnlyAtCronInterval :: Core.Maybe Core.Bool,
    -- | The association ID.
    associationId :: Core.Maybe Types.AssociationId,
    -- | The association name.
    associationName :: Core.Maybe Types.AssociationName,
    -- | The association version.
    associationVersion :: Core.Maybe Types.AssociationVersion,
    -- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
    automationTargetParameterName :: Core.Maybe Types.AutomationTargetParameterName,
    -- | The severity level that is assigned to the association.
    complianceSeverity :: Core.Maybe Types.AssociationComplianceSeverity,
    -- | The date when the association was made.
    date :: Core.Maybe Core.NominalDiffTime,
    -- | The document version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The date on which the association was last run.
    lastExecutionDate :: Core.Maybe Core.NominalDiffTime,
    -- | The last date on which the association was successfully run.
    lastSuccessfulExecutionDate :: Core.Maybe Core.NominalDiffTime,
    -- | The date when the association was last updated.
    lastUpdateAssociationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
    --
    -- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
    maxConcurrency :: Core.Maybe Types.MaxConcurrency,
    -- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
    --
    -- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
    maxErrors :: Core.Maybe Types.MaxErrors,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Types.Name,
    -- | An S3 bucket where you want to store the output details of the request.
    outputLocation :: Core.Maybe Types.InstanceAssociationOutputLocation,
    -- | Information about the association.
    overview :: Core.Maybe Types.AssociationOverview,
    -- | A description of the parameters for a document.
    parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]),
    -- | A cron expression that specifies a schedule when the association runs.
    scheduleExpression :: Core.Maybe Types.ScheduleExpression,
    -- | The association status.
    status :: Core.Maybe Types.AssociationStatus,
    -- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
    --
    -- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
    -- By default, all associations use @AUTO@ mode.
    syncCompliance :: Core.Maybe Types.AssociationSyncCompliance,
    -- | The instances targeted by the request.
    targets :: Core.Maybe [Types.Target]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssociationDescription' value with any optional fields omitted.
mkAssociationDescription ::
  AssociationDescription
mkAssociationDescription =
  AssociationDescription'
    { applyOnlyAtCronInterval = Core.Nothing,
      associationId = Core.Nothing,
      associationName = Core.Nothing,
      associationVersion = Core.Nothing,
      automationTargetParameterName = Core.Nothing,
      complianceSeverity = Core.Nothing,
      date = Core.Nothing,
      documentVersion = Core.Nothing,
      instanceId = Core.Nothing,
      lastExecutionDate = Core.Nothing,
      lastSuccessfulExecutionDate = Core.Nothing,
      lastUpdateAssociationDate = Core.Nothing,
      maxConcurrency = Core.Nothing,
      maxErrors = Core.Nothing,
      name = Core.Nothing,
      outputLocation = Core.Nothing,
      overview = Core.Nothing,
      parameters = Core.Nothing,
      scheduleExpression = Core.Nothing,
      status = Core.Nothing,
      syncCompliance = Core.Nothing,
      targets = Core.Nothing
    }

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplyOnlyAtCronInterval :: Lens.Lens' AssociationDescription (Core.Maybe Core.Bool)
adApplyOnlyAtCronInterval = Lens.field @"applyOnlyAtCronInterval"
{-# DEPRECATED adApplyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead." #-}

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAssociationId :: Lens.Lens' AssociationDescription (Core.Maybe Types.AssociationId)
adAssociationId = Lens.field @"associationId"
{-# DEPRECATED adAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The association name.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAssociationName :: Lens.Lens' AssociationDescription (Core.Maybe Types.AssociationName)
adAssociationName = Lens.field @"associationName"
{-# DEPRECATED adAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAssociationVersion :: Lens.Lens' AssociationDescription (Core.Maybe Types.AssociationVersion)
adAssociationVersion = Lens.field @"associationVersion"
{-# DEPRECATED adAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAutomationTargetParameterName :: Lens.Lens' AssociationDescription (Core.Maybe Types.AutomationTargetParameterName)
adAutomationTargetParameterName = Lens.field @"automationTargetParameterName"
{-# DEPRECATED adAutomationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead." #-}

-- | The severity level that is assigned to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adComplianceSeverity :: Lens.Lens' AssociationDescription (Core.Maybe Types.AssociationComplianceSeverity)
adComplianceSeverity = Lens.field @"complianceSeverity"
{-# DEPRECATED adComplianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead." #-}

-- | The date when the association was made.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDate :: Lens.Lens' AssociationDescription (Core.Maybe Core.NominalDiffTime)
adDate = Lens.field @"date"
{-# DEPRECATED adDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDocumentVersion :: Lens.Lens' AssociationDescription (Core.Maybe Types.DocumentVersion)
adDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED adDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInstanceId :: Lens.Lens' AssociationDescription (Core.Maybe Types.InstanceId)
adInstanceId = Lens.field @"instanceId"
{-# DEPRECATED adInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The date on which the association was last run.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastExecutionDate :: Lens.Lens' AssociationDescription (Core.Maybe Core.NominalDiffTime)
adLastExecutionDate = Lens.field @"lastExecutionDate"
{-# DEPRECATED adLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | The last date on which the association was successfully run.
--
-- /Note:/ Consider using 'lastSuccessfulExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastSuccessfulExecutionDate :: Lens.Lens' AssociationDescription (Core.Maybe Core.NominalDiffTime)
adLastSuccessfulExecutionDate = Lens.field @"lastSuccessfulExecutionDate"
{-# DEPRECATED adLastSuccessfulExecutionDate "Use generic-lens or generic-optics with 'lastSuccessfulExecutionDate' instead." #-}

-- | The date when the association was last updated.
--
-- /Note:/ Consider using 'lastUpdateAssociationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastUpdateAssociationDate :: Lens.Lens' AssociationDescription (Core.Maybe Core.NominalDiffTime)
adLastUpdateAssociationDate = Lens.field @"lastUpdateAssociationDate"
{-# DEPRECATED adLastUpdateAssociationDate "Use generic-lens or generic-optics with 'lastUpdateAssociationDate' instead." #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMaxConcurrency :: Lens.Lens' AssociationDescription (Core.Maybe Types.MaxConcurrency)
adMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED adMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMaxErrors :: Lens.Lens' AssociationDescription (Core.Maybe Types.MaxErrors)
adMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED adMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adName :: Lens.Lens' AssociationDescription (Core.Maybe Types.Name)
adName = Lens.field @"name"
{-# DEPRECATED adName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An S3 bucket where you want to store the output details of the request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOutputLocation :: Lens.Lens' AssociationDescription (Core.Maybe Types.InstanceAssociationOutputLocation)
adOutputLocation = Lens.field @"outputLocation"
{-# DEPRECATED adOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | Information about the association.
--
-- /Note:/ Consider using 'overview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOverview :: Lens.Lens' AssociationDescription (Core.Maybe Types.AssociationOverview)
adOverview = Lens.field @"overview"
{-# DEPRECATED adOverview "Use generic-lens or generic-optics with 'overview' instead." #-}

-- | A description of the parameters for a document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adParameters :: Lens.Lens' AssociationDescription (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
adParameters = Lens.field @"parameters"
{-# DEPRECATED adParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A cron expression that specifies a schedule when the association runs.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adScheduleExpression :: Lens.Lens' AssociationDescription (Core.Maybe Types.ScheduleExpression)
adScheduleExpression = Lens.field @"scheduleExpression"
{-# DEPRECATED adScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The association status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStatus :: Lens.Lens' AssociationDescription (Core.Maybe Types.AssociationStatus)
adStatus = Lens.field @"status"
{-# DEPRECATED adStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSyncCompliance :: Lens.Lens' AssociationDescription (Core.Maybe Types.AssociationSyncCompliance)
adSyncCompliance = Lens.field @"syncCompliance"
{-# DEPRECATED adSyncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead." #-}

-- | The instances targeted by the request.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTargets :: Lens.Lens' AssociationDescription (Core.Maybe [Types.Target])
adTargets = Lens.field @"targets"
{-# DEPRECATED adTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Core.FromJSON AssociationDescription where
  parseJSON =
    Core.withObject "AssociationDescription" Core.$
      \x ->
        AssociationDescription'
          Core.<$> (x Core..:? "ApplyOnlyAtCronInterval")
          Core.<*> (x Core..:? "AssociationId")
          Core.<*> (x Core..:? "AssociationName")
          Core.<*> (x Core..:? "AssociationVersion")
          Core.<*> (x Core..:? "AutomationTargetParameterName")
          Core.<*> (x Core..:? "ComplianceSeverity")
          Core.<*> (x Core..:? "Date")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "LastExecutionDate")
          Core.<*> (x Core..:? "LastSuccessfulExecutionDate")
          Core.<*> (x Core..:? "LastUpdateAssociationDate")
          Core.<*> (x Core..:? "MaxConcurrency")
          Core.<*> (x Core..:? "MaxErrors")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "OutputLocation")
          Core.<*> (x Core..:? "Overview")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "ScheduleExpression")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "SyncCompliance")
          Core.<*> (x Core..:? "Targets")
