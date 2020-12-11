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
    adAssociationId,
    adInstanceId,
    adStatus,
    adApplyOnlyAtCronInterval,
    adLastSuccessfulExecutionDate,
    adOverview,
    adLastUpdateAssociationDate,
    adDate,
    adLastExecutionDate,
    adMaxErrors,
    adScheduleExpression,
    adName,
    adOutputLocation,
    adSyncCompliance,
    adTargets,
    adParameters,
    adDocumentVersion,
    adAutomationTargetParameterName,
    adAssociationVersion,
    adAssociationName,
    adComplianceSeverity,
    adMaxConcurrency,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.AssociationStatus
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target

-- | Describes the parameters for a document.
--
-- /See:/ 'mkAssociationDescription' smart constructor.
data AssociationDescription = AssociationDescription'
  { associationId ::
      Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe AssociationStatus,
    applyOnlyAtCronInterval ::
      Lude.Maybe Lude.Bool,
    lastSuccessfulExecutionDate ::
      Lude.Maybe Lude.Timestamp,
    overview :: Lude.Maybe AssociationOverview,
    lastUpdateAssociationDate ::
      Lude.Maybe Lude.Timestamp,
    date :: Lude.Maybe Lude.Timestamp,
    lastExecutionDate ::
      Lude.Maybe Lude.Timestamp,
    maxErrors :: Lude.Maybe Lude.Text,
    scheduleExpression :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    outputLocation ::
      Lude.Maybe InstanceAssociationOutputLocation,
    syncCompliance ::
      Lude.Maybe AssociationSyncCompliance,
    targets :: Lude.Maybe [Target],
    parameters ::
      Lude.Maybe
        (Lude.HashMap Lude.Text ([Lude.Text])),
    documentVersion :: Lude.Maybe Lude.Text,
    automationTargetParameterName ::
      Lude.Maybe Lude.Text,
    associationVersion :: Lude.Maybe Lude.Text,
    associationName :: Lude.Maybe Lude.Text,
    complianceSeverity ::
      Lude.Maybe AssociationComplianceSeverity,
    maxConcurrency :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationDescription' with the minimum fields required to make a request.
--
-- * 'applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
-- * 'associationId' - The association ID.
-- * 'associationName' - The association name.
-- * 'associationVersion' - The association version.
-- * 'automationTargetParameterName' - Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
-- * 'complianceSeverity' - The severity level that is assigned to the association.
-- * 'date' - The date when the association was made.
-- * 'documentVersion' - The document version.
-- * 'instanceId' - The ID of the instance.
-- * 'lastExecutionDate' - The date on which the association was last run.
-- * 'lastSuccessfulExecutionDate' - The last date on which the association was successfully run.
-- * 'lastUpdateAssociationDate' - The date when the association was last updated.
-- * 'maxConcurrency' - The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
-- * 'maxErrors' - The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
-- * 'name' - The name of the Systems Manager document.
-- * 'outputLocation' - An S3 bucket where you want to store the output details of the request.
-- * 'overview' - Information about the association.
-- * 'parameters' - A description of the parameters for a document.
-- * 'scheduleExpression' - A cron expression that specifies a schedule when the association runs.
-- * 'status' - The association status.
-- * 'syncCompliance' - The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
-- * 'targets' - The instances targeted by the request.
mkAssociationDescription ::
  AssociationDescription
mkAssociationDescription =
  AssociationDescription'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      status = Lude.Nothing,
      applyOnlyAtCronInterval = Lude.Nothing,
      lastSuccessfulExecutionDate = Lude.Nothing,
      overview = Lude.Nothing,
      lastUpdateAssociationDate = Lude.Nothing,
      date = Lude.Nothing,
      lastExecutionDate = Lude.Nothing,
      maxErrors = Lude.Nothing,
      scheduleExpression = Lude.Nothing,
      name = Lude.Nothing,
      outputLocation = Lude.Nothing,
      syncCompliance = Lude.Nothing,
      targets = Lude.Nothing,
      parameters = Lude.Nothing,
      documentVersion = Lude.Nothing,
      automationTargetParameterName = Lude.Nothing,
      associationVersion = Lude.Nothing,
      associationName = Lude.Nothing,
      complianceSeverity = Lude.Nothing,
      maxConcurrency = Lude.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAssociationId :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adAssociationId = Lens.lens (associationId :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociationDescription)
{-# DEPRECATED adAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInstanceId :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adInstanceId = Lens.lens (instanceId :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: AssociationDescription)
{-# DEPRECATED adInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The association status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStatus :: Lens.Lens' AssociationDescription (Lude.Maybe AssociationStatus)
adStatus = Lens.lens (status :: AssociationDescription -> Lude.Maybe AssociationStatus) (\s a -> s {status = a} :: AssociationDescription)
{-# DEPRECATED adStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplyOnlyAtCronInterval :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Bool)
adApplyOnlyAtCronInterval = Lens.lens (applyOnlyAtCronInterval :: AssociationDescription -> Lude.Maybe Lude.Bool) (\s a -> s {applyOnlyAtCronInterval = a} :: AssociationDescription)
{-# DEPRECATED adApplyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead." #-}

-- | The last date on which the association was successfully run.
--
-- /Note:/ Consider using 'lastSuccessfulExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastSuccessfulExecutionDate :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Timestamp)
adLastSuccessfulExecutionDate = Lens.lens (lastSuccessfulExecutionDate :: AssociationDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastSuccessfulExecutionDate = a} :: AssociationDescription)
{-# DEPRECATED adLastSuccessfulExecutionDate "Use generic-lens or generic-optics with 'lastSuccessfulExecutionDate' instead." #-}

-- | Information about the association.
--
-- /Note:/ Consider using 'overview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOverview :: Lens.Lens' AssociationDescription (Lude.Maybe AssociationOverview)
adOverview = Lens.lens (overview :: AssociationDescription -> Lude.Maybe AssociationOverview) (\s a -> s {overview = a} :: AssociationDescription)
{-# DEPRECATED adOverview "Use generic-lens or generic-optics with 'overview' instead." #-}

-- | The date when the association was last updated.
--
-- /Note:/ Consider using 'lastUpdateAssociationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastUpdateAssociationDate :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Timestamp)
adLastUpdateAssociationDate = Lens.lens (lastUpdateAssociationDate :: AssociationDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateAssociationDate = a} :: AssociationDescription)
{-# DEPRECATED adLastUpdateAssociationDate "Use generic-lens or generic-optics with 'lastUpdateAssociationDate' instead." #-}

-- | The date when the association was made.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDate :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Timestamp)
adDate = Lens.lens (date :: AssociationDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {date = a} :: AssociationDescription)
{-# DEPRECATED adDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The date on which the association was last run.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastExecutionDate :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Timestamp)
adLastExecutionDate = Lens.lens (lastExecutionDate :: AssociationDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastExecutionDate = a} :: AssociationDescription)
{-# DEPRECATED adLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMaxErrors :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adMaxErrors = Lens.lens (maxErrors :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: AssociationDescription)
{-# DEPRECATED adMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | A cron expression that specifies a schedule when the association runs.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adScheduleExpression :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adScheduleExpression = Lens.lens (scheduleExpression :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: AssociationDescription)
{-# DEPRECATED adScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adName :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adName = Lens.lens (name :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AssociationDescription)
{-# DEPRECATED adName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An S3 bucket where you want to store the output details of the request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOutputLocation :: Lens.Lens' AssociationDescription (Lude.Maybe InstanceAssociationOutputLocation)
adOutputLocation = Lens.lens (outputLocation :: AssociationDescription -> Lude.Maybe InstanceAssociationOutputLocation) (\s a -> s {outputLocation = a} :: AssociationDescription)
{-# DEPRECATED adOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSyncCompliance :: Lens.Lens' AssociationDescription (Lude.Maybe AssociationSyncCompliance)
adSyncCompliance = Lens.lens (syncCompliance :: AssociationDescription -> Lude.Maybe AssociationSyncCompliance) (\s a -> s {syncCompliance = a} :: AssociationDescription)
{-# DEPRECATED adSyncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead." #-}

-- | The instances targeted by the request.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTargets :: Lens.Lens' AssociationDescription (Lude.Maybe [Target])
adTargets = Lens.lens (targets :: AssociationDescription -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: AssociationDescription)
{-# DEPRECATED adTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A description of the parameters for a document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adParameters :: Lens.Lens' AssociationDescription (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
adParameters = Lens.lens (parameters :: AssociationDescription -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: AssociationDescription)
{-# DEPRECATED adParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDocumentVersion :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adDocumentVersion = Lens.lens (documentVersion :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: AssociationDescription)
{-# DEPRECATED adDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAutomationTargetParameterName :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adAutomationTargetParameterName = Lens.lens (automationTargetParameterName :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {automationTargetParameterName = a} :: AssociationDescription)
{-# DEPRECATED adAutomationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAssociationVersion :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adAssociationVersion = Lens.lens (associationVersion :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: AssociationDescription)
{-# DEPRECATED adAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The association name.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAssociationName :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adAssociationName = Lens.lens (associationName :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: AssociationDescription)
{-# DEPRECATED adAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | The severity level that is assigned to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adComplianceSeverity :: Lens.Lens' AssociationDescription (Lude.Maybe AssociationComplianceSeverity)
adComplianceSeverity = Lens.lens (complianceSeverity :: AssociationDescription -> Lude.Maybe AssociationComplianceSeverity) (\s a -> s {complianceSeverity = a} :: AssociationDescription)
{-# DEPRECATED adComplianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead." #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMaxConcurrency :: Lens.Lens' AssociationDescription (Lude.Maybe Lude.Text)
adMaxConcurrency = Lens.lens (maxConcurrency :: AssociationDescription -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: AssociationDescription)
{-# DEPRECATED adMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

instance Lude.FromJSON AssociationDescription where
  parseJSON =
    Lude.withObject
      "AssociationDescription"
      ( \x ->
          AssociationDescription'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ApplyOnlyAtCronInterval")
            Lude.<*> (x Lude..:? "LastSuccessfulExecutionDate")
            Lude.<*> (x Lude..:? "Overview")
            Lude.<*> (x Lude..:? "LastUpdateAssociationDate")
            Lude.<*> (x Lude..:? "Date")
            Lude.<*> (x Lude..:? "LastExecutionDate")
            Lude.<*> (x Lude..:? "MaxErrors")
            Lude.<*> (x Lude..:? "ScheduleExpression")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "OutputLocation")
            Lude.<*> (x Lude..:? "SyncCompliance")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "AutomationTargetParameterName")
            Lude.<*> (x Lude..:? "AssociationVersion")
            Lude.<*> (x Lude..:? "AssociationName")
            Lude.<*> (x Lude..:? "ComplianceSeverity")
            Lude.<*> (x Lude..:? "MaxConcurrency")
      )
