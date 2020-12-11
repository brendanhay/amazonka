-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
  ( CreateAssociationBatchRequestEntry (..),

    -- * Smart constructor
    mkCreateAssociationBatchRequestEntry,

    -- * Lenses
    cabreInstanceId,
    cabreApplyOnlyAtCronInterval,
    cabreMaxErrors,
    cabreScheduleExpression,
    cabreOutputLocation,
    cabreSyncCompliance,
    cabreTargets,
    cabreParameters,
    cabreDocumentVersion,
    cabreAutomationTargetParameterName,
    cabreAssociationName,
    cabreComplianceSeverity,
    cabreMaxConcurrency,
    cabreName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target

-- | Describes the association of a Systems Manager SSM document and an instance.
--
-- /See:/ 'mkCreateAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
  { instanceId ::
      Lude.Maybe Lude.Text,
    applyOnlyAtCronInterval ::
      Lude.Maybe Lude.Bool,
    maxErrors ::
      Lude.Maybe Lude.Text,
    scheduleExpression ::
      Lude.Maybe Lude.Text,
    outputLocation ::
      Lude.Maybe
        InstanceAssociationOutputLocation,
    syncCompliance ::
      Lude.Maybe
        AssociationSyncCompliance,
    targets ::
      Lude.Maybe [Target],
    parameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ([Lude.Text])
        ),
    documentVersion ::
      Lude.Maybe Lude.Text,
    automationTargetParameterName ::
      Lude.Maybe Lude.Text,
    associationName ::
      Lude.Maybe Lude.Text,
    complianceSeverity ::
      Lude.Maybe
        AssociationComplianceSeverity,
    maxConcurrency ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAssociationBatchRequestEntry' with the minimum fields required to make a request.
--
-- * 'applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
-- * 'associationName' - Specify a descriptive name for the association.
-- * 'automationTargetParameterName' - Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
-- * 'complianceSeverity' - The severity level to assign to the association.
-- * 'documentVersion' - The document version.
-- * 'instanceId' - The ID of the instance.
-- * 'maxConcurrency' - The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
-- * 'maxErrors' - The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
-- * 'name' - The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account.
-- For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format:
-- @arn:aws:ssm:/region/ :/account-id/ :document//document-name/ @
-- For example:
-- @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@
-- For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
-- * 'outputLocation' - An S3 bucket where you want to store the results of this request.
-- * 'parameters' - A description of the parameters for a document.
-- * 'scheduleExpression' - A cron expression that specifies a schedule when the association runs.
-- * 'syncCompliance' - The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
-- * 'targets' - The instances targeted by the request.
mkCreateAssociationBatchRequestEntry ::
  -- | 'name'
  Lude.Text ->
  CreateAssociationBatchRequestEntry
mkCreateAssociationBatchRequestEntry pName_ =
  CreateAssociationBatchRequestEntry'
    { instanceId = Lude.Nothing,
      applyOnlyAtCronInterval = Lude.Nothing,
      maxErrors = Lude.Nothing,
      scheduleExpression = Lude.Nothing,
      outputLocation = Lude.Nothing,
      syncCompliance = Lude.Nothing,
      targets = Lude.Nothing,
      parameters = Lude.Nothing,
      documentVersion = Lude.Nothing,
      automationTargetParameterName = Lude.Nothing,
      associationName = Lude.Nothing,
      complianceSeverity = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      name = pName_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreInstanceId :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Text)
cabreInstanceId = Lens.lens (instanceId :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreApplyOnlyAtCronInterval :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Bool)
cabreApplyOnlyAtCronInterval = Lens.lens (applyOnlyAtCronInterval :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Bool) (\s a -> s {applyOnlyAtCronInterval = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreApplyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead." #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreMaxErrors :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Text)
cabreMaxErrors = Lens.lens (maxErrors :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | A cron expression that specifies a schedule when the association runs.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreScheduleExpression :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Text)
cabreScheduleExpression = Lens.lens (scheduleExpression :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | An S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreOutputLocation :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe InstanceAssociationOutputLocation)
cabreOutputLocation = Lens.lens (outputLocation :: CreateAssociationBatchRequestEntry -> Lude.Maybe InstanceAssociationOutputLocation) (\s a -> s {outputLocation = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreSyncCompliance :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe AssociationSyncCompliance)
cabreSyncCompliance = Lens.lens (syncCompliance :: CreateAssociationBatchRequestEntry -> Lude.Maybe AssociationSyncCompliance) (\s a -> s {syncCompliance = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreSyncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead." #-}

-- | The instances targeted by the request.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreTargets :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe [Target])
cabreTargets = Lens.lens (targets :: CreateAssociationBatchRequestEntry -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A description of the parameters for a document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreParameters :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
cabreParameters = Lens.lens (parameters :: CreateAssociationBatchRequestEntry -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreDocumentVersion :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Text)
cabreDocumentVersion = Lens.lens (documentVersion :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreAutomationTargetParameterName :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Text)
cabreAutomationTargetParameterName = Lens.lens (automationTargetParameterName :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {automationTargetParameterName = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreAutomationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead." #-}

-- | Specify a descriptive name for the association.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreAssociationName :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Text)
cabreAssociationName = Lens.lens (associationName :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | The severity level to assign to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreComplianceSeverity :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe AssociationComplianceSeverity)
cabreComplianceSeverity = Lens.lens (complianceSeverity :: CreateAssociationBatchRequestEntry -> Lude.Maybe AssociationComplianceSeverity) (\s a -> s {complianceSeverity = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreComplianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead." #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreMaxConcurrency :: Lens.Lens' CreateAssociationBatchRequestEntry (Lude.Maybe Lude.Text)
cabreMaxConcurrency = Lens.lens (maxConcurrency :: CreateAssociationBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account.
-- For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format:
-- @arn:aws:ssm:/region/ :/account-id/ :document//document-name/ @
-- For example:
-- @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@
-- For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreName :: Lens.Lens' CreateAssociationBatchRequestEntry Lude.Text
cabreName = Lens.lens (name :: CreateAssociationBatchRequestEntry -> Lude.Text) (\s a -> s {name = a} :: CreateAssociationBatchRequestEntry)
{-# DEPRECATED cabreName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CreateAssociationBatchRequestEntry where
  parseJSON =
    Lude.withObject
      "CreateAssociationBatchRequestEntry"
      ( \x ->
          CreateAssociationBatchRequestEntry'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "ApplyOnlyAtCronInterval")
            Lude.<*> (x Lude..:? "MaxErrors")
            Lude.<*> (x Lude..:? "ScheduleExpression")
            Lude.<*> (x Lude..:? "OutputLocation")
            Lude.<*> (x Lude..:? "SyncCompliance")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "AutomationTargetParameterName")
            Lude.<*> (x Lude..:? "AssociationName")
            Lude.<*> (x Lude..:? "ComplianceSeverity")
            Lude.<*> (x Lude..:? "MaxConcurrency")
            Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON CreateAssociationBatchRequestEntry where
  toJSON CreateAssociationBatchRequestEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("ApplyOnlyAtCronInterval" Lude..=)
              Lude.<$> applyOnlyAtCronInterval,
            ("MaxErrors" Lude..=) Lude.<$> maxErrors,
            ("ScheduleExpression" Lude..=) Lude.<$> scheduleExpression,
            ("OutputLocation" Lude..=) Lude.<$> outputLocation,
            ("SyncCompliance" Lude..=) Lude.<$> syncCompliance,
            ("Targets" Lude..=) Lude.<$> targets,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            ("AutomationTargetParameterName" Lude..=)
              Lude.<$> automationTargetParameterName,
            ("AssociationName" Lude..=) Lude.<$> associationName,
            ("ComplianceSeverity" Lude..=) Lude.<$> complianceSeverity,
            ("MaxConcurrency" Lude..=) Lude.<$> maxConcurrency,
            Lude.Just ("Name" Lude..= name)
          ]
      )
