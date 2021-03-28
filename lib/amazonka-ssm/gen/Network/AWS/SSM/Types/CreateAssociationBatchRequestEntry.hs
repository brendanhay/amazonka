{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
  ( CreateAssociationBatchRequestEntry (..)
  -- * Smart constructor
  , mkCreateAssociationBatchRequestEntry
  -- * Lenses
  , cabreName
  , cabreApplyOnlyAtCronInterval
  , cabreAssociationName
  , cabreAutomationTargetParameterName
  , cabreComplianceSeverity
  , cabreDocumentVersion
  , cabreInstanceId
  , cabreMaxConcurrency
  , cabreMaxErrors
  , cabreOutputLocation
  , cabreParameters
  , cabreScheduleExpression
  , cabreSyncCompliance
  , cabreTargets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationComplianceSeverity as Types
import qualified Network.AWS.SSM.Types.AssociationName as Types
import qualified Network.AWS.SSM.Types.AssociationSyncCompliance as Types
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

-- | Describes the association of a Systems Manager SSM document and an instance.
--
-- /See:/ 'mkCreateAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
  { name :: Types.Name
    -- ^ The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account.
-- For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format:
-- @arn:aws:ssm:/region/ :/account-id/ :document//document-name/ @ 
-- For example:
-- @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@ 
-- For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
  , applyOnlyAtCronInterval :: Core.Maybe Core.Bool
    -- ^ By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
  , associationName :: Core.Maybe Types.AssociationName
    -- ^ Specify a descriptive name for the association.
  , automationTargetParameterName :: Core.Maybe Types.AutomationTargetParameterName
    -- ^ Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
  , complianceSeverity :: Core.Maybe Types.AssociationComplianceSeverity
    -- ^ The severity level to assign to the association.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The document version.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of the instance. 
  , maxConcurrency :: Core.Maybe Types.MaxConcurrency
    -- ^ The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
  , maxErrors :: Core.Maybe Types.MaxErrors
    -- ^ The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
  , outputLocation :: Core.Maybe Types.InstanceAssociationOutputLocation
    -- ^ An S3 bucket where you want to store the results of this request.
  , parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue])
    -- ^ A description of the parameters for a document. 
  , scheduleExpression :: Core.Maybe Types.ScheduleExpression
    -- ^ A cron expression that specifies a schedule when the association runs.
  , syncCompliance :: Core.Maybe Types.AssociationSyncCompliance
    -- ^ The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ . 
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
  , targets :: Core.Maybe [Types.Target]
    -- ^ The instances targeted by the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAssociationBatchRequestEntry' value with any optional fields omitted.
mkCreateAssociationBatchRequestEntry
    :: Types.Name -- ^ 'name'
    -> CreateAssociationBatchRequestEntry
mkCreateAssociationBatchRequestEntry name
  = CreateAssociationBatchRequestEntry'{name,
                                        applyOnlyAtCronInterval = Core.Nothing,
                                        associationName = Core.Nothing,
                                        automationTargetParameterName = Core.Nothing,
                                        complianceSeverity = Core.Nothing,
                                        documentVersion = Core.Nothing, instanceId = Core.Nothing,
                                        maxConcurrency = Core.Nothing, maxErrors = Core.Nothing,
                                        outputLocation = Core.Nothing, parameters = Core.Nothing,
                                        scheduleExpression = Core.Nothing,
                                        syncCompliance = Core.Nothing, targets = Core.Nothing}

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
cabreName :: Lens.Lens' CreateAssociationBatchRequestEntry Types.Name
cabreName = Lens.field @"name"
{-# INLINEABLE cabreName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreApplyOnlyAtCronInterval :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Core.Bool)
cabreApplyOnlyAtCronInterval = Lens.field @"applyOnlyAtCronInterval"
{-# INLINEABLE cabreApplyOnlyAtCronInterval #-}
{-# DEPRECATED applyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead"  #-}

-- | Specify a descriptive name for the association.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreAssociationName :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.AssociationName)
cabreAssociationName = Lens.field @"associationName"
{-# INLINEABLE cabreAssociationName #-}
{-# DEPRECATED associationName "Use generic-lens or generic-optics with 'associationName' instead"  #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreAutomationTargetParameterName :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.AutomationTargetParameterName)
cabreAutomationTargetParameterName = Lens.field @"automationTargetParameterName"
{-# INLINEABLE cabreAutomationTargetParameterName #-}
{-# DEPRECATED automationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead"  #-}

-- | The severity level to assign to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreComplianceSeverity :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.AssociationComplianceSeverity)
cabreComplianceSeverity = Lens.field @"complianceSeverity"
{-# INLINEABLE cabreComplianceSeverity #-}
{-# DEPRECATED complianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead"  #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreDocumentVersion :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.DocumentVersion)
cabreDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE cabreDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | The ID of the instance. 
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreInstanceId :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.InstanceId)
cabreInstanceId = Lens.field @"instanceId"
{-# INLINEABLE cabreInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreMaxConcurrency :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.MaxConcurrency)
cabreMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE cabreMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreMaxErrors :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.MaxErrors)
cabreMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE cabreMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

-- | An S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreOutputLocation :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.InstanceAssociationOutputLocation)
cabreOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE cabreOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

-- | A description of the parameters for a document. 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreParameters :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
cabreParameters = Lens.field @"parameters"
{-# INLINEABLE cabreParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | A cron expression that specifies a schedule when the association runs.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreScheduleExpression :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.ScheduleExpression)
cabreScheduleExpression = Lens.field @"scheduleExpression"
{-# INLINEABLE cabreScheduleExpression #-}
{-# DEPRECATED scheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead"  #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ . 
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreSyncCompliance :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe Types.AssociationSyncCompliance)
cabreSyncCompliance = Lens.field @"syncCompliance"
{-# INLINEABLE cabreSyncCompliance #-}
{-# DEPRECATED syncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead"  #-}

-- | The instances targeted by the request.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabreTargets :: Lens.Lens' CreateAssociationBatchRequestEntry (Core.Maybe [Types.Target])
cabreTargets = Lens.field @"targets"
{-# INLINEABLE cabreTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

instance Core.FromJSON CreateAssociationBatchRequestEntry where
        toJSON CreateAssociationBatchRequestEntry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("ApplyOnlyAtCronInterval" Core..=) Core.<$>
                    applyOnlyAtCronInterval,
                  ("AssociationName" Core..=) Core.<$> associationName,
                  ("AutomationTargetParameterName" Core..=) Core.<$>
                    automationTargetParameterName,
                  ("ComplianceSeverity" Core..=) Core.<$> complianceSeverity,
                  ("DocumentVersion" Core..=) Core.<$> documentVersion,
                  ("InstanceId" Core..=) Core.<$> instanceId,
                  ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
                  ("MaxErrors" Core..=) Core.<$> maxErrors,
                  ("OutputLocation" Core..=) Core.<$> outputLocation,
                  ("Parameters" Core..=) Core.<$> parameters,
                  ("ScheduleExpression" Core..=) Core.<$> scheduleExpression,
                  ("SyncCompliance" Core..=) Core.<$> syncCompliance,
                  ("Targets" Core..=) Core.<$> targets])

instance Core.FromJSON CreateAssociationBatchRequestEntry where
        parseJSON
          = Core.withObject "CreateAssociationBatchRequestEntry" Core.$
              \ x ->
                CreateAssociationBatchRequestEntry' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..:? "ApplyOnlyAtCronInterval"
                    Core.<*> x Core..:? "AssociationName"
                    Core.<*> x Core..:? "AutomationTargetParameterName"
                    Core.<*> x Core..:? "ComplianceSeverity"
                    Core.<*> x Core..:? "DocumentVersion"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "MaxConcurrency"
                    Core.<*> x Core..:? "MaxErrors"
                    Core.<*> x Core..:? "OutputLocation"
                    Core.<*> x Core..:? "Parameters"
                    Core.<*> x Core..:? "ScheduleExpression"
                    Core.<*> x Core..:? "SyncCompliance"
                    Core.<*> x Core..:? "Targets"
