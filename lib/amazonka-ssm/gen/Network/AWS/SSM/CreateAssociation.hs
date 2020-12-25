{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A State Manager association defines the state that you want to maintain on your instances. For example, an association can specify that anti-virus software must be installed and running on your instances, or that certain ports must be closed. For static targets, the association specifies a schedule for when the configuration is reapplied. For dynamic targets, such as an AWS Resource Group or an AWS Autoscaling Group, State Manager applies the configuration when new instances are added to the group. The association also specifies actions to take when applying the configuration. For example, an association for anti-virus software might run once a day. If the software is not installed, then State Manager installs it. If the software is installed, but the service is not running, then the association might instruct State Manager to start the service.
module Network.AWS.SSM.CreateAssociation
  ( -- * Creating a request
    CreateAssociation (..),
    mkCreateAssociation,

    -- ** Request lenses
    caName,
    caApplyOnlyAtCronInterval,
    caAssociationName,
    caAutomationTargetParameterName,
    caComplianceSeverity,
    caDocumentVersion,
    caInstanceId,
    caMaxConcurrency,
    caMaxErrors,
    caOutputLocation,
    caParameters,
    caScheduleExpression,
    caSyncCompliance,
    caTargets,

    -- * Destructuring the response
    CreateAssociationResponse (..),
    mkCreateAssociationResponse,

    -- ** Response lenses
    carrsAssociationDescription,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreateAssociation' smart constructor.
data CreateAssociation = CreateAssociation'
  { -- | The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents.
    --
    -- You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account.
    -- For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format:
    -- @arn:/partition/ :ssm:/region/ :/account-id/ :document//document-name/ @
    -- For example:
    -- @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@
    -- For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
    name :: Types.Name,
    -- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
    applyOnlyAtCronInterval :: Core.Maybe Core.Bool,
    -- | Specify a descriptive name for the association.
    associationName :: Core.Maybe Types.AssociationName,
    -- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
    automationTargetParameterName :: Core.Maybe Types.AutomationTargetParameterName,
    -- | The severity level to assign to the association.
    complianceSeverity :: Core.Maybe Types.AssociationComplianceSeverity,
    -- | The document version you want to associate with the target(s). Can be a specific version or the default version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The instance ID.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
    --
    -- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
    maxConcurrency :: Core.Maybe Types.MaxConcurrency,
    -- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
    --
    -- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
    maxErrors :: Core.Maybe Types.MaxErrors,
    -- | An S3 bucket where you want to store the output details of the request.
    outputLocation :: Core.Maybe Types.InstanceAssociationOutputLocation,
    -- | The parameters for the runtime configuration of the document.
    parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]),
    -- | A cron expression when the association will be applied to the target(s).
    scheduleExpression :: Core.Maybe Types.ScheduleExpression,
    -- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
    --
    -- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
    -- By default, all associations use @AUTO@ mode.
    syncCompliance :: Core.Maybe Types.AssociationSyncCompliance,
    -- | The targets for the association. You can target instances by using tags, AWS Resource Groups, all instances in an AWS account, or individual instance IDs. For more information about choosing targets for an association, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations> in the /AWS Systems Manager User Guide/ .
    targets :: Core.Maybe [Types.Target]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAssociation' value with any optional fields omitted.
mkCreateAssociation ::
  -- | 'name'
  Types.Name ->
  CreateAssociation
mkCreateAssociation name =
  CreateAssociation'
    { name,
      applyOnlyAtCronInterval = Core.Nothing,
      associationName = Core.Nothing,
      automationTargetParameterName = Core.Nothing,
      complianceSeverity = Core.Nothing,
      documentVersion = Core.Nothing,
      instanceId = Core.Nothing,
      maxConcurrency = Core.Nothing,
      maxErrors = Core.Nothing,
      outputLocation = Core.Nothing,
      parameters = Core.Nothing,
      scheduleExpression = Core.Nothing,
      syncCompliance = Core.Nothing,
      targets = Core.Nothing
    }

-- | The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account.
-- For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format:
-- @arn:/partition/ :ssm:/region/ :/account-id/ :document//document-name/ @
-- For example:
-- @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@
-- For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateAssociation Types.Name
caName = Lens.field @"name"
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplyOnlyAtCronInterval :: Lens.Lens' CreateAssociation (Core.Maybe Core.Bool)
caApplyOnlyAtCronInterval = Lens.field @"applyOnlyAtCronInterval"
{-# DEPRECATED caApplyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead." #-}

-- | Specify a descriptive name for the association.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAssociationName :: Lens.Lens' CreateAssociation (Core.Maybe Types.AssociationName)
caAssociationName = Lens.field @"associationName"
{-# DEPRECATED caAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAutomationTargetParameterName :: Lens.Lens' CreateAssociation (Core.Maybe Types.AutomationTargetParameterName)
caAutomationTargetParameterName = Lens.field @"automationTargetParameterName"
{-# DEPRECATED caAutomationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead." #-}

-- | The severity level to assign to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caComplianceSeverity :: Lens.Lens' CreateAssociation (Core.Maybe Types.AssociationComplianceSeverity)
caComplianceSeverity = Lens.field @"complianceSeverity"
{-# DEPRECATED caComplianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead." #-}

-- | The document version you want to associate with the target(s). Can be a specific version or the default version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDocumentVersion :: Lens.Lens' CreateAssociation (Core.Maybe Types.DocumentVersion)
caDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED caDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caInstanceId :: Lens.Lens' CreateAssociation (Core.Maybe Types.InstanceId)
caInstanceId = Lens.field @"instanceId"
{-# DEPRECATED caInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caMaxConcurrency :: Lens.Lens' CreateAssociation (Core.Maybe Types.MaxConcurrency)
caMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED caMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caMaxErrors :: Lens.Lens' CreateAssociation (Core.Maybe Types.MaxErrors)
caMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED caMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | An S3 bucket where you want to store the output details of the request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOutputLocation :: Lens.Lens' CreateAssociation (Core.Maybe Types.InstanceAssociationOutputLocation)
caOutputLocation = Lens.field @"outputLocation"
{-# DEPRECATED caOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The parameters for the runtime configuration of the document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caParameters :: Lens.Lens' CreateAssociation (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
caParameters = Lens.field @"parameters"
{-# DEPRECATED caParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A cron expression when the association will be applied to the target(s).
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caScheduleExpression :: Lens.Lens' CreateAssociation (Core.Maybe Types.ScheduleExpression)
caScheduleExpression = Lens.field @"scheduleExpression"
{-# DEPRECATED caScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSyncCompliance :: Lens.Lens' CreateAssociation (Core.Maybe Types.AssociationSyncCompliance)
caSyncCompliance = Lens.field @"syncCompliance"
{-# DEPRECATED caSyncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead." #-}

-- | The targets for the association. You can target instances by using tags, AWS Resource Groups, all instances in an AWS account, or individual instance IDs. For more information about choosing targets for an association, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTargets :: Lens.Lens' CreateAssociation (Core.Maybe [Types.Target])
caTargets = Lens.field @"targets"
{-# DEPRECATED caTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Core.FromJSON CreateAssociation where
  toJSON CreateAssociation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("ApplyOnlyAtCronInterval" Core..=)
              Core.<$> applyOnlyAtCronInterval,
            ("AssociationName" Core..=) Core.<$> associationName,
            ("AutomationTargetParameterName" Core..=)
              Core.<$> automationTargetParameterName,
            ("ComplianceSeverity" Core..=) Core.<$> complianceSeverity,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("InstanceId" Core..=) Core.<$> instanceId,
            ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
            ("MaxErrors" Core..=) Core.<$> maxErrors,
            ("OutputLocation" Core..=) Core.<$> outputLocation,
            ("Parameters" Core..=) Core.<$> parameters,
            ("ScheduleExpression" Core..=) Core.<$> scheduleExpression,
            ("SyncCompliance" Core..=) Core.<$> syncCompliance,
            ("Targets" Core..=) Core.<$> targets
          ]
      )

instance Core.AWSRequest CreateAssociation where
  type Rs CreateAssociation = CreateAssociationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.CreateAssociation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssociationResponse'
            Core.<$> (x Core..:? "AssociationDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAssociationResponse' smart constructor.
data CreateAssociationResponse = CreateAssociationResponse'
  { -- | Information about the association.
    associationDescription :: Core.Maybe Types.AssociationDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateAssociationResponse' value with any optional fields omitted.
mkCreateAssociationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAssociationResponse
mkCreateAssociationResponse responseStatus =
  CreateAssociationResponse'
    { associationDescription = Core.Nothing,
      responseStatus
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAssociationDescription :: Lens.Lens' CreateAssociationResponse (Core.Maybe Types.AssociationDescription)
carrsAssociationDescription = Lens.field @"associationDescription"
{-# DEPRECATED carrsAssociationDescription "Use generic-lens or generic-optics with 'associationDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAssociationResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
