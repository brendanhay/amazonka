{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an association. You can update the association name and version, the document version, schedule, parameters, and Amazon S3 output. 
--
-- In order to call this API action, your IAM user account, group, or role must be configured with permission to call the 'DescribeAssociation' API action. If you don't have permission to call DescribeAssociation, then you receive the following error: @An error occurred (AccessDeniedException) when calling the UpdateAssociation operation: User: <user_arn> is not authorized to perform: ssm:DescribeAssociation on resource: <resource_arn>@ 
-- /Important:/ When you update an association, the association immediately runs against the specified targets.
module Network.AWS.SSM.UpdateAssociation
    (
    -- * Creating a request
      UpdateAssociation (..)
    , mkUpdateAssociation
    -- ** Request lenses
    , uaAssociationId
    , uaApplyOnlyAtCronInterval
    , uaAssociationName
    , uaAssociationVersion
    , uaAutomationTargetParameterName
    , uaComplianceSeverity
    , uaDocumentVersion
    , uaMaxConcurrency
    , uaMaxErrors
    , uaName
    , uaOutputLocation
    , uaParameters
    , uaScheduleExpression
    , uaSyncCompliance
    , uaTargets

    -- * Destructuring the response
    , UpdateAssociationResponse (..)
    , mkUpdateAssociationResponse
    -- ** Response lenses
    , uarrsAssociationDescription
    , uarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateAssociation' smart constructor.
data UpdateAssociation = UpdateAssociation'
  { associationId :: Types.AssociationId
    -- ^ The ID of the association you want to update. 
  , applyOnlyAtCronInterval :: Core.Maybe Core.Bool
    -- ^ By default, when you update an association, the system runs it immediately after it is updated and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you update it.
--
-- Also, if you specified this option when you created the association, you can reset it. To do so, specify the @no-apply-only-at-cron-interval@ parameter when you update the association from the command line. This parameter forces the association to run immediately after updating it and according to the interval specified.
  , associationName :: Core.Maybe Types.AssociationName
    -- ^ The name of the association that you want to update.
  , associationVersion :: Core.Maybe Types.AssociationVersion
    -- ^ This parameter is provided for concurrency control purposes. You must specify the latest association version in the service. If you want to ensure that this request succeeds, either specify @> LATEST@ , or omit this parameter.
  , automationTargetParameterName :: Core.Maybe Types.AutomationTargetParameterName
    -- ^ Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
  , complianceSeverity :: Core.Maybe Types.AssociationComplianceSeverity
    -- ^ The severity level to assign to the association.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The document version you want update for the association. 
  , maxConcurrency :: Core.Maybe Types.MaxConcurrency
    -- ^ The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
  , maxErrors :: Core.Maybe Types.MaxErrors
    -- ^ The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
  , name :: Core.Maybe Types.DocumentARN
    -- ^ The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account.
-- For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format:
-- @arn:aws:ssm:/region/ :/account-id/ :document//document-name/ @ 
-- For example:
-- @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@ 
-- For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
  , outputLocation :: Core.Maybe Types.InstanceAssociationOutputLocation
    -- ^ An S3 bucket where you want to store the results of this request.
  , parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue])
    -- ^ The parameters you want to update for the association. If you create a parameter using Parameter Store, you can reference the parameter using {{ssm:parameter-name}}
  , scheduleExpression :: Core.Maybe Types.ScheduleExpression
    -- ^ The cron expression used to schedule the association that you want to update.
  , syncCompliance :: Core.Maybe Types.AssociationSyncCompliance
    -- ^ The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
  , targets :: Core.Maybe [Types.Target]
    -- ^ The targets of the association.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAssociation' value with any optional fields omitted.
mkUpdateAssociation
    :: Types.AssociationId -- ^ 'associationId'
    -> UpdateAssociation
mkUpdateAssociation associationId
  = UpdateAssociation'{associationId,
                       applyOnlyAtCronInterval = Core.Nothing,
                       associationName = Core.Nothing, associationVersion = Core.Nothing,
                       automationTargetParameterName = Core.Nothing,
                       complianceSeverity = Core.Nothing, documentVersion = Core.Nothing,
                       maxConcurrency = Core.Nothing, maxErrors = Core.Nothing,
                       name = Core.Nothing, outputLocation = Core.Nothing,
                       parameters = Core.Nothing, scheduleExpression = Core.Nothing,
                       syncCompliance = Core.Nothing, targets = Core.Nothing}

-- | The ID of the association you want to update. 
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAssociationId :: Lens.Lens' UpdateAssociation Types.AssociationId
uaAssociationId = Lens.field @"associationId"
{-# INLINEABLE uaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | By default, when you update an association, the system runs it immediately after it is updated and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you update it.
--
-- Also, if you specified this option when you created the association, you can reset it. To do so, specify the @no-apply-only-at-cron-interval@ parameter when you update the association from the command line. This parameter forces the association to run immediately after updating it and according to the interval specified.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplyOnlyAtCronInterval :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Bool)
uaApplyOnlyAtCronInterval = Lens.field @"applyOnlyAtCronInterval"
{-# INLINEABLE uaApplyOnlyAtCronInterval #-}
{-# DEPRECATED applyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead"  #-}

-- | The name of the association that you want to update.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAssociationName :: Lens.Lens' UpdateAssociation (Core.Maybe Types.AssociationName)
uaAssociationName = Lens.field @"associationName"
{-# INLINEABLE uaAssociationName #-}
{-# DEPRECATED associationName "Use generic-lens or generic-optics with 'associationName' instead"  #-}

-- | This parameter is provided for concurrency control purposes. You must specify the latest association version in the service. If you want to ensure that this request succeeds, either specify @> LATEST@ , or omit this parameter.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAssociationVersion :: Lens.Lens' UpdateAssociation (Core.Maybe Types.AssociationVersion)
uaAssociationVersion = Lens.field @"associationVersion"
{-# INLINEABLE uaAssociationVersion #-}
{-# DEPRECATED associationVersion "Use generic-lens or generic-optics with 'associationVersion' instead"  #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAutomationTargetParameterName :: Lens.Lens' UpdateAssociation (Core.Maybe Types.AutomationTargetParameterName)
uaAutomationTargetParameterName = Lens.field @"automationTargetParameterName"
{-# INLINEABLE uaAutomationTargetParameterName #-}
{-# DEPRECATED automationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead"  #-}

-- | The severity level to assign to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaComplianceSeverity :: Lens.Lens' UpdateAssociation (Core.Maybe Types.AssociationComplianceSeverity)
uaComplianceSeverity = Lens.field @"complianceSeverity"
{-# INLINEABLE uaComplianceSeverity #-}
{-# DEPRECATED complianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead"  #-}

-- | The document version you want update for the association. 
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDocumentVersion :: Lens.Lens' UpdateAssociation (Core.Maybe Types.DocumentVersion)
uaDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE uaDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaMaxConcurrency :: Lens.Lens' UpdateAssociation (Core.Maybe Types.MaxConcurrency)
uaMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE uaMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaMaxErrors :: Lens.Lens' UpdateAssociation (Core.Maybe Types.MaxErrors)
uaMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE uaMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

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
uaName :: Lens.Lens' UpdateAssociation (Core.Maybe Types.DocumentARN)
uaName = Lens.field @"name"
{-# INLINEABLE uaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaOutputLocation :: Lens.Lens' UpdateAssociation (Core.Maybe Types.InstanceAssociationOutputLocation)
uaOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE uaOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

-- | The parameters you want to update for the association. If you create a parameter using Parameter Store, you can reference the parameter using {{ssm:parameter-name}}
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaParameters :: Lens.Lens' UpdateAssociation (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
uaParameters = Lens.field @"parameters"
{-# INLINEABLE uaParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The cron expression used to schedule the association that you want to update.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaScheduleExpression :: Lens.Lens' UpdateAssociation (Core.Maybe Types.ScheduleExpression)
uaScheduleExpression = Lens.field @"scheduleExpression"
{-# INLINEABLE uaScheduleExpression #-}
{-# DEPRECATED scheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead"  #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaSyncCompliance :: Lens.Lens' UpdateAssociation (Core.Maybe Types.AssociationSyncCompliance)
uaSyncCompliance = Lens.field @"syncCompliance"
{-# INLINEABLE uaSyncCompliance #-}
{-# DEPRECATED syncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead"  #-}

-- | The targets of the association.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTargets :: Lens.Lens' UpdateAssociation (Core.Maybe [Types.Target])
uaTargets = Lens.field @"targets"
{-# INLINEABLE uaTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

instance Core.ToQuery UpdateAssociation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAssociation where
        toHeaders UpdateAssociation{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.UpdateAssociation") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAssociation where
        toJSON UpdateAssociation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AssociationId" Core..= associationId),
                  ("ApplyOnlyAtCronInterval" Core..=) Core.<$>
                    applyOnlyAtCronInterval,
                  ("AssociationName" Core..=) Core.<$> associationName,
                  ("AssociationVersion" Core..=) Core.<$> associationVersion,
                  ("AutomationTargetParameterName" Core..=) Core.<$>
                    automationTargetParameterName,
                  ("ComplianceSeverity" Core..=) Core.<$> complianceSeverity,
                  ("DocumentVersion" Core..=) Core.<$> documentVersion,
                  ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
                  ("MaxErrors" Core..=) Core.<$> maxErrors,
                  ("Name" Core..=) Core.<$> name,
                  ("OutputLocation" Core..=) Core.<$> outputLocation,
                  ("Parameters" Core..=) Core.<$> parameters,
                  ("ScheduleExpression" Core..=) Core.<$> scheduleExpression,
                  ("SyncCompliance" Core..=) Core.<$> syncCompliance,
                  ("Targets" Core..=) Core.<$> targets])

instance Core.AWSRequest UpdateAssociation where
        type Rs UpdateAssociation = UpdateAssociationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAssociationResponse' Core.<$>
                   (x Core..:? "AssociationDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAssociationResponse' smart constructor.
data UpdateAssociationResponse = UpdateAssociationResponse'
  { associationDescription :: Core.Maybe Types.AssociationDescription
    -- ^ The description of the association that was updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateAssociationResponse' value with any optional fields omitted.
mkUpdateAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateAssociationResponse
mkUpdateAssociationResponse responseStatus
  = UpdateAssociationResponse'{associationDescription = Core.Nothing,
                               responseStatus}

-- | The description of the association that was updated.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsAssociationDescription :: Lens.Lens' UpdateAssociationResponse (Core.Maybe Types.AssociationDescription)
uarrsAssociationDescription = Lens.field @"associationDescription"
{-# INLINEABLE uarrsAssociationDescription #-}
{-# DEPRECATED associationDescription "Use generic-lens or generic-optics with 'associationDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateAssociationResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
