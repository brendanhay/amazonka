{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateAssociation (..),
    mkUpdateAssociation,

    -- ** Request lenses
    uaApplyOnlyAtCronInterval,
    uaMaxErrors,
    uaScheduleExpression,
    uaName,
    uaOutputLocation,
    uaSyncCompliance,
    uaTargets,
    uaParameters,
    uaDocumentVersion,
    uaAutomationTargetParameterName,
    uaAssociationVersion,
    uaAssociationName,
    uaComplianceSeverity,
    uaMaxConcurrency,
    uaAssociationId,

    -- * Destructuring the response
    UpdateAssociationResponse (..),
    mkUpdateAssociationResponse,

    -- ** Response lenses
    uarsAssociationDescription,
    uarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateAssociation' smart constructor.
data UpdateAssociation = UpdateAssociation'
  { applyOnlyAtCronInterval ::
      Lude.Maybe Lude.Bool,
    maxErrors :: Lude.Maybe Lude.Text,
    scheduleExpression :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    outputLocation ::
      Lude.Maybe InstanceAssociationOutputLocation,
    syncCompliance :: Lude.Maybe AssociationSyncCompliance,
    targets :: Lude.Maybe [Target],
    parameters ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    documentVersion :: Lude.Maybe Lude.Text,
    automationTargetParameterName :: Lude.Maybe Lude.Text,
    associationVersion :: Lude.Maybe Lude.Text,
    associationName :: Lude.Maybe Lude.Text,
    complianceSeverity ::
      Lude.Maybe AssociationComplianceSeverity,
    maxConcurrency :: Lude.Maybe Lude.Text,
    associationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAssociation' with the minimum fields required to make a request.
--
-- * 'applyOnlyAtCronInterval' - By default, when you update an association, the system runs it immediately after it is updated and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you update it.
--
-- Also, if you specified this option when you created the association, you can reset it. To do so, specify the @no-apply-only-at-cron-interval@ parameter when you update the association from the command line. This parameter forces the association to run immediately after updating it and according to the interval specified.
-- * 'associationId' - The ID of the association you want to update.
-- * 'associationName' - The name of the association that you want to update.
-- * 'associationVersion' - This parameter is provided for concurrency control purposes. You must specify the latest association version in the service. If you want to ensure that this request succeeds, either specify @> LATEST@ , or omit this parameter.
-- * 'automationTargetParameterName' - Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
-- * 'complianceSeverity' - The severity level to assign to the association.
-- * 'documentVersion' - The document version you want update for the association.
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
-- * 'parameters' - The parameters you want to update for the association. If you create a parameter using Parameter Store, you can reference the parameter using {{ssm:parameter-name}}
-- * 'scheduleExpression' - The cron expression used to schedule the association that you want to update.
-- * 'syncCompliance' - The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
-- * 'targets' - The targets of the association.
mkUpdateAssociation ::
  -- | 'associationId'
  Lude.Text ->
  UpdateAssociation
mkUpdateAssociation pAssociationId_ =
  UpdateAssociation'
    { applyOnlyAtCronInterval = Lude.Nothing,
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
      maxConcurrency = Lude.Nothing,
      associationId = pAssociationId_
    }

-- | By default, when you update an association, the system runs it immediately after it is updated and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you update it.
--
-- Also, if you specified this option when you created the association, you can reset it. To do so, specify the @no-apply-only-at-cron-interval@ parameter when you update the association from the command line. This parameter forces the association to run immediately after updating it and according to the interval specified.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplyOnlyAtCronInterval :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Bool)
uaApplyOnlyAtCronInterval = Lens.lens (applyOnlyAtCronInterval :: UpdateAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {applyOnlyAtCronInterval = a} :: UpdateAssociation)
{-# DEPRECATED uaApplyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead." #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaMaxErrors :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaMaxErrors = Lens.lens (maxErrors :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: UpdateAssociation)
{-# DEPRECATED uaMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The cron expression used to schedule the association that you want to update.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaScheduleExpression :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaScheduleExpression = Lens.lens (scheduleExpression :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: UpdateAssociation)
{-# DEPRECATED uaScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

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
uaName :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaName = Lens.lens (name :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateAssociation)
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaOutputLocation :: Lens.Lens' UpdateAssociation (Lude.Maybe InstanceAssociationOutputLocation)
uaOutputLocation = Lens.lens (outputLocation :: UpdateAssociation -> Lude.Maybe InstanceAssociationOutputLocation) (\s a -> s {outputLocation = a} :: UpdateAssociation)
{-# DEPRECATED uaOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaSyncCompliance :: Lens.Lens' UpdateAssociation (Lude.Maybe AssociationSyncCompliance)
uaSyncCompliance = Lens.lens (syncCompliance :: UpdateAssociation -> Lude.Maybe AssociationSyncCompliance) (\s a -> s {syncCompliance = a} :: UpdateAssociation)
{-# DEPRECATED uaSyncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead." #-}

-- | The targets of the association.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTargets :: Lens.Lens' UpdateAssociation (Lude.Maybe [Target])
uaTargets = Lens.lens (targets :: UpdateAssociation -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: UpdateAssociation)
{-# DEPRECATED uaTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The parameters you want to update for the association. If you create a parameter using Parameter Store, you can reference the parameter using {{ssm:parameter-name}}
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaParameters :: Lens.Lens' UpdateAssociation (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
uaParameters = Lens.lens (parameters :: UpdateAssociation -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: UpdateAssociation)
{-# DEPRECATED uaParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The document version you want update for the association.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDocumentVersion :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaDocumentVersion = Lens.lens (documentVersion :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: UpdateAssociation)
{-# DEPRECATED uaDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAutomationTargetParameterName :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaAutomationTargetParameterName = Lens.lens (automationTargetParameterName :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {automationTargetParameterName = a} :: UpdateAssociation)
{-# DEPRECATED uaAutomationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead." #-}

-- | This parameter is provided for concurrency control purposes. You must specify the latest association version in the service. If you want to ensure that this request succeeds, either specify @> LATEST@ , or omit this parameter.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAssociationVersion :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaAssociationVersion = Lens.lens (associationVersion :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: UpdateAssociation)
{-# DEPRECATED uaAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The name of the association that you want to update.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAssociationName :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaAssociationName = Lens.lens (associationName :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: UpdateAssociation)
{-# DEPRECATED uaAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | The severity level to assign to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaComplianceSeverity :: Lens.Lens' UpdateAssociation (Lude.Maybe AssociationComplianceSeverity)
uaComplianceSeverity = Lens.lens (complianceSeverity :: UpdateAssociation -> Lude.Maybe AssociationComplianceSeverity) (\s a -> s {complianceSeverity = a} :: UpdateAssociation)
{-# DEPRECATED uaComplianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead." #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaMaxConcurrency :: Lens.Lens' UpdateAssociation (Lude.Maybe Lude.Text)
uaMaxConcurrency = Lens.lens (maxConcurrency :: UpdateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: UpdateAssociation)
{-# DEPRECATED uaMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The ID of the association you want to update.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAssociationId :: Lens.Lens' UpdateAssociation Lude.Text
uaAssociationId = Lens.lens (associationId :: UpdateAssociation -> Lude.Text) (\s a -> s {associationId = a} :: UpdateAssociation)
{-# DEPRECATED uaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Lude.AWSRequest UpdateAssociation where
  type Rs UpdateAssociation = UpdateAssociationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAssociationResponse'
            Lude.<$> (x Lude..?> "AssociationDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateAssociation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAssociation where
  toJSON UpdateAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ApplyOnlyAtCronInterval" Lude..=)
              Lude.<$> applyOnlyAtCronInterval,
            ("MaxErrors" Lude..=) Lude.<$> maxErrors,
            ("ScheduleExpression" Lude..=) Lude.<$> scheduleExpression,
            ("Name" Lude..=) Lude.<$> name,
            ("OutputLocation" Lude..=) Lude.<$> outputLocation,
            ("SyncCompliance" Lude..=) Lude.<$> syncCompliance,
            ("Targets" Lude..=) Lude.<$> targets,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            ("AutomationTargetParameterName" Lude..=)
              Lude.<$> automationTargetParameterName,
            ("AssociationVersion" Lude..=) Lude.<$> associationVersion,
            ("AssociationName" Lude..=) Lude.<$> associationName,
            ("ComplianceSeverity" Lude..=) Lude.<$> complianceSeverity,
            ("MaxConcurrency" Lude..=) Lude.<$> maxConcurrency,
            Lude.Just ("AssociationId" Lude..= associationId)
          ]
      )

instance Lude.ToPath UpdateAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAssociationResponse' smart constructor.
data UpdateAssociationResponse = UpdateAssociationResponse'
  { associationDescription ::
      Lude.Maybe AssociationDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAssociationResponse' with the minimum fields required to make a request.
--
-- * 'associationDescription' - The description of the association that was updated.
-- * 'responseStatus' - The response status code.
mkUpdateAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAssociationResponse
mkUpdateAssociationResponse pResponseStatus_ =
  UpdateAssociationResponse'
    { associationDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The description of the association that was updated.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsAssociationDescription :: Lens.Lens' UpdateAssociationResponse (Lude.Maybe AssociationDescription)
uarsAssociationDescription = Lens.lens (associationDescription :: UpdateAssociationResponse -> Lude.Maybe AssociationDescription) (\s a -> s {associationDescription = a} :: UpdateAssociationResponse)
{-# DEPRECATED uarsAssociationDescription "Use generic-lens or generic-optics with 'associationDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateAssociationResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAssociationResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
