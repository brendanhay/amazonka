{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    caInstanceId,
    caApplyOnlyAtCronInterval,
    caMaxErrors,
    caScheduleExpression,
    caOutputLocation,
    caSyncCompliance,
    caTargets,
    caParameters,
    caDocumentVersion,
    caAutomationTargetParameterName,
    caAssociationName,
    caComplianceSeverity,
    caMaxConcurrency,
    caName,

    -- * Destructuring the response
    CreateAssociationResponse (..),
    mkCreateAssociationResponse,

    -- ** Response lenses
    crsAssociationDescription,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreateAssociation' smart constructor.
data CreateAssociation = CreateAssociation'
  { instanceId ::
      Lude.Maybe Lude.Text,
    applyOnlyAtCronInterval :: Lude.Maybe Lude.Bool,
    maxErrors :: Lude.Maybe Lude.Text,
    scheduleExpression :: Lude.Maybe Lude.Text,
    outputLocation ::
      Lude.Maybe InstanceAssociationOutputLocation,
    syncCompliance :: Lude.Maybe AssociationSyncCompliance,
    targets :: Lude.Maybe [Target],
    parameters ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    documentVersion :: Lude.Maybe Lude.Text,
    automationTargetParameterName :: Lude.Maybe Lude.Text,
    associationName :: Lude.Maybe Lude.Text,
    complianceSeverity ::
      Lude.Maybe AssociationComplianceSeverity,
    maxConcurrency :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateAssociation' with the minimum fields required to make a request.
--
-- * 'applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
-- * 'associationName' - Specify a descriptive name for the association.
-- * 'automationTargetParameterName' - Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
-- * 'complianceSeverity' - The severity level to assign to the association.
-- * 'documentVersion' - The document version you want to associate with the target(s). Can be a specific version or the default version.
-- * 'instanceId' - The instance ID.
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
-- @arn:/partition/ :ssm:/region/ :/account-id/ :document//document-name/ @
-- For example:
-- @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@
-- For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
-- * 'outputLocation' - An S3 bucket where you want to store the output details of the request.
-- * 'parameters' - The parameters for the runtime configuration of the document.
-- * 'scheduleExpression' - A cron expression when the association will be applied to the target(s).
-- * 'syncCompliance' - The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
-- * 'targets' - The targets for the association. You can target instances by using tags, AWS Resource Groups, all instances in an AWS account, or individual instance IDs. For more information about choosing targets for an association, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations> in the /AWS Systems Manager User Guide/ .
mkCreateAssociation ::
  -- | 'name'
  Lude.Text ->
  CreateAssociation
mkCreateAssociation pName_ =
  CreateAssociation'
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

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caInstanceId :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Text)
caInstanceId = Lens.lens (instanceId :: CreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CreateAssociation)
{-# DEPRECATED caInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplyOnlyAtCronInterval :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Bool)
caApplyOnlyAtCronInterval = Lens.lens (applyOnlyAtCronInterval :: CreateAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {applyOnlyAtCronInterval = a} :: CreateAssociation)
{-# DEPRECATED caApplyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead." #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caMaxErrors :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Text)
caMaxErrors = Lens.lens (maxErrors :: CreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: CreateAssociation)
{-# DEPRECATED caMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | A cron expression when the association will be applied to the target(s).
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caScheduleExpression :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Text)
caScheduleExpression = Lens.lens (scheduleExpression :: CreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: CreateAssociation)
{-# DEPRECATED caScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | An S3 bucket where you want to store the output details of the request.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOutputLocation :: Lens.Lens' CreateAssociation (Lude.Maybe InstanceAssociationOutputLocation)
caOutputLocation = Lens.lens (outputLocation :: CreateAssociation -> Lude.Maybe InstanceAssociationOutputLocation) (\s a -> s {outputLocation = a} :: CreateAssociation)
{-# DEPRECATED caOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSyncCompliance :: Lens.Lens' CreateAssociation (Lude.Maybe AssociationSyncCompliance)
caSyncCompliance = Lens.lens (syncCompliance :: CreateAssociation -> Lude.Maybe AssociationSyncCompliance) (\s a -> s {syncCompliance = a} :: CreateAssociation)
{-# DEPRECATED caSyncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead." #-}

-- | The targets for the association. You can target instances by using tags, AWS Resource Groups, all instances in an AWS account, or individual instance IDs. For more information about choosing targets for an association, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTargets :: Lens.Lens' CreateAssociation (Lude.Maybe [Target])
caTargets = Lens.lens (targets :: CreateAssociation -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: CreateAssociation)
{-# DEPRECATED caTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The parameters for the runtime configuration of the document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caParameters :: Lens.Lens' CreateAssociation (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
caParameters = Lens.lens (parameters :: CreateAssociation -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: CreateAssociation)
{-# DEPRECATED caParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The document version you want to associate with the target(s). Can be a specific version or the default version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDocumentVersion :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Text)
caDocumentVersion = Lens.lens (documentVersion :: CreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: CreateAssociation)
{-# DEPRECATED caDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- /Note:/ Consider using 'automationTargetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAutomationTargetParameterName :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Text)
caAutomationTargetParameterName = Lens.lens (automationTargetParameterName :: CreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {automationTargetParameterName = a} :: CreateAssociation)
{-# DEPRECATED caAutomationTargetParameterName "Use generic-lens or generic-optics with 'automationTargetParameterName' instead." #-}

-- | Specify a descriptive name for the association.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAssociationName :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Text)
caAssociationName = Lens.lens (associationName :: CreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: CreateAssociation)
{-# DEPRECATED caAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | The severity level to assign to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caComplianceSeverity :: Lens.Lens' CreateAssociation (Lude.Maybe AssociationComplianceSeverity)
caComplianceSeverity = Lens.lens (complianceSeverity :: CreateAssociation -> Lude.Maybe AssociationComplianceSeverity) (\s a -> s {complianceSeverity = a} :: CreateAssociation)
{-# DEPRECATED caComplianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead." #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caMaxConcurrency :: Lens.Lens' CreateAssociation (Lude.Maybe Lude.Text)
caMaxConcurrency = Lens.lens (maxConcurrency :: CreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: CreateAssociation)
{-# DEPRECATED caMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

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
caName :: Lens.Lens' CreateAssociation Lude.Text
caName = Lens.lens (name :: CreateAssociation -> Lude.Text) (\s a -> s {name = a} :: CreateAssociation)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateAssociation where
  type Rs CreateAssociation = CreateAssociationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAssociationResponse'
            Lude.<$> (x Lude..?> "AssociationDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreateAssociation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAssociation where
  toJSON CreateAssociation' {..} =
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

instance Lude.ToPath CreateAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAssociationResponse' smart constructor.
data CreateAssociationResponse = CreateAssociationResponse'
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

-- | Creates a value of 'CreateAssociationResponse' with the minimum fields required to make a request.
--
-- * 'associationDescription' - Information about the association.
-- * 'responseStatus' - The response status code.
mkCreateAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAssociationResponse
mkCreateAssociationResponse pResponseStatus_ =
  CreateAssociationResponse'
    { associationDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsAssociationDescription :: Lens.Lens' CreateAssociationResponse (Lude.Maybe AssociationDescription)
crsAssociationDescription = Lens.lens (associationDescription :: CreateAssociationResponse -> Lude.Maybe AssociationDescription) (\s a -> s {associationDescription = a} :: CreateAssociationResponse)
{-# DEPRECATED crsAssociationDescription "Use generic-lens or generic-optics with 'associationDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateAssociationResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAssociationResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
