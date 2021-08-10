{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A State Manager association defines the state that you want to maintain
-- on your instances. For example, an association can specify that
-- anti-virus software must be installed and running on your instances, or
-- that certain ports must be closed. For static targets, the association
-- specifies a schedule for when the configuration is reapplied. For
-- dynamic targets, such as an AWS Resource Group or an AWS Autoscaling
-- Group, State Manager applies the configuration when new instances are
-- added to the group. The association also specifies actions to take when
-- applying the configuration. For example, an association for anti-virus
-- software might run once a day. If the software is not installed, then
-- State Manager installs it. If the software is installed, but the service
-- is not running, then the association might instruct State Manager to
-- start the service.
module Network.AWS.SSM.CreateAssociation
  ( -- * Creating a Request
    CreateAssociation (..),
    newCreateAssociation,

    -- * Request Lenses
    createAssociation_maxErrors,
    createAssociation_instanceId,
    createAssociation_complianceSeverity,
    createAssociation_automationTargetParameterName,
    createAssociation_targets,
    createAssociation_targetLocations,
    createAssociation_scheduleExpression,
    createAssociation_maxConcurrency,
    createAssociation_associationName,
    createAssociation_documentVersion,
    createAssociation_parameters,
    createAssociation_outputLocation,
    createAssociation_applyOnlyAtCronInterval,
    createAssociation_syncCompliance,
    createAssociation_name,

    -- * Destructuring the Response
    CreateAssociationResponse (..),
    newCreateAssociationResponse,

    -- * Response Lenses
    createAssociationResponse_associationDescription,
    createAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateAssociation' smart constructor.
data CreateAssociation = CreateAssociation'
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
    -- | The instance ID.
    --
    -- @InstanceId@ has been deprecated. To specify an instance ID for an
    -- association, use the @Targets@ parameter. Requests that include the
    -- parameter @InstanceID@ with SSM documents that use schema version 2.0 or
    -- later will fail. In addition, if you use the parameter @InstanceId@, you
    -- cannot use the parameters @AssociationName@, @DocumentVersion@,
    -- @MaxErrors@, @MaxConcurrency@, @OutputLocation@, or
    -- @ScheduleExpression@. To use these parameters, you must use the
    -- @Targets@ parameter.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The severity level to assign to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
    -- | Specify the target for the association. This target is required for
    -- associations that use an Automation document and target resources by
    -- using rate controls.
    automationTargetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The targets for the association. You can target instances by using tags,
    -- AWS Resource Groups, all instances in an AWS account, or individual
    -- instance IDs. For more information about choosing targets for an
    -- association, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations>
    -- in the /AWS Systems Manager User Guide/.
    targets :: Prelude.Maybe [Target],
    -- | A location is a combination of AWS Regions and AWS accounts where you
    -- want to run the association. Use this action to create an association in
    -- multiple Regions and multiple accounts.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | A cron expression when the association will be applied to the target(s).
    scheduleExpression :: Prelude.Maybe Prelude.Text,
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
    -- | Specify a descriptive name for the association.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The document version you want to associate with the target(s). Can be a
    -- specific version or the default version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the runtime configuration of the document.
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
    syncCompliance :: Prelude.Maybe AssociationSyncCompliance,
    -- | The name of the SSM document that contains the configuration information
    -- for the instance. You can specify Command or Automation documents.
    --
    -- You can specify AWS-predefined documents, documents you created, or a
    -- document that is shared with you from another account.
    --
    -- For SSM documents that are shared with you from other AWS accounts, you
    -- must specify the complete SSM document ARN, in the following format:
    --
    -- @arn:partition:ssm:region:account-id:document\/document-name @
    --
    -- For example:
    --
    -- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
    --
    -- For AWS-predefined documents and SSM documents you created in your
    -- account, you only need to specify the document name. For example,
    -- @AWS-ApplyPatchBaseline@ or @My-Document@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'createAssociation_maxErrors' - The number of errors that are allowed before the system stops sending
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
-- 'instanceId', 'createAssociation_instanceId' - The instance ID.
--
-- @InstanceId@ has been deprecated. To specify an instance ID for an
-- association, use the @Targets@ parameter. Requests that include the
-- parameter @InstanceID@ with SSM documents that use schema version 2.0 or
-- later will fail. In addition, if you use the parameter @InstanceId@, you
-- cannot use the parameters @AssociationName@, @DocumentVersion@,
-- @MaxErrors@, @MaxConcurrency@, @OutputLocation@, or
-- @ScheduleExpression@. To use these parameters, you must use the
-- @Targets@ parameter.
--
-- 'complianceSeverity', 'createAssociation_complianceSeverity' - The severity level to assign to the association.
--
-- 'automationTargetParameterName', 'createAssociation_automationTargetParameterName' - Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
--
-- 'targets', 'createAssociation_targets' - The targets for the association. You can target instances by using tags,
-- AWS Resource Groups, all instances in an AWS account, or individual
-- instance IDs. For more information about choosing targets for an
-- association, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations>
-- in the /AWS Systems Manager User Guide/.
--
-- 'targetLocations', 'createAssociation_targetLocations' - A location is a combination of AWS Regions and AWS accounts where you
-- want to run the association. Use this action to create an association in
-- multiple Regions and multiple accounts.
--
-- 'scheduleExpression', 'createAssociation_scheduleExpression' - A cron expression when the association will be applied to the target(s).
--
-- 'maxConcurrency', 'createAssociation_maxConcurrency' - The maximum number of targets allowed to run the association at the same
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
-- 'associationName', 'createAssociation_associationName' - Specify a descriptive name for the association.
--
-- 'documentVersion', 'createAssociation_documentVersion' - The document version you want to associate with the target(s). Can be a
-- specific version or the default version.
--
-- 'parameters', 'createAssociation_parameters' - The parameters for the runtime configuration of the document.
--
-- 'outputLocation', 'createAssociation_outputLocation' - An S3 bucket where you want to store the output details of the request.
--
-- 'applyOnlyAtCronInterval', 'createAssociation_applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter is not supported for
-- rate expressions.
--
-- 'syncCompliance', 'createAssociation_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
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
--
-- 'name', 'createAssociation_name' - The name of the SSM document that contains the configuration information
-- for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a
-- document that is shared with you from another account.
--
-- For SSM documents that are shared with you from other AWS accounts, you
-- must specify the complete SSM document ARN, in the following format:
--
-- @arn:partition:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For AWS-predefined documents and SSM documents you created in your
-- account, you only need to specify the document name. For example,
-- @AWS-ApplyPatchBaseline@ or @My-Document@.
newCreateAssociation ::
  -- | 'name'
  Prelude.Text ->
  CreateAssociation
newCreateAssociation pName_ =
  CreateAssociation'
    { maxErrors = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      automationTargetParameterName = Prelude.Nothing,
      targets = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      associationName = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      applyOnlyAtCronInterval = Prelude.Nothing,
      syncCompliance = Prelude.Nothing,
      name = pName_
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
createAssociation_maxErrors :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_maxErrors = Lens.lens (\CreateAssociation' {maxErrors} -> maxErrors) (\s@CreateAssociation' {} a -> s {maxErrors = a} :: CreateAssociation)

-- | The instance ID.
--
-- @InstanceId@ has been deprecated. To specify an instance ID for an
-- association, use the @Targets@ parameter. Requests that include the
-- parameter @InstanceID@ with SSM documents that use schema version 2.0 or
-- later will fail. In addition, if you use the parameter @InstanceId@, you
-- cannot use the parameters @AssociationName@, @DocumentVersion@,
-- @MaxErrors@, @MaxConcurrency@, @OutputLocation@, or
-- @ScheduleExpression@. To use these parameters, you must use the
-- @Targets@ parameter.
createAssociation_instanceId :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_instanceId = Lens.lens (\CreateAssociation' {instanceId} -> instanceId) (\s@CreateAssociation' {} a -> s {instanceId = a} :: CreateAssociation)

-- | The severity level to assign to the association.
createAssociation_complianceSeverity :: Lens.Lens' CreateAssociation (Prelude.Maybe AssociationComplianceSeverity)
createAssociation_complianceSeverity = Lens.lens (\CreateAssociation' {complianceSeverity} -> complianceSeverity) (\s@CreateAssociation' {} a -> s {complianceSeverity = a} :: CreateAssociation)

-- | Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
createAssociation_automationTargetParameterName :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_automationTargetParameterName = Lens.lens (\CreateAssociation' {automationTargetParameterName} -> automationTargetParameterName) (\s@CreateAssociation' {} a -> s {automationTargetParameterName = a} :: CreateAssociation)

-- | The targets for the association. You can target instances by using tags,
-- AWS Resource Groups, all instances in an AWS account, or individual
-- instance IDs. For more information about choosing targets for an
-- association, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations>
-- in the /AWS Systems Manager User Guide/.
createAssociation_targets :: Lens.Lens' CreateAssociation (Prelude.Maybe [Target])
createAssociation_targets = Lens.lens (\CreateAssociation' {targets} -> targets) (\s@CreateAssociation' {} a -> s {targets = a} :: CreateAssociation) Prelude.. Lens.mapping Lens._Coerce

-- | A location is a combination of AWS Regions and AWS accounts where you
-- want to run the association. Use this action to create an association in
-- multiple Regions and multiple accounts.
createAssociation_targetLocations :: Lens.Lens' CreateAssociation (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
createAssociation_targetLocations = Lens.lens (\CreateAssociation' {targetLocations} -> targetLocations) (\s@CreateAssociation' {} a -> s {targetLocations = a} :: CreateAssociation) Prelude.. Lens.mapping Lens._Coerce

-- | A cron expression when the association will be applied to the target(s).
createAssociation_scheduleExpression :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_scheduleExpression = Lens.lens (\CreateAssociation' {scheduleExpression} -> scheduleExpression) (\s@CreateAssociation' {} a -> s {scheduleExpression = a} :: CreateAssociation)

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
createAssociation_maxConcurrency :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_maxConcurrency = Lens.lens (\CreateAssociation' {maxConcurrency} -> maxConcurrency) (\s@CreateAssociation' {} a -> s {maxConcurrency = a} :: CreateAssociation)

-- | Specify a descriptive name for the association.
createAssociation_associationName :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_associationName = Lens.lens (\CreateAssociation' {associationName} -> associationName) (\s@CreateAssociation' {} a -> s {associationName = a} :: CreateAssociation)

-- | The document version you want to associate with the target(s). Can be a
-- specific version or the default version.
createAssociation_documentVersion :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_documentVersion = Lens.lens (\CreateAssociation' {documentVersion} -> documentVersion) (\s@CreateAssociation' {} a -> s {documentVersion = a} :: CreateAssociation)

-- | The parameters for the runtime configuration of the document.
createAssociation_parameters :: Lens.Lens' CreateAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
createAssociation_parameters = Lens.lens (\CreateAssociation' {parameters} -> parameters) (\s@CreateAssociation' {} a -> s {parameters = a} :: CreateAssociation) Prelude.. Lens.mapping Lens._Coerce

-- | An S3 bucket where you want to store the output details of the request.
createAssociation_outputLocation :: Lens.Lens' CreateAssociation (Prelude.Maybe InstanceAssociationOutputLocation)
createAssociation_outputLocation = Lens.lens (\CreateAssociation' {outputLocation} -> outputLocation) (\s@CreateAssociation' {} a -> s {outputLocation = a} :: CreateAssociation)

-- | By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter is not supported for
-- rate expressions.
createAssociation_applyOnlyAtCronInterval :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Bool)
createAssociation_applyOnlyAtCronInterval = Lens.lens (\CreateAssociation' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@CreateAssociation' {} a -> s {applyOnlyAtCronInterval = a} :: CreateAssociation)

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
createAssociation_syncCompliance :: Lens.Lens' CreateAssociation (Prelude.Maybe AssociationSyncCompliance)
createAssociation_syncCompliance = Lens.lens (\CreateAssociation' {syncCompliance} -> syncCompliance) (\s@CreateAssociation' {} a -> s {syncCompliance = a} :: CreateAssociation)

-- | The name of the SSM document that contains the configuration information
-- for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a
-- document that is shared with you from another account.
--
-- For SSM documents that are shared with you from other AWS accounts, you
-- must specify the complete SSM document ARN, in the following format:
--
-- @arn:partition:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For AWS-predefined documents and SSM documents you created in your
-- account, you only need to specify the document name. For example,
-- @AWS-ApplyPatchBaseline@ or @My-Document@.
createAssociation_name :: Lens.Lens' CreateAssociation Prelude.Text
createAssociation_name = Lens.lens (\CreateAssociation' {name} -> name) (\s@CreateAssociation' {} a -> s {name = a} :: CreateAssociation)

instance Core.AWSRequest CreateAssociation where
  type
    AWSResponse CreateAssociation =
      CreateAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssociationResponse'
            Prelude.<$> (x Core..?> "AssociationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssociation

instance Prelude.NFData CreateAssociation

instance Core.ToHeaders CreateAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.CreateAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAssociation where
  toJSON CreateAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxErrors" Core..=) Prelude.<$> maxErrors,
            ("InstanceId" Core..=) Prelude.<$> instanceId,
            ("ComplianceSeverity" Core..=)
              Prelude.<$> complianceSeverity,
            ("AutomationTargetParameterName" Core..=)
              Prelude.<$> automationTargetParameterName,
            ("Targets" Core..=) Prelude.<$> targets,
            ("TargetLocations" Core..=)
              Prelude.<$> targetLocations,
            ("ScheduleExpression" Core..=)
              Prelude.<$> scheduleExpression,
            ("MaxConcurrency" Core..=)
              Prelude.<$> maxConcurrency,
            ("AssociationName" Core..=)
              Prelude.<$> associationName,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("OutputLocation" Core..=)
              Prelude.<$> outputLocation,
            ("ApplyOnlyAtCronInterval" Core..=)
              Prelude.<$> applyOnlyAtCronInterval,
            ("SyncCompliance" Core..=)
              Prelude.<$> syncCompliance,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateAssociation where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssociationResponse' smart constructor.
data CreateAssociationResponse = CreateAssociationResponse'
  { -- | Information about the association.
    associationDescription :: Prelude.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDescription', 'createAssociationResponse_associationDescription' - Information about the association.
--
-- 'httpStatus', 'createAssociationResponse_httpStatus' - The response's http status code.
newCreateAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssociationResponse
newCreateAssociationResponse pHttpStatus_ =
  CreateAssociationResponse'
    { associationDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the association.
createAssociationResponse_associationDescription :: Lens.Lens' CreateAssociationResponse (Prelude.Maybe AssociationDescription)
createAssociationResponse_associationDescription = Lens.lens (\CreateAssociationResponse' {associationDescription} -> associationDescription) (\s@CreateAssociationResponse' {} a -> s {associationDescription = a} :: CreateAssociationResponse)

-- | The response's http status code.
createAssociationResponse_httpStatus :: Lens.Lens' CreateAssociationResponse Prelude.Int
createAssociationResponse_httpStatus = Lens.lens (\CreateAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateAssociationResponse' {} a -> s {httpStatus = a} :: CreateAssociationResponse)

instance Prelude.NFData CreateAssociationResponse
