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
-- Module      : Network.AWS.SSM.UpdateAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an association. You can update the association name and version,
-- the document version, schedule, parameters, and Amazon S3 output.
--
-- In order to call this API action, your IAM user account, group, or role
-- must be configured with permission to call the DescribeAssociation API
-- action. If you don\'t have permission to call DescribeAssociation, then
-- you receive the following error:
-- @An error occurred (AccessDeniedException) when calling the UpdateAssociation operation: User: \<user_arn> is not authorized to perform: ssm:DescribeAssociation on resource: \<resource_arn>@
--
-- When you update an association, the association immediately runs against
-- the specified targets.
module Network.AWS.SSM.UpdateAssociation
  ( -- * Creating a Request
    UpdateAssociation (..),
    newUpdateAssociation,

    -- * Request Lenses
    updateAssociation_maxErrors,
    updateAssociation_complianceSeverity,
    updateAssociation_automationTargetParameterName,
    updateAssociation_targets,
    updateAssociation_targetLocations,
    updateAssociation_scheduleExpression,
    updateAssociation_name,
    updateAssociation_maxConcurrency,
    updateAssociation_associationName,
    updateAssociation_associationVersion,
    updateAssociation_documentVersion,
    updateAssociation_parameters,
    updateAssociation_outputLocation,
    updateAssociation_applyOnlyAtCronInterval,
    updateAssociation_syncCompliance,
    updateAssociation_associationId,

    -- * Destructuring the Response
    UpdateAssociationResponse (..),
    newUpdateAssociationResponse,

    -- * Response Lenses
    updateAssociationResponse_associationDescription,
    updateAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateAssociation' smart constructor.
data UpdateAssociation = UpdateAssociation'
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
    maxErrors :: Core.Maybe Core.Text,
    -- | The severity level to assign to the association.
    complianceSeverity :: Core.Maybe AssociationComplianceSeverity,
    -- | Specify the target for the association. This target is required for
    -- associations that use an Automation document and target resources by
    -- using rate controls.
    automationTargetParameterName :: Core.Maybe Core.Text,
    -- | The targets of the association.
    targets :: Core.Maybe [Target],
    -- | A location is a combination of AWS Regions and AWS accounts where you
    -- want to run the association. Use this action to update an association in
    -- multiple Regions and multiple accounts.
    targetLocations :: Core.Maybe (Core.NonEmpty TargetLocation),
    -- | The cron expression used to schedule the association that you want to
    -- update.
    scheduleExpression :: Core.Maybe Core.Text,
    -- | The name of the SSM document that contains the configuration information
    -- for the instance. You can specify Command or Automation documents.
    --
    -- You can specify AWS-predefined documents, documents you created, or a
    -- document that is shared with you from another account.
    --
    -- For SSM documents that are shared with you from other AWS accounts, you
    -- must specify the complete SSM document ARN, in the following format:
    --
    -- @arn:aws:ssm:region:account-id:document\/document-name @
    --
    -- For example:
    --
    -- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
    --
    -- For AWS-predefined documents and SSM documents you created in your
    -- account, you only need to specify the document name. For example,
    -- @AWS-ApplyPatchBaseline@ or @My-Document@.
    name :: Core.Maybe Core.Text,
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
    maxConcurrency :: Core.Maybe Core.Text,
    -- | The name of the association that you want to update.
    associationName :: Core.Maybe Core.Text,
    -- | This parameter is provided for concurrency control purposes. You must
    -- specify the latest association version in the service. If you want to
    -- ensure that this request succeeds, either specify @$LATEST@, or omit
    -- this parameter.
    associationVersion :: Core.Maybe Core.Text,
    -- | The document version you want update for the association.
    documentVersion :: Core.Maybe Core.Text,
    -- | The parameters you want to update for the association. If you create a
    -- parameter using Parameter Store, you can reference the parameter using
    -- {{ssm:parameter-name}}
    parameters :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | An S3 bucket where you want to store the results of this request.
    outputLocation :: Core.Maybe InstanceAssociationOutputLocation,
    -- | By default, when you update an association, the system runs it
    -- immediately after it is updated and then according to the schedule you
    -- specified. Specify this option if you don\'t want an association to run
    -- immediately after you update it. This parameter is not supported for
    -- rate expressions.
    --
    -- Also, if you specified this option when you created the association, you
    -- can reset it. To do so, specify the @no-apply-only-at-cron-interval@
    -- parameter when you update the association from the command line. This
    -- parameter forces the association to run immediately after updating it
    -- and according to the interval specified.
    applyOnlyAtCronInterval :: Core.Maybe Core.Bool,
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
    syncCompliance :: Core.Maybe AssociationSyncCompliance,
    -- | The ID of the association you want to update.
    associationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'updateAssociation_maxErrors' - The number of errors that are allowed before the system stops sending
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
-- 'complianceSeverity', 'updateAssociation_complianceSeverity' - The severity level to assign to the association.
--
-- 'automationTargetParameterName', 'updateAssociation_automationTargetParameterName' - Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
--
-- 'targets', 'updateAssociation_targets' - The targets of the association.
--
-- 'targetLocations', 'updateAssociation_targetLocations' - A location is a combination of AWS Regions and AWS accounts where you
-- want to run the association. Use this action to update an association in
-- multiple Regions and multiple accounts.
--
-- 'scheduleExpression', 'updateAssociation_scheduleExpression' - The cron expression used to schedule the association that you want to
-- update.
--
-- 'name', 'updateAssociation_name' - The name of the SSM document that contains the configuration information
-- for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a
-- document that is shared with you from another account.
--
-- For SSM documents that are shared with you from other AWS accounts, you
-- must specify the complete SSM document ARN, in the following format:
--
-- @arn:aws:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For AWS-predefined documents and SSM documents you created in your
-- account, you only need to specify the document name. For example,
-- @AWS-ApplyPatchBaseline@ or @My-Document@.
--
-- 'maxConcurrency', 'updateAssociation_maxConcurrency' - The maximum number of targets allowed to run the association at the same
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
-- 'associationName', 'updateAssociation_associationName' - The name of the association that you want to update.
--
-- 'associationVersion', 'updateAssociation_associationVersion' - This parameter is provided for concurrency control purposes. You must
-- specify the latest association version in the service. If you want to
-- ensure that this request succeeds, either specify @$LATEST@, or omit
-- this parameter.
--
-- 'documentVersion', 'updateAssociation_documentVersion' - The document version you want update for the association.
--
-- 'parameters', 'updateAssociation_parameters' - The parameters you want to update for the association. If you create a
-- parameter using Parameter Store, you can reference the parameter using
-- {{ssm:parameter-name}}
--
-- 'outputLocation', 'updateAssociation_outputLocation' - An S3 bucket where you want to store the results of this request.
--
-- 'applyOnlyAtCronInterval', 'updateAssociation_applyOnlyAtCronInterval' - By default, when you update an association, the system runs it
-- immediately after it is updated and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you update it. This parameter is not supported for
-- rate expressions.
--
-- Also, if you specified this option when you created the association, you
-- can reset it. To do so, specify the @no-apply-only-at-cron-interval@
-- parameter when you update the association from the command line. This
-- parameter forces the association to run immediately after updating it
-- and according to the interval specified.
--
-- 'syncCompliance', 'updateAssociation_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
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
-- 'associationId', 'updateAssociation_associationId' - The ID of the association you want to update.
newUpdateAssociation ::
  -- | 'associationId'
  Core.Text ->
  UpdateAssociation
newUpdateAssociation pAssociationId_ =
  UpdateAssociation'
    { maxErrors = Core.Nothing,
      complianceSeverity = Core.Nothing,
      automationTargetParameterName = Core.Nothing,
      targets = Core.Nothing,
      targetLocations = Core.Nothing,
      scheduleExpression = Core.Nothing,
      name = Core.Nothing,
      maxConcurrency = Core.Nothing,
      associationName = Core.Nothing,
      associationVersion = Core.Nothing,
      documentVersion = Core.Nothing,
      parameters = Core.Nothing,
      outputLocation = Core.Nothing,
      applyOnlyAtCronInterval = Core.Nothing,
      syncCompliance = Core.Nothing,
      associationId = pAssociationId_
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
updateAssociation_maxErrors :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_maxErrors = Lens.lens (\UpdateAssociation' {maxErrors} -> maxErrors) (\s@UpdateAssociation' {} a -> s {maxErrors = a} :: UpdateAssociation)

-- | The severity level to assign to the association.
updateAssociation_complianceSeverity :: Lens.Lens' UpdateAssociation (Core.Maybe AssociationComplianceSeverity)
updateAssociation_complianceSeverity = Lens.lens (\UpdateAssociation' {complianceSeverity} -> complianceSeverity) (\s@UpdateAssociation' {} a -> s {complianceSeverity = a} :: UpdateAssociation)

-- | Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
updateAssociation_automationTargetParameterName :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_automationTargetParameterName = Lens.lens (\UpdateAssociation' {automationTargetParameterName} -> automationTargetParameterName) (\s@UpdateAssociation' {} a -> s {automationTargetParameterName = a} :: UpdateAssociation)

-- | The targets of the association.
updateAssociation_targets :: Lens.Lens' UpdateAssociation (Core.Maybe [Target])
updateAssociation_targets = Lens.lens (\UpdateAssociation' {targets} -> targets) (\s@UpdateAssociation' {} a -> s {targets = a} :: UpdateAssociation) Core.. Lens.mapping Lens._Coerce

-- | A location is a combination of AWS Regions and AWS accounts where you
-- want to run the association. Use this action to update an association in
-- multiple Regions and multiple accounts.
updateAssociation_targetLocations :: Lens.Lens' UpdateAssociation (Core.Maybe (Core.NonEmpty TargetLocation))
updateAssociation_targetLocations = Lens.lens (\UpdateAssociation' {targetLocations} -> targetLocations) (\s@UpdateAssociation' {} a -> s {targetLocations = a} :: UpdateAssociation) Core.. Lens.mapping Lens._Coerce

-- | The cron expression used to schedule the association that you want to
-- update.
updateAssociation_scheduleExpression :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_scheduleExpression = Lens.lens (\UpdateAssociation' {scheduleExpression} -> scheduleExpression) (\s@UpdateAssociation' {} a -> s {scheduleExpression = a} :: UpdateAssociation)

-- | The name of the SSM document that contains the configuration information
-- for the instance. You can specify Command or Automation documents.
--
-- You can specify AWS-predefined documents, documents you created, or a
-- document that is shared with you from another account.
--
-- For SSM documents that are shared with you from other AWS accounts, you
-- must specify the complete SSM document ARN, in the following format:
--
-- @arn:aws:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For AWS-predefined documents and SSM documents you created in your
-- account, you only need to specify the document name. For example,
-- @AWS-ApplyPatchBaseline@ or @My-Document@.
updateAssociation_name :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_name = Lens.lens (\UpdateAssociation' {name} -> name) (\s@UpdateAssociation' {} a -> s {name = a} :: UpdateAssociation)

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
updateAssociation_maxConcurrency :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_maxConcurrency = Lens.lens (\UpdateAssociation' {maxConcurrency} -> maxConcurrency) (\s@UpdateAssociation' {} a -> s {maxConcurrency = a} :: UpdateAssociation)

-- | The name of the association that you want to update.
updateAssociation_associationName :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_associationName = Lens.lens (\UpdateAssociation' {associationName} -> associationName) (\s@UpdateAssociation' {} a -> s {associationName = a} :: UpdateAssociation)

-- | This parameter is provided for concurrency control purposes. You must
-- specify the latest association version in the service. If you want to
-- ensure that this request succeeds, either specify @$LATEST@, or omit
-- this parameter.
updateAssociation_associationVersion :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_associationVersion = Lens.lens (\UpdateAssociation' {associationVersion} -> associationVersion) (\s@UpdateAssociation' {} a -> s {associationVersion = a} :: UpdateAssociation)

-- | The document version you want update for the association.
updateAssociation_documentVersion :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Text)
updateAssociation_documentVersion = Lens.lens (\UpdateAssociation' {documentVersion} -> documentVersion) (\s@UpdateAssociation' {} a -> s {documentVersion = a} :: UpdateAssociation)

-- | The parameters you want to update for the association. If you create a
-- parameter using Parameter Store, you can reference the parameter using
-- {{ssm:parameter-name}}
updateAssociation_parameters :: Lens.Lens' UpdateAssociation (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
updateAssociation_parameters = Lens.lens (\UpdateAssociation' {parameters} -> parameters) (\s@UpdateAssociation' {} a -> s {parameters = a} :: UpdateAssociation) Core.. Lens.mapping Lens._Coerce

-- | An S3 bucket where you want to store the results of this request.
updateAssociation_outputLocation :: Lens.Lens' UpdateAssociation (Core.Maybe InstanceAssociationOutputLocation)
updateAssociation_outputLocation = Lens.lens (\UpdateAssociation' {outputLocation} -> outputLocation) (\s@UpdateAssociation' {} a -> s {outputLocation = a} :: UpdateAssociation)

-- | By default, when you update an association, the system runs it
-- immediately after it is updated and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you update it. This parameter is not supported for
-- rate expressions.
--
-- Also, if you specified this option when you created the association, you
-- can reset it. To do so, specify the @no-apply-only-at-cron-interval@
-- parameter when you update the association from the command line. This
-- parameter forces the association to run immediately after updating it
-- and according to the interval specified.
updateAssociation_applyOnlyAtCronInterval :: Lens.Lens' UpdateAssociation (Core.Maybe Core.Bool)
updateAssociation_applyOnlyAtCronInterval = Lens.lens (\UpdateAssociation' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@UpdateAssociation' {} a -> s {applyOnlyAtCronInterval = a} :: UpdateAssociation)

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
updateAssociation_syncCompliance :: Lens.Lens' UpdateAssociation (Core.Maybe AssociationSyncCompliance)
updateAssociation_syncCompliance = Lens.lens (\UpdateAssociation' {syncCompliance} -> syncCompliance) (\s@UpdateAssociation' {} a -> s {syncCompliance = a} :: UpdateAssociation)

-- | The ID of the association you want to update.
updateAssociation_associationId :: Lens.Lens' UpdateAssociation Core.Text
updateAssociation_associationId = Lens.lens (\UpdateAssociation' {associationId} -> associationId) (\s@UpdateAssociation' {} a -> s {associationId = a} :: UpdateAssociation)

instance Core.AWSRequest UpdateAssociation where
  type
    AWSResponse UpdateAssociation =
      UpdateAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssociationResponse'
            Core.<$> (x Core..?> "AssociationDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateAssociation

instance Core.NFData UpdateAssociation

instance Core.ToHeaders UpdateAssociation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.UpdateAssociation" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateAssociation where
  toJSON UpdateAssociation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxErrors" Core..=) Core.<$> maxErrors,
            ("ComplianceSeverity" Core..=)
              Core.<$> complianceSeverity,
            ("AutomationTargetParameterName" Core..=)
              Core.<$> automationTargetParameterName,
            ("Targets" Core..=) Core.<$> targets,
            ("TargetLocations" Core..=) Core.<$> targetLocations,
            ("ScheduleExpression" Core..=)
              Core.<$> scheduleExpression,
            ("Name" Core..=) Core.<$> name,
            ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
            ("AssociationName" Core..=) Core.<$> associationName,
            ("AssociationVersion" Core..=)
              Core.<$> associationVersion,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("Parameters" Core..=) Core.<$> parameters,
            ("OutputLocation" Core..=) Core.<$> outputLocation,
            ("ApplyOnlyAtCronInterval" Core..=)
              Core.<$> applyOnlyAtCronInterval,
            ("SyncCompliance" Core..=) Core.<$> syncCompliance,
            Core.Just ("AssociationId" Core..= associationId)
          ]
      )

instance Core.ToPath UpdateAssociation where
  toPath = Core.const "/"

instance Core.ToQuery UpdateAssociation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAssociationResponse' smart constructor.
data UpdateAssociationResponse = UpdateAssociationResponse'
  { -- | The description of the association that was updated.
    associationDescription :: Core.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDescription', 'updateAssociationResponse_associationDescription' - The description of the association that was updated.
--
-- 'httpStatus', 'updateAssociationResponse_httpStatus' - The response's http status code.
newUpdateAssociationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateAssociationResponse
newUpdateAssociationResponse pHttpStatus_ =
  UpdateAssociationResponse'
    { associationDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the association that was updated.
updateAssociationResponse_associationDescription :: Lens.Lens' UpdateAssociationResponse (Core.Maybe AssociationDescription)
updateAssociationResponse_associationDescription = Lens.lens (\UpdateAssociationResponse' {associationDescription} -> associationDescription) (\s@UpdateAssociationResponse' {} a -> s {associationDescription = a} :: UpdateAssociationResponse)

-- | The response's http status code.
updateAssociationResponse_httpStatus :: Lens.Lens' UpdateAssociationResponse Core.Int
updateAssociationResponse_httpStatus = Lens.lens (\UpdateAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateAssociationResponse' {} a -> s {httpStatus = a} :: UpdateAssociationResponse)

instance Core.NFData UpdateAssociationResponse
