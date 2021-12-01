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
-- Module      : Amazonka.SSM.UpdateAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an association. You can update the association name and version,
-- the document version, schedule, parameters, and Amazon Simple Storage
-- Service (Amazon S3) output.
--
-- In order to call this API operation, your Identity and Access Management
-- (IAM) user account, group, or role must be configured with permission to
-- call the DescribeAssociation API operation. If you don\'t have
-- permission to call @DescribeAssociation@, then you receive the following
-- error:
-- @An error occurred (AccessDeniedException) when calling the UpdateAssociation operation: User: \<user_arn> isn\'t authorized to perform: ssm:DescribeAssociation on resource: \<resource_arn>@
--
-- When you update an association, the association immediately runs against
-- the specified targets.
module Amazonka.SSM.UpdateAssociation
  ( -- * Creating a Request
    UpdateAssociation (..),
    newUpdateAssociation,

    -- * Request Lenses
    updateAssociation_targetLocations,
    updateAssociation_applyOnlyAtCronInterval,
    updateAssociation_maxErrors,
    updateAssociation_scheduleExpression,
    updateAssociation_name,
    updateAssociation_outputLocation,
    updateAssociation_syncCompliance,
    updateAssociation_targets,
    updateAssociation_parameters,
    updateAssociation_documentVersion,
    updateAssociation_automationTargetParameterName,
    updateAssociation_associationVersion,
    updateAssociation_associationName,
    updateAssociation_calendarNames,
    updateAssociation_complianceSeverity,
    updateAssociation_maxConcurrency,
    updateAssociation_associationId,

    -- * Destructuring the Response
    UpdateAssociationResponse (..),
    newUpdateAssociationResponse,

    -- * Response Lenses
    updateAssociationResponse_associationDescription,
    updateAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateAssociation' smart constructor.
data UpdateAssociation = UpdateAssociation'
  { -- | A location is a combination of Amazon Web Services Regions and Amazon
    -- Web Services accounts where you want to run the association. Use this
    -- action to update an association in multiple Regions and multiple
    -- accounts.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | By default, when you update an association, the system runs it
    -- immediately after it is updated and then according to the schedule you
    -- specified. Specify this option if you don\'t want an association to run
    -- immediately after you update it. This parameter isn\'t supported for
    -- rate expressions.
    --
    -- Also, if you specified this option when you created the association, you
    -- can reset it. To do so, specify the @no-apply-only-at-cron-interval@
    -- parameter when you update the association from the command line. This
    -- parameter forces the association to run immediately after updating it
    -- and according to the interval specified.
    applyOnlyAtCronInterval :: Prelude.Maybe Prelude.Bool,
    -- | The number of errors that are allowed before the system stops sending
    -- requests to run the association on additional targets. You can specify
    -- either an absolute number of errors, for example 10, or a percentage of
    -- the target set, for example 10%. If you specify 3, for example, the
    -- system stops sending requests when the fourth error is received. If you
    -- specify 0, then the system stops sending requests after the first error
    -- is returned. If you run an association on 50 instances and set
    -- @MaxError@ to 10%, then the system stops sending the request when the
    -- sixth error is received.
    --
    -- Executions that are already running an association when @MaxErrors@ is
    -- reached are allowed to complete, but some of these executions may fail
    -- as well. If you need to ensure that there won\'t be more than max-errors
    -- failed executions, set @MaxConcurrency@ to 1 so that executions proceed
    -- one at a time.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The cron expression used to schedule the association that you want to
    -- update.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSM Command document or Automation runbook that contains
    -- the configuration information for the instance.
    --
    -- You can specify Amazon Web Services-predefined documents, documents you
    -- created, or a document that is shared with you from another account.
    --
    -- For Systems Manager document (SSM document) that are shared with you
    -- from other Amazon Web Services accounts, you must specify the complete
    -- SSM document ARN, in the following format:
    --
    -- @arn:aws:ssm:region:account-id:document\/document-name @
    --
    -- For example:
    --
    -- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
    --
    -- For Amazon Web Services-predefined documents and SSM documents you
    -- created in your account, you only need to specify the document name. For
    -- example, @AWS-ApplyPatchBaseline@ or @My-Document@.
    name :: Prelude.Maybe Prelude.Text,
    -- | An S3 bucket where you want to store the results of this request.
    outputLocation :: Prelude.Maybe InstanceAssociationOutputLocation,
    -- | The mode for generating association compliance. You can specify @AUTO@
    -- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
    -- association execution to determine the compliance status. If the
    -- association execution runs successfully, then the association is
    -- @COMPLIANT@. If the association execution doesn\'t run successfully, the
    -- association is @NON-COMPLIANT@.
    --
    -- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
    -- for the PutComplianceItems API operation. In this case, compliance data
    -- isn\'t managed by State Manager, a capability of Amazon Web Services
    -- Systems Manager. It is managed by your direct call to the
    -- PutComplianceItems API operation.
    --
    -- By default, all associations use @AUTO@ mode.
    syncCompliance :: Prelude.Maybe AssociationSyncCompliance,
    -- | The targets of the association.
    targets :: Prelude.Maybe [Target],
    -- | The parameters you want to update for the association. If you create a
    -- parameter using Parameter Store, a capability of Amazon Web Services
    -- Systems Manager, you can reference the parameter using
    -- @{{ssm:parameter-name}}@.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The document version you want update for the association.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Specify the target for the association. This target is required for
    -- associations that use an Automation runbook and target resources by
    -- using rate controls. Automation is a capability of Amazon Web Services
    -- Systems Manager.
    automationTargetParameterName :: Prelude.Maybe Prelude.Text,
    -- | This parameter is provided for concurrency control purposes. You must
    -- specify the latest association version in the service. If you want to
    -- ensure that this request succeeds, either specify @$LATEST@, or omit
    -- this parameter.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the association that you want to update.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
    -- documents you want to gate your associations under. The associations
    -- only run when that change calendar is open. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
    calendarNames :: Prelude.Maybe [Prelude.Text],
    -- | The severity level to assign to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
    -- | The maximum number of targets allowed to run the association at the same
    -- time. You can specify a number, for example 10, or a percentage of the
    -- target set, for example 10%. The default value is 100%, which means all
    -- targets run the association at the same time.
    --
    -- If a new instance starts and attempts to run an association while
    -- Systems Manager is running @MaxConcurrency@ associations, the
    -- association is allowed to run. During the next association interval, the
    -- new instance will process its association within the limit specified for
    -- @MaxConcurrency@.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association you want to update.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetLocations', 'updateAssociation_targetLocations' - A location is a combination of Amazon Web Services Regions and Amazon
-- Web Services accounts where you want to run the association. Use this
-- action to update an association in multiple Regions and multiple
-- accounts.
--
-- 'applyOnlyAtCronInterval', 'updateAssociation_applyOnlyAtCronInterval' - By default, when you update an association, the system runs it
-- immediately after it is updated and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you update it. This parameter isn\'t supported for
-- rate expressions.
--
-- Also, if you specified this option when you created the association, you
-- can reset it. To do so, specify the @no-apply-only-at-cron-interval@
-- parameter when you update the association from the command line. This
-- parameter forces the association to run immediately after updating it
-- and according to the interval specified.
--
-- 'maxErrors', 'updateAssociation_maxErrors' - The number of errors that are allowed before the system stops sending
-- requests to run the association on additional targets. You can specify
-- either an absolute number of errors, for example 10, or a percentage of
-- the target set, for example 10%. If you specify 3, for example, the
-- system stops sending requests when the fourth error is received. If you
-- specify 0, then the system stops sending requests after the first error
-- is returned. If you run an association on 50 instances and set
-- @MaxError@ to 10%, then the system stops sending the request when the
-- sixth error is received.
--
-- Executions that are already running an association when @MaxErrors@ is
-- reached are allowed to complete, but some of these executions may fail
-- as well. If you need to ensure that there won\'t be more than max-errors
-- failed executions, set @MaxConcurrency@ to 1 so that executions proceed
-- one at a time.
--
-- 'scheduleExpression', 'updateAssociation_scheduleExpression' - The cron expression used to schedule the association that you want to
-- update.
--
-- 'name', 'updateAssociation_name' - The name of the SSM Command document or Automation runbook that contains
-- the configuration information for the instance.
--
-- You can specify Amazon Web Services-predefined documents, documents you
-- created, or a document that is shared with you from another account.
--
-- For Systems Manager document (SSM document) that are shared with you
-- from other Amazon Web Services accounts, you must specify the complete
-- SSM document ARN, in the following format:
--
-- @arn:aws:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For Amazon Web Services-predefined documents and SSM documents you
-- created in your account, you only need to specify the document name. For
-- example, @AWS-ApplyPatchBaseline@ or @My-Document@.
--
-- 'outputLocation', 'updateAssociation_outputLocation' - An S3 bucket where you want to store the results of this request.
--
-- 'syncCompliance', 'updateAssociation_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
-- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
-- association execution to determine the compliance status. If the
-- association execution runs successfully, then the association is
-- @COMPLIANT@. If the association execution doesn\'t run successfully, the
-- association is @NON-COMPLIANT@.
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
-- for the PutComplianceItems API operation. In this case, compliance data
-- isn\'t managed by State Manager, a capability of Amazon Web Services
-- Systems Manager. It is managed by your direct call to the
-- PutComplianceItems API operation.
--
-- By default, all associations use @AUTO@ mode.
--
-- 'targets', 'updateAssociation_targets' - The targets of the association.
--
-- 'parameters', 'updateAssociation_parameters' - The parameters you want to update for the association. If you create a
-- parameter using Parameter Store, a capability of Amazon Web Services
-- Systems Manager, you can reference the parameter using
-- @{{ssm:parameter-name}}@.
--
-- 'documentVersion', 'updateAssociation_documentVersion' - The document version you want update for the association.
--
-- 'automationTargetParameterName', 'updateAssociation_automationTargetParameterName' - Specify the target for the association. This target is required for
-- associations that use an Automation runbook and target resources by
-- using rate controls. Automation is a capability of Amazon Web Services
-- Systems Manager.
--
-- 'associationVersion', 'updateAssociation_associationVersion' - This parameter is provided for concurrency control purposes. You must
-- specify the latest association version in the service. If you want to
-- ensure that this request succeeds, either specify @$LATEST@, or omit
-- this parameter.
--
-- 'associationName', 'updateAssociation_associationName' - The name of the association that you want to update.
--
-- 'calendarNames', 'updateAssociation_calendarNames' - The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents you want to gate your associations under. The associations
-- only run when that change calendar is open. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
--
-- 'complianceSeverity', 'updateAssociation_complianceSeverity' - The severity level to assign to the association.
--
-- 'maxConcurrency', 'updateAssociation_maxConcurrency' - The maximum number of targets allowed to run the association at the same
-- time. You can specify a number, for example 10, or a percentage of the
-- target set, for example 10%. The default value is 100%, which means all
-- targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while
-- Systems Manager is running @MaxConcurrency@ associations, the
-- association is allowed to run. During the next association interval, the
-- new instance will process its association within the limit specified for
-- @MaxConcurrency@.
--
-- 'associationId', 'updateAssociation_associationId' - The ID of the association you want to update.
newUpdateAssociation ::
  -- | 'associationId'
  Prelude.Text ->
  UpdateAssociation
newUpdateAssociation pAssociationId_ =
  UpdateAssociation'
    { targetLocations =
        Prelude.Nothing,
      applyOnlyAtCronInterval = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      name = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      syncCompliance = Prelude.Nothing,
      targets = Prelude.Nothing,
      parameters = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      automationTargetParameterName = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      associationName = Prelude.Nothing,
      calendarNames = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      associationId = pAssociationId_
    }

-- | A location is a combination of Amazon Web Services Regions and Amazon
-- Web Services accounts where you want to run the association. Use this
-- action to update an association in multiple Regions and multiple
-- accounts.
updateAssociation_targetLocations :: Lens.Lens' UpdateAssociation (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
updateAssociation_targetLocations = Lens.lens (\UpdateAssociation' {targetLocations} -> targetLocations) (\s@UpdateAssociation' {} a -> s {targetLocations = a} :: UpdateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | By default, when you update an association, the system runs it
-- immediately after it is updated and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you update it. This parameter isn\'t supported for
-- rate expressions.
--
-- Also, if you specified this option when you created the association, you
-- can reset it. To do so, specify the @no-apply-only-at-cron-interval@
-- parameter when you update the association from the command line. This
-- parameter forces the association to run immediately after updating it
-- and according to the interval specified.
updateAssociation_applyOnlyAtCronInterval :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Bool)
updateAssociation_applyOnlyAtCronInterval = Lens.lens (\UpdateAssociation' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@UpdateAssociation' {} a -> s {applyOnlyAtCronInterval = a} :: UpdateAssociation)

-- | The number of errors that are allowed before the system stops sending
-- requests to run the association on additional targets. You can specify
-- either an absolute number of errors, for example 10, or a percentage of
-- the target set, for example 10%. If you specify 3, for example, the
-- system stops sending requests when the fourth error is received. If you
-- specify 0, then the system stops sending requests after the first error
-- is returned. If you run an association on 50 instances and set
-- @MaxError@ to 10%, then the system stops sending the request when the
-- sixth error is received.
--
-- Executions that are already running an association when @MaxErrors@ is
-- reached are allowed to complete, but some of these executions may fail
-- as well. If you need to ensure that there won\'t be more than max-errors
-- failed executions, set @MaxConcurrency@ to 1 so that executions proceed
-- one at a time.
updateAssociation_maxErrors :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_maxErrors = Lens.lens (\UpdateAssociation' {maxErrors} -> maxErrors) (\s@UpdateAssociation' {} a -> s {maxErrors = a} :: UpdateAssociation)

-- | The cron expression used to schedule the association that you want to
-- update.
updateAssociation_scheduleExpression :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_scheduleExpression = Lens.lens (\UpdateAssociation' {scheduleExpression} -> scheduleExpression) (\s@UpdateAssociation' {} a -> s {scheduleExpression = a} :: UpdateAssociation)

-- | The name of the SSM Command document or Automation runbook that contains
-- the configuration information for the instance.
--
-- You can specify Amazon Web Services-predefined documents, documents you
-- created, or a document that is shared with you from another account.
--
-- For Systems Manager document (SSM document) that are shared with you
-- from other Amazon Web Services accounts, you must specify the complete
-- SSM document ARN, in the following format:
--
-- @arn:aws:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For Amazon Web Services-predefined documents and SSM documents you
-- created in your account, you only need to specify the document name. For
-- example, @AWS-ApplyPatchBaseline@ or @My-Document@.
updateAssociation_name :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_name = Lens.lens (\UpdateAssociation' {name} -> name) (\s@UpdateAssociation' {} a -> s {name = a} :: UpdateAssociation)

-- | An S3 bucket where you want to store the results of this request.
updateAssociation_outputLocation :: Lens.Lens' UpdateAssociation (Prelude.Maybe InstanceAssociationOutputLocation)
updateAssociation_outputLocation = Lens.lens (\UpdateAssociation' {outputLocation} -> outputLocation) (\s@UpdateAssociation' {} a -> s {outputLocation = a} :: UpdateAssociation)

-- | The mode for generating association compliance. You can specify @AUTO@
-- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
-- association execution to determine the compliance status. If the
-- association execution runs successfully, then the association is
-- @COMPLIANT@. If the association execution doesn\'t run successfully, the
-- association is @NON-COMPLIANT@.
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
-- for the PutComplianceItems API operation. In this case, compliance data
-- isn\'t managed by State Manager, a capability of Amazon Web Services
-- Systems Manager. It is managed by your direct call to the
-- PutComplianceItems API operation.
--
-- By default, all associations use @AUTO@ mode.
updateAssociation_syncCompliance :: Lens.Lens' UpdateAssociation (Prelude.Maybe AssociationSyncCompliance)
updateAssociation_syncCompliance = Lens.lens (\UpdateAssociation' {syncCompliance} -> syncCompliance) (\s@UpdateAssociation' {} a -> s {syncCompliance = a} :: UpdateAssociation)

-- | The targets of the association.
updateAssociation_targets :: Lens.Lens' UpdateAssociation (Prelude.Maybe [Target])
updateAssociation_targets = Lens.lens (\UpdateAssociation' {targets} -> targets) (\s@UpdateAssociation' {} a -> s {targets = a} :: UpdateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The parameters you want to update for the association. If you create a
-- parameter using Parameter Store, a capability of Amazon Web Services
-- Systems Manager, you can reference the parameter using
-- @{{ssm:parameter-name}}@.
updateAssociation_parameters :: Lens.Lens' UpdateAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
updateAssociation_parameters = Lens.lens (\UpdateAssociation' {parameters} -> parameters) (\s@UpdateAssociation' {} a -> s {parameters = a} :: UpdateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The document version you want update for the association.
updateAssociation_documentVersion :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_documentVersion = Lens.lens (\UpdateAssociation' {documentVersion} -> documentVersion) (\s@UpdateAssociation' {} a -> s {documentVersion = a} :: UpdateAssociation)

-- | Specify the target for the association. This target is required for
-- associations that use an Automation runbook and target resources by
-- using rate controls. Automation is a capability of Amazon Web Services
-- Systems Manager.
updateAssociation_automationTargetParameterName :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_automationTargetParameterName = Lens.lens (\UpdateAssociation' {automationTargetParameterName} -> automationTargetParameterName) (\s@UpdateAssociation' {} a -> s {automationTargetParameterName = a} :: UpdateAssociation)

-- | This parameter is provided for concurrency control purposes. You must
-- specify the latest association version in the service. If you want to
-- ensure that this request succeeds, either specify @$LATEST@, or omit
-- this parameter.
updateAssociation_associationVersion :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_associationVersion = Lens.lens (\UpdateAssociation' {associationVersion} -> associationVersion) (\s@UpdateAssociation' {} a -> s {associationVersion = a} :: UpdateAssociation)

-- | The name of the association that you want to update.
updateAssociation_associationName :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_associationName = Lens.lens (\UpdateAssociation' {associationName} -> associationName) (\s@UpdateAssociation' {} a -> s {associationName = a} :: UpdateAssociation)

-- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents you want to gate your associations under. The associations
-- only run when that change calendar is open. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
updateAssociation_calendarNames :: Lens.Lens' UpdateAssociation (Prelude.Maybe [Prelude.Text])
updateAssociation_calendarNames = Lens.lens (\UpdateAssociation' {calendarNames} -> calendarNames) (\s@UpdateAssociation' {} a -> s {calendarNames = a} :: UpdateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The severity level to assign to the association.
updateAssociation_complianceSeverity :: Lens.Lens' UpdateAssociation (Prelude.Maybe AssociationComplianceSeverity)
updateAssociation_complianceSeverity = Lens.lens (\UpdateAssociation' {complianceSeverity} -> complianceSeverity) (\s@UpdateAssociation' {} a -> s {complianceSeverity = a} :: UpdateAssociation)

-- | The maximum number of targets allowed to run the association at the same
-- time. You can specify a number, for example 10, or a percentage of the
-- target set, for example 10%. The default value is 100%, which means all
-- targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while
-- Systems Manager is running @MaxConcurrency@ associations, the
-- association is allowed to run. During the next association interval, the
-- new instance will process its association within the limit specified for
-- @MaxConcurrency@.
updateAssociation_maxConcurrency :: Lens.Lens' UpdateAssociation (Prelude.Maybe Prelude.Text)
updateAssociation_maxConcurrency = Lens.lens (\UpdateAssociation' {maxConcurrency} -> maxConcurrency) (\s@UpdateAssociation' {} a -> s {maxConcurrency = a} :: UpdateAssociation)

-- | The ID of the association you want to update.
updateAssociation_associationId :: Lens.Lens' UpdateAssociation Prelude.Text
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
            Prelude.<$> (x Core..?> "AssociationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAssociation where
  hashWithSalt salt' UpdateAssociation' {..} =
    salt' `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` complianceSeverity
      `Prelude.hashWithSalt` calendarNames
      `Prelude.hashWithSalt` associationName
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` automationTargetParameterName
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` syncCompliance
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` applyOnlyAtCronInterval
      `Prelude.hashWithSalt` targetLocations

instance Prelude.NFData UpdateAssociation where
  rnf UpdateAssociation' {..} =
    Prelude.rnf targetLocations
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf complianceSeverity
      `Prelude.seq` Prelude.rnf calendarNames
      `Prelude.seq` Prelude.rnf associationName
      `Prelude.seq` Prelude.rnf associationVersion
      `Prelude.seq` Prelude.rnf automationTargetParameterName
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf syncCompliance
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf applyOnlyAtCronInterval

instance Core.ToHeaders UpdateAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAssociation where
  toJSON UpdateAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetLocations" Core..=)
              Prelude.<$> targetLocations,
            ("ApplyOnlyAtCronInterval" Core..=)
              Prelude.<$> applyOnlyAtCronInterval,
            ("MaxErrors" Core..=) Prelude.<$> maxErrors,
            ("ScheduleExpression" Core..=)
              Prelude.<$> scheduleExpression,
            ("Name" Core..=) Prelude.<$> name,
            ("OutputLocation" Core..=)
              Prelude.<$> outputLocation,
            ("SyncCompliance" Core..=)
              Prelude.<$> syncCompliance,
            ("Targets" Core..=) Prelude.<$> targets,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            ("AutomationTargetParameterName" Core..=)
              Prelude.<$> automationTargetParameterName,
            ("AssociationVersion" Core..=)
              Prelude.<$> associationVersion,
            ("AssociationName" Core..=)
              Prelude.<$> associationName,
            ("CalendarNames" Core..=) Prelude.<$> calendarNames,
            ("ComplianceSeverity" Core..=)
              Prelude.<$> complianceSeverity,
            ("MaxConcurrency" Core..=)
              Prelude.<$> maxConcurrency,
            Prelude.Just
              ("AssociationId" Core..= associationId)
          ]
      )

instance Core.ToPath UpdateAssociation where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssociationResponse' smart constructor.
data UpdateAssociationResponse = UpdateAssociationResponse'
  { -- | The description of the association that was updated.
    associationDescription :: Prelude.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateAssociationResponse
newUpdateAssociationResponse pHttpStatus_ =
  UpdateAssociationResponse'
    { associationDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the association that was updated.
updateAssociationResponse_associationDescription :: Lens.Lens' UpdateAssociationResponse (Prelude.Maybe AssociationDescription)
updateAssociationResponse_associationDescription = Lens.lens (\UpdateAssociationResponse' {associationDescription} -> associationDescription) (\s@UpdateAssociationResponse' {} a -> s {associationDescription = a} :: UpdateAssociationResponse)

-- | The response's http status code.
updateAssociationResponse_httpStatus :: Lens.Lens' UpdateAssociationResponse Prelude.Int
updateAssociationResponse_httpStatus = Lens.lens (\UpdateAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateAssociationResponse' {} a -> s {httpStatus = a} :: UpdateAssociationResponse)

instance Prelude.NFData UpdateAssociationResponse where
  rnf UpdateAssociationResponse' {..} =
    Prelude.rnf associationDescription
      `Prelude.seq` Prelude.rnf httpStatus
