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
-- Module      : Amazonka.SSM.CreateAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A State Manager association defines the state that you want to maintain
-- on your managed nodes. For example, an association can specify that
-- anti-virus software must be installed and running on your managed nodes,
-- or that certain ports must be closed. For static targets, the
-- association specifies a schedule for when the configuration is
-- reapplied. For dynamic targets, such as an Amazon Web Services resource
-- group or an Amazon Web Services autoscaling group, State Manager, a
-- capability of Amazon Web Services Systems Manager applies the
-- configuration when new managed nodes are added to the group. The
-- association also specifies actions to take when applying the
-- configuration. For example, an association for anti-virus software might
-- run once a day. If the software isn\'t installed, then State Manager
-- installs it. If the software is installed, but the service isn\'t
-- running, then the association might instruct State Manager to start the
-- service.
module Amazonka.SSM.CreateAssociation
  ( -- * Creating a Request
    CreateAssociation (..),
    newCreateAssociation,

    -- * Request Lenses
    createAssociation_alarmConfiguration,
    createAssociation_applyOnlyAtCronInterval,
    createAssociation_associationName,
    createAssociation_automationTargetParameterName,
    createAssociation_calendarNames,
    createAssociation_complianceSeverity,
    createAssociation_documentVersion,
    createAssociation_instanceId,
    createAssociation_maxConcurrency,
    createAssociation_maxErrors,
    createAssociation_outputLocation,
    createAssociation_parameters,
    createAssociation_scheduleExpression,
    createAssociation_scheduleOffset,
    createAssociation_syncCompliance,
    createAssociation_tags,
    createAssociation_targetLocations,
    createAssociation_targetMaps,
    createAssociation_targets,
    createAssociation_name,

    -- * Destructuring the Response
    CreateAssociationResponse (..),
    newCreateAssociationResponse,

    -- * Response Lenses
    createAssociationResponse_associationDescription,
    createAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCreateAssociation' smart constructor.
data CreateAssociation = CreateAssociation'
  { alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | By default, when you create a new association, the system runs it
    -- immediately after it is created and then according to the schedule you
    -- specified. Specify this option if you don\'t want an association to run
    -- immediately after you create it. This parameter isn\'t supported for
    -- rate expressions.
    applyOnlyAtCronInterval :: Prelude.Maybe Prelude.Bool,
    -- | Specify a descriptive name for the association.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | Choose the parameter that will define how your automation will branch
    -- out. This target is required for associations that use an Automation
    -- runbook and target resources by using rate controls. Automation is a
    -- capability of Amazon Web Services Systems Manager.
    automationTargetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
    -- documents you want to gate your associations under. The associations
    -- only run when that change calendar is open. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
    calendarNames :: Prelude.Maybe [Prelude.Text],
    -- | The severity level to assign to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
    -- | The document version you want to associate with the target(s). Can be a
    -- specific version or the default version.
    --
    -- State Manager doesn\'t support running associations that use a new
    -- version of a document if that document is shared from another account.
    -- State Manager always runs the @default@ version of a document if shared
    -- from another account, even though the Systems Manager console shows that
    -- a new version was processed. If you want to run an association using a
    -- new version of a document shared form another account, you must set the
    -- document version to @default@.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID.
    --
    -- @InstanceId@ has been deprecated. To specify a managed node ID for an
    -- association, use the @Targets@ parameter. Requests that include the
    -- parameter @InstanceID@ with Systems Manager documents (SSM documents)
    -- that use schema version 2.0 or later will fail. In addition, if you use
    -- the parameter @InstanceId@, you can\'t use the parameters
    -- @AssociationName@, @DocumentVersion@, @MaxErrors@, @MaxConcurrency@,
    -- @OutputLocation@, or @ScheduleExpression@. To use these parameters, you
    -- must use the @Targets@ parameter.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of targets allowed to run the association at the same
    -- time. You can specify a number, for example 10, or a percentage of the
    -- target set, for example 10%. The default value is 100%, which means all
    -- targets run the association at the same time.
    --
    -- If a new managed node starts and attempts to run an association while
    -- Systems Manager is running @MaxConcurrency@ associations, the
    -- association is allowed to run. During the next association interval, the
    -- new managed node will process its association within the limit specified
    -- for @MaxConcurrency@.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The number of errors that are allowed before the system stops sending
    -- requests to run the association on additional targets. You can specify
    -- either an absolute number of errors, for example 10, or a percentage of
    -- the target set, for example 10%. If you specify 3, for example, the
    -- system stops sending requests when the fourth error is received. If you
    -- specify 0, then the system stops sending requests after the first error
    -- is returned. If you run an association on 50 managed nodes and set
    -- @MaxError@ to 10%, then the system stops sending the request when the
    -- sixth error is received.
    --
    -- Executions that are already running an association when @MaxErrors@ is
    -- reached are allowed to complete, but some of these executions may fail
    -- as well. If you need to ensure that there won\'t be more than max-errors
    -- failed executions, set @MaxConcurrency@ to 1 so that executions proceed
    -- one at a time.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Simple Storage Service (Amazon S3) bucket where you want to
    -- store the output details of the request.
    outputLocation :: Prelude.Maybe InstanceAssociationOutputLocation,
    -- | The parameters for the runtime configuration of the document.
    parameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text [Prelude.Text])),
    -- | A cron expression when the association will be applied to the target(s).
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | Number of days to wait after the scheduled day to run an association.
    -- For example, if you specified a cron schedule of
    -- @cron(0 0 ? * THU#2 *)@, you could specify an offset of 3 to run the
    -- association each Sunday after the second Thursday of the month. For more
    -- information about cron schedules for associations, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/reference-cron-and-rate-expressions.html Reference: Cron and rate expressions for Systems Manager>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    --
    -- To use offsets, you must specify the @ApplyOnlyAtCronInterval@
    -- parameter. This option tells the system not to run an association
    -- immediately after you create it.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | The mode for generating association compliance. You can specify @AUTO@
    -- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
    -- association execution to determine the compliance status. If the
    -- association execution runs successfully, then the association is
    -- @COMPLIANT@. If the association execution doesn\'t run successfully, the
    -- association is @NON-COMPLIANT@.
    --
    -- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
    -- for the PutComplianceItems API operation. In this case, compliance data
    -- isn\'t managed by State Manager. It is managed by your direct call to
    -- the PutComplianceItems API operation.
    --
    -- By default, all associations use @AUTO@ mode.
    syncCompliance :: Prelude.Maybe AssociationSyncCompliance,
    -- | Adds or overwrites one or more tags for a State Manager association.
    -- /Tags/ are metadata that you can assign to your Amazon Web Services
    -- resources. Tags enable you to categorize your resources in different
    -- ways, for example, by purpose, owner, or environment. Each tag consists
    -- of a key and an optional value, both of which you define.
    tags :: Prelude.Maybe [Tag],
    -- | A location is a combination of Amazon Web Services Regions and Amazon
    -- Web Services accounts where you want to run the association. Use this
    -- action to create an association in multiple Regions and multiple
    -- accounts.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | A key-value mapping of document parameters to target resources. Both
    -- Targets and TargetMaps can\'t be specified together.
    targetMaps :: Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]],
    -- | The targets for the association. You can target managed nodes by using
    -- tags, Amazon Web Services resource groups, all managed nodes in an
    -- Amazon Web Services account, or individual managed node IDs. You can
    -- target all managed nodes in an Amazon Web Services account by specifying
    -- the @InstanceIds@ key with a value of @*@. For more information about
    -- choosing targets for an association, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    targets :: Prelude.Maybe [Target],
    -- | The name of the SSM Command document or Automation runbook that contains
    -- the configuration information for the managed node.
    --
    -- You can specify Amazon Web Services-predefined documents, documents you
    -- created, or a document that is shared with you from another account.
    --
    -- For Systems Manager documents (SSM documents) that are shared with you
    -- from other Amazon Web Services accounts, you must specify the complete
    -- SSM document ARN, in the following format:
    --
    -- @arn:partition:ssm:region:account-id:document\/document-name @
    --
    -- For example:
    --
    -- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
    --
    -- For Amazon Web Services-predefined documents and SSM documents you
    -- created in your account, you only need to specify the document name. For
    -- example, @AWS-ApplyPatchBaseline@ or @My-Document@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmConfiguration', 'createAssociation_alarmConfiguration' - Undocumented member.
--
-- 'applyOnlyAtCronInterval', 'createAssociation_applyOnlyAtCronInterval' - By default, when you create a new association, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
--
-- 'associationName', 'createAssociation_associationName' - Specify a descriptive name for the association.
--
-- 'automationTargetParameterName', 'createAssociation_automationTargetParameterName' - Choose the parameter that will define how your automation will branch
-- out. This target is required for associations that use an Automation
-- runbook and target resources by using rate controls. Automation is a
-- capability of Amazon Web Services Systems Manager.
--
-- 'calendarNames', 'createAssociation_calendarNames' - The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents you want to gate your associations under. The associations
-- only run when that change calendar is open. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
--
-- 'complianceSeverity', 'createAssociation_complianceSeverity' - The severity level to assign to the association.
--
-- 'documentVersion', 'createAssociation_documentVersion' - The document version you want to associate with the target(s). Can be a
-- specific version or the default version.
--
-- State Manager doesn\'t support running associations that use a new
-- version of a document if that document is shared from another account.
-- State Manager always runs the @default@ version of a document if shared
-- from another account, even though the Systems Manager console shows that
-- a new version was processed. If you want to run an association using a
-- new version of a document shared form another account, you must set the
-- document version to @default@.
--
-- 'instanceId', 'createAssociation_instanceId' - The managed node ID.
--
-- @InstanceId@ has been deprecated. To specify a managed node ID for an
-- association, use the @Targets@ parameter. Requests that include the
-- parameter @InstanceID@ with Systems Manager documents (SSM documents)
-- that use schema version 2.0 or later will fail. In addition, if you use
-- the parameter @InstanceId@, you can\'t use the parameters
-- @AssociationName@, @DocumentVersion@, @MaxErrors@, @MaxConcurrency@,
-- @OutputLocation@, or @ScheduleExpression@. To use these parameters, you
-- must use the @Targets@ parameter.
--
-- 'maxConcurrency', 'createAssociation_maxConcurrency' - The maximum number of targets allowed to run the association at the same
-- time. You can specify a number, for example 10, or a percentage of the
-- target set, for example 10%. The default value is 100%, which means all
-- targets run the association at the same time.
--
-- If a new managed node starts and attempts to run an association while
-- Systems Manager is running @MaxConcurrency@ associations, the
-- association is allowed to run. During the next association interval, the
-- new managed node will process its association within the limit specified
-- for @MaxConcurrency@.
--
-- 'maxErrors', 'createAssociation_maxErrors' - The number of errors that are allowed before the system stops sending
-- requests to run the association on additional targets. You can specify
-- either an absolute number of errors, for example 10, or a percentage of
-- the target set, for example 10%. If you specify 3, for example, the
-- system stops sending requests when the fourth error is received. If you
-- specify 0, then the system stops sending requests after the first error
-- is returned. If you run an association on 50 managed nodes and set
-- @MaxError@ to 10%, then the system stops sending the request when the
-- sixth error is received.
--
-- Executions that are already running an association when @MaxErrors@ is
-- reached are allowed to complete, but some of these executions may fail
-- as well. If you need to ensure that there won\'t be more than max-errors
-- failed executions, set @MaxConcurrency@ to 1 so that executions proceed
-- one at a time.
--
-- 'outputLocation', 'createAssociation_outputLocation' - An Amazon Simple Storage Service (Amazon S3) bucket where you want to
-- store the output details of the request.
--
-- 'parameters', 'createAssociation_parameters' - The parameters for the runtime configuration of the document.
--
-- 'scheduleExpression', 'createAssociation_scheduleExpression' - A cron expression when the association will be applied to the target(s).
--
-- 'scheduleOffset', 'createAssociation_scheduleOffset' - Number of days to wait after the scheduled day to run an association.
-- For example, if you specified a cron schedule of
-- @cron(0 0 ? * THU#2 *)@, you could specify an offset of 3 to run the
-- association each Sunday after the second Thursday of the month. For more
-- information about cron schedules for associations, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/reference-cron-and-rate-expressions.html Reference: Cron and rate expressions for Systems Manager>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- To use offsets, you must specify the @ApplyOnlyAtCronInterval@
-- parameter. This option tells the system not to run an association
-- immediately after you create it.
--
-- 'syncCompliance', 'createAssociation_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
-- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
-- association execution to determine the compliance status. If the
-- association execution runs successfully, then the association is
-- @COMPLIANT@. If the association execution doesn\'t run successfully, the
-- association is @NON-COMPLIANT@.
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
-- for the PutComplianceItems API operation. In this case, compliance data
-- isn\'t managed by State Manager. It is managed by your direct call to
-- the PutComplianceItems API operation.
--
-- By default, all associations use @AUTO@ mode.
--
-- 'tags', 'createAssociation_tags' - Adds or overwrites one or more tags for a State Manager association.
-- /Tags/ are metadata that you can assign to your Amazon Web Services
-- resources. Tags enable you to categorize your resources in different
-- ways, for example, by purpose, owner, or environment. Each tag consists
-- of a key and an optional value, both of which you define.
--
-- 'targetLocations', 'createAssociation_targetLocations' - A location is a combination of Amazon Web Services Regions and Amazon
-- Web Services accounts where you want to run the association. Use this
-- action to create an association in multiple Regions and multiple
-- accounts.
--
-- 'targetMaps', 'createAssociation_targetMaps' - A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
--
-- 'targets', 'createAssociation_targets' - The targets for the association. You can target managed nodes by using
-- tags, Amazon Web Services resource groups, all managed nodes in an
-- Amazon Web Services account, or individual managed node IDs. You can
-- target all managed nodes in an Amazon Web Services account by specifying
-- the @InstanceIds@ key with a value of @*@. For more information about
-- choosing targets for an association, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'name', 'createAssociation_name' - The name of the SSM Command document or Automation runbook that contains
-- the configuration information for the managed node.
--
-- You can specify Amazon Web Services-predefined documents, documents you
-- created, or a document that is shared with you from another account.
--
-- For Systems Manager documents (SSM documents) that are shared with you
-- from other Amazon Web Services accounts, you must specify the complete
-- SSM document ARN, in the following format:
--
-- @arn:partition:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For Amazon Web Services-predefined documents and SSM documents you
-- created in your account, you only need to specify the document name. For
-- example, @AWS-ApplyPatchBaseline@ or @My-Document@.
newCreateAssociation ::
  -- | 'name'
  Prelude.Text ->
  CreateAssociation
newCreateAssociation pName_ =
  CreateAssociation'
    { alarmConfiguration =
        Prelude.Nothing,
      applyOnlyAtCronInterval = Prelude.Nothing,
      associationName = Prelude.Nothing,
      automationTargetParameterName = Prelude.Nothing,
      calendarNames = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      parameters = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      syncCompliance = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      targetMaps = Prelude.Nothing,
      targets = Prelude.Nothing,
      name = pName_
    }

-- | Undocumented member.
createAssociation_alarmConfiguration :: Lens.Lens' CreateAssociation (Prelude.Maybe AlarmConfiguration)
createAssociation_alarmConfiguration = Lens.lens (\CreateAssociation' {alarmConfiguration} -> alarmConfiguration) (\s@CreateAssociation' {} a -> s {alarmConfiguration = a} :: CreateAssociation)

-- | By default, when you create a new association, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
createAssociation_applyOnlyAtCronInterval :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Bool)
createAssociation_applyOnlyAtCronInterval = Lens.lens (\CreateAssociation' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@CreateAssociation' {} a -> s {applyOnlyAtCronInterval = a} :: CreateAssociation)

-- | Specify a descriptive name for the association.
createAssociation_associationName :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_associationName = Lens.lens (\CreateAssociation' {associationName} -> associationName) (\s@CreateAssociation' {} a -> s {associationName = a} :: CreateAssociation)

-- | Choose the parameter that will define how your automation will branch
-- out. This target is required for associations that use an Automation
-- runbook and target resources by using rate controls. Automation is a
-- capability of Amazon Web Services Systems Manager.
createAssociation_automationTargetParameterName :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_automationTargetParameterName = Lens.lens (\CreateAssociation' {automationTargetParameterName} -> automationTargetParameterName) (\s@CreateAssociation' {} a -> s {automationTargetParameterName = a} :: CreateAssociation)

-- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents you want to gate your associations under. The associations
-- only run when that change calendar is open. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
createAssociation_calendarNames :: Lens.Lens' CreateAssociation (Prelude.Maybe [Prelude.Text])
createAssociation_calendarNames = Lens.lens (\CreateAssociation' {calendarNames} -> calendarNames) (\s@CreateAssociation' {} a -> s {calendarNames = a} :: CreateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The severity level to assign to the association.
createAssociation_complianceSeverity :: Lens.Lens' CreateAssociation (Prelude.Maybe AssociationComplianceSeverity)
createAssociation_complianceSeverity = Lens.lens (\CreateAssociation' {complianceSeverity} -> complianceSeverity) (\s@CreateAssociation' {} a -> s {complianceSeverity = a} :: CreateAssociation)

-- | The document version you want to associate with the target(s). Can be a
-- specific version or the default version.
--
-- State Manager doesn\'t support running associations that use a new
-- version of a document if that document is shared from another account.
-- State Manager always runs the @default@ version of a document if shared
-- from another account, even though the Systems Manager console shows that
-- a new version was processed. If you want to run an association using a
-- new version of a document shared form another account, you must set the
-- document version to @default@.
createAssociation_documentVersion :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_documentVersion = Lens.lens (\CreateAssociation' {documentVersion} -> documentVersion) (\s@CreateAssociation' {} a -> s {documentVersion = a} :: CreateAssociation)

-- | The managed node ID.
--
-- @InstanceId@ has been deprecated. To specify a managed node ID for an
-- association, use the @Targets@ parameter. Requests that include the
-- parameter @InstanceID@ with Systems Manager documents (SSM documents)
-- that use schema version 2.0 or later will fail. In addition, if you use
-- the parameter @InstanceId@, you can\'t use the parameters
-- @AssociationName@, @DocumentVersion@, @MaxErrors@, @MaxConcurrency@,
-- @OutputLocation@, or @ScheduleExpression@. To use these parameters, you
-- must use the @Targets@ parameter.
createAssociation_instanceId :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_instanceId = Lens.lens (\CreateAssociation' {instanceId} -> instanceId) (\s@CreateAssociation' {} a -> s {instanceId = a} :: CreateAssociation)

-- | The maximum number of targets allowed to run the association at the same
-- time. You can specify a number, for example 10, or a percentage of the
-- target set, for example 10%. The default value is 100%, which means all
-- targets run the association at the same time.
--
-- If a new managed node starts and attempts to run an association while
-- Systems Manager is running @MaxConcurrency@ associations, the
-- association is allowed to run. During the next association interval, the
-- new managed node will process its association within the limit specified
-- for @MaxConcurrency@.
createAssociation_maxConcurrency :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_maxConcurrency = Lens.lens (\CreateAssociation' {maxConcurrency} -> maxConcurrency) (\s@CreateAssociation' {} a -> s {maxConcurrency = a} :: CreateAssociation)

-- | The number of errors that are allowed before the system stops sending
-- requests to run the association on additional targets. You can specify
-- either an absolute number of errors, for example 10, or a percentage of
-- the target set, for example 10%. If you specify 3, for example, the
-- system stops sending requests when the fourth error is received. If you
-- specify 0, then the system stops sending requests after the first error
-- is returned. If you run an association on 50 managed nodes and set
-- @MaxError@ to 10%, then the system stops sending the request when the
-- sixth error is received.
--
-- Executions that are already running an association when @MaxErrors@ is
-- reached are allowed to complete, but some of these executions may fail
-- as well. If you need to ensure that there won\'t be more than max-errors
-- failed executions, set @MaxConcurrency@ to 1 so that executions proceed
-- one at a time.
createAssociation_maxErrors :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_maxErrors = Lens.lens (\CreateAssociation' {maxErrors} -> maxErrors) (\s@CreateAssociation' {} a -> s {maxErrors = a} :: CreateAssociation)

-- | An Amazon Simple Storage Service (Amazon S3) bucket where you want to
-- store the output details of the request.
createAssociation_outputLocation :: Lens.Lens' CreateAssociation (Prelude.Maybe InstanceAssociationOutputLocation)
createAssociation_outputLocation = Lens.lens (\CreateAssociation' {outputLocation} -> outputLocation) (\s@CreateAssociation' {} a -> s {outputLocation = a} :: CreateAssociation)

-- | The parameters for the runtime configuration of the document.
createAssociation_parameters :: Lens.Lens' CreateAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
createAssociation_parameters = Lens.lens (\CreateAssociation' {parameters} -> parameters) (\s@CreateAssociation' {} a -> s {parameters = a} :: CreateAssociation) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A cron expression when the association will be applied to the target(s).
createAssociation_scheduleExpression :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Text)
createAssociation_scheduleExpression = Lens.lens (\CreateAssociation' {scheduleExpression} -> scheduleExpression) (\s@CreateAssociation' {} a -> s {scheduleExpression = a} :: CreateAssociation)

-- | Number of days to wait after the scheduled day to run an association.
-- For example, if you specified a cron schedule of
-- @cron(0 0 ? * THU#2 *)@, you could specify an offset of 3 to run the
-- association each Sunday after the second Thursday of the month. For more
-- information about cron schedules for associations, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/reference-cron-and-rate-expressions.html Reference: Cron and rate expressions for Systems Manager>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- To use offsets, you must specify the @ApplyOnlyAtCronInterval@
-- parameter. This option tells the system not to run an association
-- immediately after you create it.
createAssociation_scheduleOffset :: Lens.Lens' CreateAssociation (Prelude.Maybe Prelude.Natural)
createAssociation_scheduleOffset = Lens.lens (\CreateAssociation' {scheduleOffset} -> scheduleOffset) (\s@CreateAssociation' {} a -> s {scheduleOffset = a} :: CreateAssociation)

-- | The mode for generating association compliance. You can specify @AUTO@
-- or @MANUAL@. In @AUTO@ mode, the system uses the status of the
-- association execution to determine the compliance status. If the
-- association execution runs successfully, then the association is
-- @COMPLIANT@. If the association execution doesn\'t run successfully, the
-- association is @NON-COMPLIANT@.
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter
-- for the PutComplianceItems API operation. In this case, compliance data
-- isn\'t managed by State Manager. It is managed by your direct call to
-- the PutComplianceItems API operation.
--
-- By default, all associations use @AUTO@ mode.
createAssociation_syncCompliance :: Lens.Lens' CreateAssociation (Prelude.Maybe AssociationSyncCompliance)
createAssociation_syncCompliance = Lens.lens (\CreateAssociation' {syncCompliance} -> syncCompliance) (\s@CreateAssociation' {} a -> s {syncCompliance = a} :: CreateAssociation)

-- | Adds or overwrites one or more tags for a State Manager association.
-- /Tags/ are metadata that you can assign to your Amazon Web Services
-- resources. Tags enable you to categorize your resources in different
-- ways, for example, by purpose, owner, or environment. Each tag consists
-- of a key and an optional value, both of which you define.
createAssociation_tags :: Lens.Lens' CreateAssociation (Prelude.Maybe [Tag])
createAssociation_tags = Lens.lens (\CreateAssociation' {tags} -> tags) (\s@CreateAssociation' {} a -> s {tags = a} :: CreateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | A location is a combination of Amazon Web Services Regions and Amazon
-- Web Services accounts where you want to run the association. Use this
-- action to create an association in multiple Regions and multiple
-- accounts.
createAssociation_targetLocations :: Lens.Lens' CreateAssociation (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
createAssociation_targetLocations = Lens.lens (\CreateAssociation' {targetLocations} -> targetLocations) (\s@CreateAssociation' {} a -> s {targetLocations = a} :: CreateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
createAssociation_targetMaps :: Lens.Lens' CreateAssociation (Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]])
createAssociation_targetMaps = Lens.lens (\CreateAssociation' {targetMaps} -> targetMaps) (\s@CreateAssociation' {} a -> s {targetMaps = a} :: CreateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the association. You can target managed nodes by using
-- tags, Amazon Web Services resource groups, all managed nodes in an
-- Amazon Web Services account, or individual managed node IDs. You can
-- target all managed nodes in an Amazon Web Services account by specifying
-- the @InstanceIds@ key with a value of @*@. For more information about
-- choosing targets for an association, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html Using targets and rate controls with State Manager associations>
-- in the /Amazon Web Services Systems Manager User Guide/.
createAssociation_targets :: Lens.Lens' CreateAssociation (Prelude.Maybe [Target])
createAssociation_targets = Lens.lens (\CreateAssociation' {targets} -> targets) (\s@CreateAssociation' {} a -> s {targets = a} :: CreateAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the SSM Command document or Automation runbook that contains
-- the configuration information for the managed node.
--
-- You can specify Amazon Web Services-predefined documents, documents you
-- created, or a document that is shared with you from another account.
--
-- For Systems Manager documents (SSM documents) that are shared with you
-- from other Amazon Web Services accounts, you must specify the complete
-- SSM document ARN, in the following format:
--
-- @arn:partition:ssm:region:account-id:document\/document-name @
--
-- For example:
--
-- @arn:aws:ssm:us-east-2:12345678912:document\/My-Shared-Document@
--
-- For Amazon Web Services-predefined documents and SSM documents you
-- created in your account, you only need to specify the document name. For
-- example, @AWS-ApplyPatchBaseline@ or @My-Document@.
createAssociation_name :: Lens.Lens' CreateAssociation Prelude.Text
createAssociation_name = Lens.lens (\CreateAssociation' {name} -> name) (\s@CreateAssociation' {} a -> s {name = a} :: CreateAssociation)

instance Core.AWSRequest CreateAssociation where
  type
    AWSResponse CreateAssociation =
      CreateAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssociationResponse'
            Prelude.<$> (x Data..?> "AssociationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssociation where
  hashWithSalt _salt CreateAssociation' {..} =
    _salt `Prelude.hashWithSalt` alarmConfiguration
      `Prelude.hashWithSalt` applyOnlyAtCronInterval
      `Prelude.hashWithSalt` associationName
      `Prelude.hashWithSalt` automationTargetParameterName
      `Prelude.hashWithSalt` calendarNames
      `Prelude.hashWithSalt` complianceSeverity
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` syncCompliance
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetLocations
      `Prelude.hashWithSalt` targetMaps
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateAssociation where
  rnf CreateAssociation' {..} =
    Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf applyOnlyAtCronInterval
      `Prelude.seq` Prelude.rnf associationName
      `Prelude.seq` Prelude.rnf automationTargetParameterName
      `Prelude.seq` Prelude.rnf calendarNames
      `Prelude.seq` Prelude.rnf complianceSeverity
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf scheduleOffset
      `Prelude.seq` Prelude.rnf syncCompliance
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetLocations
      `Prelude.seq` Prelude.rnf targetMaps
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.CreateAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssociation where
  toJSON CreateAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlarmConfiguration" Data..=)
              Prelude.<$> alarmConfiguration,
            ("ApplyOnlyAtCronInterval" Data..=)
              Prelude.<$> applyOnlyAtCronInterval,
            ("AssociationName" Data..=)
              Prelude.<$> associationName,
            ("AutomationTargetParameterName" Data..=)
              Prelude.<$> automationTargetParameterName,
            ("CalendarNames" Data..=) Prelude.<$> calendarNames,
            ("ComplianceSeverity" Data..=)
              Prelude.<$> complianceSeverity,
            ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("MaxConcurrency" Data..=)
              Prelude.<$> maxConcurrency,
            ("MaxErrors" Data..=) Prelude.<$> maxErrors,
            ("OutputLocation" Data..=)
              Prelude.<$> outputLocation,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("ScheduleExpression" Data..=)
              Prelude.<$> scheduleExpression,
            ("ScheduleOffset" Data..=)
              Prelude.<$> scheduleOffset,
            ("SyncCompliance" Data..=)
              Prelude.<$> syncCompliance,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetLocations" Data..=)
              Prelude.<$> targetLocations,
            ("TargetMaps" Data..=) Prelude.<$> targetMaps,
            ("Targets" Data..=) Prelude.<$> targets,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssociationResponse' smart constructor.
data CreateAssociationResponse = CreateAssociationResponse'
  { -- | Information about the association.
    associationDescription :: Prelude.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateAssociationResponse where
  rnf CreateAssociationResponse' {..} =
    Prelude.rnf associationDescription
      `Prelude.seq` Prelude.rnf httpStatus
