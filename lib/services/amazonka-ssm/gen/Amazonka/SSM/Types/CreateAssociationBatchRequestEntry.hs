{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Types.CreateAssociationBatchRequestEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.CreateAssociationBatchRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AlarmConfiguration
import Amazonka.SSM.Types.AssociationComplianceSeverity
import Amazonka.SSM.Types.AssociationSyncCompliance
import Amazonka.SSM.Types.InstanceAssociationOutputLocation
import Amazonka.SSM.Types.Target
import Amazonka.SSM.Types.TargetLocation

-- | Describes the association of a Amazon Web Services Systems Manager
-- document (SSM document) and a managed node.
--
-- /See:/ 'newCreateAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
  { -- | Specify a descriptive name for the association.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | Use this action to create an association in multiple Regions and
    -- multiple accounts.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | Specify the target for the association. This target is required for
    -- associations that use an Automation runbook and target resources by
    -- using rate controls. Automation is a capability of Amazon Web Services
    -- Systems Manager.
    automationTargetParameterName :: Prelude.Maybe Prelude.Text,
    -- | A key-value mapping of document parameters to target resources. Both
    -- Targets and TargetMaps can\'t be specified together.
    targetMaps :: Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]],
    -- | An S3 bucket where you want to store the results of this request.
    outputLocation :: Prelude.Maybe InstanceAssociationOutputLocation,
    -- | The managed nodes targeted by the request.
    targets :: Prelude.Maybe [Target],
    -- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
    -- documents your associations are gated under. The associations only run
    -- when that Change Calendar is open. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
    calendarNames :: Prelude.Maybe [Prelude.Text],
    -- | A cron expression that specifies a schedule when the association runs.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | Number of days to wait after the scheduled day to run an association.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
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
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
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
    -- | By default, when you create a new associations, the system runs it
    -- immediately after it is created and then according to the schedule you
    -- specified. Specify this option if you don\'t want an association to run
    -- immediately after you create it. This parameter isn\'t supported for
    -- rate expressions.
    applyOnlyAtCronInterval :: Prelude.Maybe Prelude.Bool,
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
    -- | The severity level to assign to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
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
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameters for a document.
    parameters :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text [Prelude.Text])),
    -- | The name of the SSM document that contains the configuration information
    -- for the managed node. You can specify Command or Automation runbooks.
    --
    -- You can specify Amazon Web Services-predefined documents, documents you
    -- created, or a document that is shared with you from another account.
    --
    -- For SSM documents that are shared with you from other Amazon Web
    -- Services accounts, you must specify the complete SSM document ARN, in
    -- the following format:
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
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssociationBatchRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationName', 'createAssociationBatchRequestEntry_associationName' - Specify a descriptive name for the association.
--
-- 'targetLocations', 'createAssociationBatchRequestEntry_targetLocations' - Use this action to create an association in multiple Regions and
-- multiple accounts.
--
-- 'automationTargetParameterName', 'createAssociationBatchRequestEntry_automationTargetParameterName' - Specify the target for the association. This target is required for
-- associations that use an Automation runbook and target resources by
-- using rate controls. Automation is a capability of Amazon Web Services
-- Systems Manager.
--
-- 'targetMaps', 'createAssociationBatchRequestEntry_targetMaps' - A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
--
-- 'outputLocation', 'createAssociationBatchRequestEntry_outputLocation' - An S3 bucket where you want to store the results of this request.
--
-- 'targets', 'createAssociationBatchRequestEntry_targets' - The managed nodes targeted by the request.
--
-- 'calendarNames', 'createAssociationBatchRequestEntry_calendarNames' - The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents your associations are gated under. The associations only run
-- when that Change Calendar is open. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
--
-- 'scheduleExpression', 'createAssociationBatchRequestEntry_scheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- 'scheduleOffset', 'createAssociationBatchRequestEntry_scheduleOffset' - Number of days to wait after the scheduled day to run an association.
--
-- 'instanceId', 'createAssociationBatchRequestEntry_instanceId' - The managed node ID.
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
-- 'alarmConfiguration', 'createAssociationBatchRequestEntry_alarmConfiguration' - Undocumented member.
--
-- 'maxConcurrency', 'createAssociationBatchRequestEntry_maxConcurrency' - The maximum number of targets allowed to run the association at the same
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
-- 'applyOnlyAtCronInterval', 'createAssociationBatchRequestEntry_applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
--
-- 'maxErrors', 'createAssociationBatchRequestEntry_maxErrors' - The number of errors that are allowed before the system stops sending
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
-- 'complianceSeverity', 'createAssociationBatchRequestEntry_complianceSeverity' - The severity level to assign to the association.
--
-- 'syncCompliance', 'createAssociationBatchRequestEntry_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
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
-- 'documentVersion', 'createAssociationBatchRequestEntry_documentVersion' - The document version.
--
-- 'parameters', 'createAssociationBatchRequestEntry_parameters' - A description of the parameters for a document.
--
-- 'name', 'createAssociationBatchRequestEntry_name' - The name of the SSM document that contains the configuration information
-- for the managed node. You can specify Command or Automation runbooks.
--
-- You can specify Amazon Web Services-predefined documents, documents you
-- created, or a document that is shared with you from another account.
--
-- For SSM documents that are shared with you from other Amazon Web
-- Services accounts, you must specify the complete SSM document ARN, in
-- the following format:
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
newCreateAssociationBatchRequestEntry ::
  -- | 'name'
  Prelude.Text ->
  CreateAssociationBatchRequestEntry
newCreateAssociationBatchRequestEntry pName_ =
  CreateAssociationBatchRequestEntry'
    { associationName =
        Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      automationTargetParameterName =
        Prelude.Nothing,
      targetMaps = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      targets = Prelude.Nothing,
      calendarNames = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      alarmConfiguration = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      applyOnlyAtCronInterval =
        Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      syncCompliance = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_
    }

-- | Specify a descriptive name for the association.
createAssociationBatchRequestEntry_associationName :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_associationName = Lens.lens (\CreateAssociationBatchRequestEntry' {associationName} -> associationName) (\s@CreateAssociationBatchRequestEntry' {} a -> s {associationName = a} :: CreateAssociationBatchRequestEntry)

-- | Use this action to create an association in multiple Regions and
-- multiple accounts.
createAssociationBatchRequestEntry_targetLocations :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
createAssociationBatchRequestEntry_targetLocations = Lens.lens (\CreateAssociationBatchRequestEntry' {targetLocations} -> targetLocations) (\s@CreateAssociationBatchRequestEntry' {} a -> s {targetLocations = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | Specify the target for the association. This target is required for
-- associations that use an Automation runbook and target resources by
-- using rate controls. Automation is a capability of Amazon Web Services
-- Systems Manager.
createAssociationBatchRequestEntry_automationTargetParameterName :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_automationTargetParameterName = Lens.lens (\CreateAssociationBatchRequestEntry' {automationTargetParameterName} -> automationTargetParameterName) (\s@CreateAssociationBatchRequestEntry' {} a -> s {automationTargetParameterName = a} :: CreateAssociationBatchRequestEntry)

-- | A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
createAssociationBatchRequestEntry_targetMaps :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]])
createAssociationBatchRequestEntry_targetMaps = Lens.lens (\CreateAssociationBatchRequestEntry' {targetMaps} -> targetMaps) (\s@CreateAssociationBatchRequestEntry' {} a -> s {targetMaps = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | An S3 bucket where you want to store the results of this request.
createAssociationBatchRequestEntry_outputLocation :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe InstanceAssociationOutputLocation)
createAssociationBatchRequestEntry_outputLocation = Lens.lens (\CreateAssociationBatchRequestEntry' {outputLocation} -> outputLocation) (\s@CreateAssociationBatchRequestEntry' {} a -> s {outputLocation = a} :: CreateAssociationBatchRequestEntry)

-- | The managed nodes targeted by the request.
createAssociationBatchRequestEntry_targets :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe [Target])
createAssociationBatchRequestEntry_targets = Lens.lens (\CreateAssociationBatchRequestEntry' {targets} -> targets) (\s@CreateAssociationBatchRequestEntry' {} a -> s {targets = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents your associations are gated under. The associations only run
-- when that Change Calendar is open. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
createAssociationBatchRequestEntry_calendarNames :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe [Prelude.Text])
createAssociationBatchRequestEntry_calendarNames = Lens.lens (\CreateAssociationBatchRequestEntry' {calendarNames} -> calendarNames) (\s@CreateAssociationBatchRequestEntry' {} a -> s {calendarNames = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | A cron expression that specifies a schedule when the association runs.
createAssociationBatchRequestEntry_scheduleExpression :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_scheduleExpression = Lens.lens (\CreateAssociationBatchRequestEntry' {scheduleExpression} -> scheduleExpression) (\s@CreateAssociationBatchRequestEntry' {} a -> s {scheduleExpression = a} :: CreateAssociationBatchRequestEntry)

-- | Number of days to wait after the scheduled day to run an association.
createAssociationBatchRequestEntry_scheduleOffset :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Natural)
createAssociationBatchRequestEntry_scheduleOffset = Lens.lens (\CreateAssociationBatchRequestEntry' {scheduleOffset} -> scheduleOffset) (\s@CreateAssociationBatchRequestEntry' {} a -> s {scheduleOffset = a} :: CreateAssociationBatchRequestEntry)

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
createAssociationBatchRequestEntry_instanceId :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_instanceId = Lens.lens (\CreateAssociationBatchRequestEntry' {instanceId} -> instanceId) (\s@CreateAssociationBatchRequestEntry' {} a -> s {instanceId = a} :: CreateAssociationBatchRequestEntry)

-- | Undocumented member.
createAssociationBatchRequestEntry_alarmConfiguration :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe AlarmConfiguration)
createAssociationBatchRequestEntry_alarmConfiguration = Lens.lens (\CreateAssociationBatchRequestEntry' {alarmConfiguration} -> alarmConfiguration) (\s@CreateAssociationBatchRequestEntry' {} a -> s {alarmConfiguration = a} :: CreateAssociationBatchRequestEntry)

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
createAssociationBatchRequestEntry_maxConcurrency :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_maxConcurrency = Lens.lens (\CreateAssociationBatchRequestEntry' {maxConcurrency} -> maxConcurrency) (\s@CreateAssociationBatchRequestEntry' {} a -> s {maxConcurrency = a} :: CreateAssociationBatchRequestEntry)

-- | By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
createAssociationBatchRequestEntry_applyOnlyAtCronInterval :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Bool)
createAssociationBatchRequestEntry_applyOnlyAtCronInterval = Lens.lens (\CreateAssociationBatchRequestEntry' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@CreateAssociationBatchRequestEntry' {} a -> s {applyOnlyAtCronInterval = a} :: CreateAssociationBatchRequestEntry)

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
createAssociationBatchRequestEntry_maxErrors :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_maxErrors = Lens.lens (\CreateAssociationBatchRequestEntry' {maxErrors} -> maxErrors) (\s@CreateAssociationBatchRequestEntry' {} a -> s {maxErrors = a} :: CreateAssociationBatchRequestEntry)

-- | The severity level to assign to the association.
createAssociationBatchRequestEntry_complianceSeverity :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe AssociationComplianceSeverity)
createAssociationBatchRequestEntry_complianceSeverity = Lens.lens (\CreateAssociationBatchRequestEntry' {complianceSeverity} -> complianceSeverity) (\s@CreateAssociationBatchRequestEntry' {} a -> s {complianceSeverity = a} :: CreateAssociationBatchRequestEntry)

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
createAssociationBatchRequestEntry_syncCompliance :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe AssociationSyncCompliance)
createAssociationBatchRequestEntry_syncCompliance = Lens.lens (\CreateAssociationBatchRequestEntry' {syncCompliance} -> syncCompliance) (\s@CreateAssociationBatchRequestEntry' {} a -> s {syncCompliance = a} :: CreateAssociationBatchRequestEntry)

-- | The document version.
createAssociationBatchRequestEntry_documentVersion :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_documentVersion = Lens.lens (\CreateAssociationBatchRequestEntry' {documentVersion} -> documentVersion) (\s@CreateAssociationBatchRequestEntry' {} a -> s {documentVersion = a} :: CreateAssociationBatchRequestEntry)

-- | A description of the parameters for a document.
createAssociationBatchRequestEntry_parameters :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
createAssociationBatchRequestEntry_parameters = Lens.lens (\CreateAssociationBatchRequestEntry' {parameters} -> parameters) (\s@CreateAssociationBatchRequestEntry' {} a -> s {parameters = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The name of the SSM document that contains the configuration information
-- for the managed node. You can specify Command or Automation runbooks.
--
-- You can specify Amazon Web Services-predefined documents, documents you
-- created, or a document that is shared with you from another account.
--
-- For SSM documents that are shared with you from other Amazon Web
-- Services accounts, you must specify the complete SSM document ARN, in
-- the following format:
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
createAssociationBatchRequestEntry_name :: Lens.Lens' CreateAssociationBatchRequestEntry Prelude.Text
createAssociationBatchRequestEntry_name = Lens.lens (\CreateAssociationBatchRequestEntry' {name} -> name) (\s@CreateAssociationBatchRequestEntry' {} a -> s {name = a} :: CreateAssociationBatchRequestEntry)

instance
  Core.FromJSON
    CreateAssociationBatchRequestEntry
  where
  parseJSON =
    Core.withObject
      "CreateAssociationBatchRequestEntry"
      ( \x ->
          CreateAssociationBatchRequestEntry'
            Prelude.<$> (x Core..:? "AssociationName")
            Prelude.<*> (x Core..:? "TargetLocations")
            Prelude.<*> (x Core..:? "AutomationTargetParameterName")
            Prelude.<*> (x Core..:? "TargetMaps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OutputLocation")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CalendarNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "ScheduleOffset")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "AlarmConfiguration")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "ApplyOnlyAtCronInterval")
            Prelude.<*> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "ComplianceSeverity")
            Prelude.<*> (x Core..:? "SyncCompliance")
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Name")
      )

instance
  Prelude.Hashable
    CreateAssociationBatchRequestEntry
  where
  hashWithSalt
    _salt
    CreateAssociationBatchRequestEntry' {..} =
      _salt `Prelude.hashWithSalt` associationName
        `Prelude.hashWithSalt` targetLocations
        `Prelude.hashWithSalt` automationTargetParameterName
        `Prelude.hashWithSalt` targetMaps
        `Prelude.hashWithSalt` outputLocation
        `Prelude.hashWithSalt` targets
        `Prelude.hashWithSalt` calendarNames
        `Prelude.hashWithSalt` scheduleExpression
        `Prelude.hashWithSalt` scheduleOffset
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` alarmConfiguration
        `Prelude.hashWithSalt` maxConcurrency
        `Prelude.hashWithSalt` applyOnlyAtCronInterval
        `Prelude.hashWithSalt` maxErrors
        `Prelude.hashWithSalt` complianceSeverity
        `Prelude.hashWithSalt` syncCompliance
        `Prelude.hashWithSalt` documentVersion
        `Prelude.hashWithSalt` parameters
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    CreateAssociationBatchRequestEntry
  where
  rnf CreateAssociationBatchRequestEntry' {..} =
    Prelude.rnf associationName
      `Prelude.seq` Prelude.rnf targetLocations
      `Prelude.seq` Prelude.rnf automationTargetParameterName
      `Prelude.seq` Prelude.rnf targetMaps
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf calendarNames
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf scheduleOffset
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf applyOnlyAtCronInterval
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf complianceSeverity
      `Prelude.seq` Prelude.rnf syncCompliance
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf name

instance
  Core.ToJSON
    CreateAssociationBatchRequestEntry
  where
  toJSON CreateAssociationBatchRequestEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AssociationName" Core..=)
              Prelude.<$> associationName,
            ("TargetLocations" Core..=)
              Prelude.<$> targetLocations,
            ("AutomationTargetParameterName" Core..=)
              Prelude.<$> automationTargetParameterName,
            ("TargetMaps" Core..=) Prelude.<$> targetMaps,
            ("OutputLocation" Core..=)
              Prelude.<$> outputLocation,
            ("Targets" Core..=) Prelude.<$> targets,
            ("CalendarNames" Core..=) Prelude.<$> calendarNames,
            ("ScheduleExpression" Core..=)
              Prelude.<$> scheduleExpression,
            ("ScheduleOffset" Core..=)
              Prelude.<$> scheduleOffset,
            ("InstanceId" Core..=) Prelude.<$> instanceId,
            ("AlarmConfiguration" Core..=)
              Prelude.<$> alarmConfiguration,
            ("MaxConcurrency" Core..=)
              Prelude.<$> maxConcurrency,
            ("ApplyOnlyAtCronInterval" Core..=)
              Prelude.<$> applyOnlyAtCronInterval,
            ("MaxErrors" Core..=) Prelude.<$> maxErrors,
            ("ComplianceSeverity" Core..=)
              Prelude.<$> complianceSeverity,
            ("SyncCompliance" Core..=)
              Prelude.<$> syncCompliance,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("Name" Core..= name)
          ]
      )
