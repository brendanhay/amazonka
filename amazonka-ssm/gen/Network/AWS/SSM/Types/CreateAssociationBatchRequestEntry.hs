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
-- Module      : Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Describes the association of a Systems Manager SSM document and an
-- instance.
--
-- /See:/ 'newCreateAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
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
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The severity level to assign to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
    -- | Specify the target for the association. This target is required for
    -- associations that use an Automation document and target resources by
    -- using rate controls.
    automationTargetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The instances targeted by the request.
    targets :: Prelude.Maybe [Target],
    -- | Use this action to create an association in multiple Regions and
    -- multiple accounts.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | A cron expression that specifies a schedule when the association runs.
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
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameters for a document.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | An S3 bucket where you want to store the results of this request.
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
    -- @arn:aws:ssm:region:account-id:document\/document-name @
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
-- Create a value of 'CreateAssociationBatchRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'createAssociationBatchRequestEntry_maxErrors' - The number of errors that are allowed before the system stops sending
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
-- 'instanceId', 'createAssociationBatchRequestEntry_instanceId' - The ID of the instance.
--
-- 'complianceSeverity', 'createAssociationBatchRequestEntry_complianceSeverity' - The severity level to assign to the association.
--
-- 'automationTargetParameterName', 'createAssociationBatchRequestEntry_automationTargetParameterName' - Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
--
-- 'targets', 'createAssociationBatchRequestEntry_targets' - The instances targeted by the request.
--
-- 'targetLocations', 'createAssociationBatchRequestEntry_targetLocations' - Use this action to create an association in multiple Regions and
-- multiple accounts.
--
-- 'scheduleExpression', 'createAssociationBatchRequestEntry_scheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- 'maxConcurrency', 'createAssociationBatchRequestEntry_maxConcurrency' - The maximum number of targets allowed to run the association at the same
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
-- 'associationName', 'createAssociationBatchRequestEntry_associationName' - Specify a descriptive name for the association.
--
-- 'documentVersion', 'createAssociationBatchRequestEntry_documentVersion' - The document version.
--
-- 'parameters', 'createAssociationBatchRequestEntry_parameters' - A description of the parameters for a document.
--
-- 'outputLocation', 'createAssociationBatchRequestEntry_outputLocation' - An S3 bucket where you want to store the results of this request.
--
-- 'applyOnlyAtCronInterval', 'createAssociationBatchRequestEntry_applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter is not supported for
-- rate expressions.
--
-- 'syncCompliance', 'createAssociationBatchRequestEntry_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
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
-- 'name', 'createAssociationBatchRequestEntry_name' - The name of the SSM document that contains the configuration information
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
newCreateAssociationBatchRequestEntry ::
  -- | 'name'
  Prelude.Text ->
  CreateAssociationBatchRequestEntry
newCreateAssociationBatchRequestEntry pName_ =
  CreateAssociationBatchRequestEntry'
    { maxErrors =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      automationTargetParameterName =
        Prelude.Nothing,
      targets = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      associationName = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      applyOnlyAtCronInterval =
        Prelude.Nothing,
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
createAssociationBatchRequestEntry_maxErrors :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_maxErrors = Lens.lens (\CreateAssociationBatchRequestEntry' {maxErrors} -> maxErrors) (\s@CreateAssociationBatchRequestEntry' {} a -> s {maxErrors = a} :: CreateAssociationBatchRequestEntry)

-- | The ID of the instance.
createAssociationBatchRequestEntry_instanceId :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_instanceId = Lens.lens (\CreateAssociationBatchRequestEntry' {instanceId} -> instanceId) (\s@CreateAssociationBatchRequestEntry' {} a -> s {instanceId = a} :: CreateAssociationBatchRequestEntry)

-- | The severity level to assign to the association.
createAssociationBatchRequestEntry_complianceSeverity :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe AssociationComplianceSeverity)
createAssociationBatchRequestEntry_complianceSeverity = Lens.lens (\CreateAssociationBatchRequestEntry' {complianceSeverity} -> complianceSeverity) (\s@CreateAssociationBatchRequestEntry' {} a -> s {complianceSeverity = a} :: CreateAssociationBatchRequestEntry)

-- | Specify the target for the association. This target is required for
-- associations that use an Automation document and target resources by
-- using rate controls.
createAssociationBatchRequestEntry_automationTargetParameterName :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_automationTargetParameterName = Lens.lens (\CreateAssociationBatchRequestEntry' {automationTargetParameterName} -> automationTargetParameterName) (\s@CreateAssociationBatchRequestEntry' {} a -> s {automationTargetParameterName = a} :: CreateAssociationBatchRequestEntry)

-- | The instances targeted by the request.
createAssociationBatchRequestEntry_targets :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe [Target])
createAssociationBatchRequestEntry_targets = Lens.lens (\CreateAssociationBatchRequestEntry' {targets} -> targets) (\s@CreateAssociationBatchRequestEntry' {} a -> s {targets = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping Lens._Coerce

-- | Use this action to create an association in multiple Regions and
-- multiple accounts.
createAssociationBatchRequestEntry_targetLocations :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
createAssociationBatchRequestEntry_targetLocations = Lens.lens (\CreateAssociationBatchRequestEntry' {targetLocations} -> targetLocations) (\s@CreateAssociationBatchRequestEntry' {} a -> s {targetLocations = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping Lens._Coerce

-- | A cron expression that specifies a schedule when the association runs.
createAssociationBatchRequestEntry_scheduleExpression :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_scheduleExpression = Lens.lens (\CreateAssociationBatchRequestEntry' {scheduleExpression} -> scheduleExpression) (\s@CreateAssociationBatchRequestEntry' {} a -> s {scheduleExpression = a} :: CreateAssociationBatchRequestEntry)

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
createAssociationBatchRequestEntry_maxConcurrency :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_maxConcurrency = Lens.lens (\CreateAssociationBatchRequestEntry' {maxConcurrency} -> maxConcurrency) (\s@CreateAssociationBatchRequestEntry' {} a -> s {maxConcurrency = a} :: CreateAssociationBatchRequestEntry)

-- | Specify a descriptive name for the association.
createAssociationBatchRequestEntry_associationName :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_associationName = Lens.lens (\CreateAssociationBatchRequestEntry' {associationName} -> associationName) (\s@CreateAssociationBatchRequestEntry' {} a -> s {associationName = a} :: CreateAssociationBatchRequestEntry)

-- | The document version.
createAssociationBatchRequestEntry_documentVersion :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Text)
createAssociationBatchRequestEntry_documentVersion = Lens.lens (\CreateAssociationBatchRequestEntry' {documentVersion} -> documentVersion) (\s@CreateAssociationBatchRequestEntry' {} a -> s {documentVersion = a} :: CreateAssociationBatchRequestEntry)

-- | A description of the parameters for a document.
createAssociationBatchRequestEntry_parameters :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
createAssociationBatchRequestEntry_parameters = Lens.lens (\CreateAssociationBatchRequestEntry' {parameters} -> parameters) (\s@CreateAssociationBatchRequestEntry' {} a -> s {parameters = a} :: CreateAssociationBatchRequestEntry) Prelude.. Lens.mapping Lens._Coerce

-- | An S3 bucket where you want to store the results of this request.
createAssociationBatchRequestEntry_outputLocation :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe InstanceAssociationOutputLocation)
createAssociationBatchRequestEntry_outputLocation = Lens.lens (\CreateAssociationBatchRequestEntry' {outputLocation} -> outputLocation) (\s@CreateAssociationBatchRequestEntry' {} a -> s {outputLocation = a} :: CreateAssociationBatchRequestEntry)

-- | By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter is not supported for
-- rate expressions.
createAssociationBatchRequestEntry_applyOnlyAtCronInterval :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe Prelude.Bool)
createAssociationBatchRequestEntry_applyOnlyAtCronInterval = Lens.lens (\CreateAssociationBatchRequestEntry' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@CreateAssociationBatchRequestEntry' {} a -> s {applyOnlyAtCronInterval = a} :: CreateAssociationBatchRequestEntry)

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
createAssociationBatchRequestEntry_syncCompliance :: Lens.Lens' CreateAssociationBatchRequestEntry (Prelude.Maybe AssociationSyncCompliance)
createAssociationBatchRequestEntry_syncCompliance = Lens.lens (\CreateAssociationBatchRequestEntry' {syncCompliance} -> syncCompliance) (\s@CreateAssociationBatchRequestEntry' {} a -> s {syncCompliance = a} :: CreateAssociationBatchRequestEntry)

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
            Prelude.<$> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "ComplianceSeverity")
            Prelude.<*> (x Core..:? "AutomationTargetParameterName")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TargetLocations")
            Prelude.<*> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "AssociationName")
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OutputLocation")
            Prelude.<*> (x Core..:? "ApplyOnlyAtCronInterval")
            Prelude.<*> (x Core..:? "SyncCompliance")
            Prelude.<*> (x Core..: "Name")
      )

instance
  Prelude.Hashable
    CreateAssociationBatchRequestEntry

instance
  Prelude.NFData
    CreateAssociationBatchRequestEntry

instance
  Core.ToJSON
    CreateAssociationBatchRequestEntry
  where
  toJSON CreateAssociationBatchRequestEntry' {..} =
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
