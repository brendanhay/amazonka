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
-- Module      : Network.AWS.SSM.Types.AssociationVersionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationVersionInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Information about the association version.
--
-- /See:/ 'newAssociationVersionInfo' smart constructor.
data AssociationVersionInfo = AssociationVersionInfo'
  { -- | The number of errors that are allowed before the system stops sending
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
    -- | The date the association version was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The severity level that is assigned to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
    -- | The targets specified for the association when the association version
    -- was created.
    targets :: Prelude.Maybe [Target],
    -- | The name specified when the association was created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The cron or rate schedule specified for the association when the
    -- association version was created.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The combination of Amazon Web Services Regions and Amazon Web Services
    -- accounts where you wanted to run the association when this association
    -- version was created.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | The ID created by the system when the association was created.
    associationId :: Prelude.Maybe Prelude.Text,
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
    -- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
    -- documents your associations are gated under. The associations for this
    -- version only run when that Change Calendar is open. For more
    -- information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
    calendarNames :: Prelude.Maybe [Prelude.Text],
    -- | The name specified for the association version when the association
    -- version was created.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of an Amazon Web Services Systems Manager document (SSM
    -- document) used when the association version was created.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Parameters specified when the association version was created.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
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
    -- | The location in Amazon S3 specified for the association when the
    -- association version was created.
    outputLocation :: Prelude.Maybe InstanceAssociationOutputLocation,
    -- | By default, when you create a new associations, the system runs it
    -- immediately after it is created and then according to the schedule you
    -- specified. Specify this option if you don\'t want an association to run
    -- immediately after you create it. This parameter isn\'t supported for
    -- rate expressions.
    applyOnlyAtCronInterval :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'associationVersionInfo_maxErrors' - The number of errors that are allowed before the system stops sending
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
-- 'createdDate', 'associationVersionInfo_createdDate' - The date the association version was created.
--
-- 'complianceSeverity', 'associationVersionInfo_complianceSeverity' - The severity level that is assigned to the association.
--
-- 'targets', 'associationVersionInfo_targets' - The targets specified for the association when the association version
-- was created.
--
-- 'name', 'associationVersionInfo_name' - The name specified when the association was created.
--
-- 'scheduleExpression', 'associationVersionInfo_scheduleExpression' - The cron or rate schedule specified for the association when the
-- association version was created.
--
-- 'targetLocations', 'associationVersionInfo_targetLocations' - The combination of Amazon Web Services Regions and Amazon Web Services
-- accounts where you wanted to run the association when this association
-- version was created.
--
-- 'associationId', 'associationVersionInfo_associationId' - The ID created by the system when the association was created.
--
-- 'maxConcurrency', 'associationVersionInfo_maxConcurrency' - The maximum number of targets allowed to run the association at the same
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
-- 'calendarNames', 'associationVersionInfo_calendarNames' - The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents your associations are gated under. The associations for this
-- version only run when that Change Calendar is open. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
--
-- 'associationName', 'associationVersionInfo_associationName' - The name specified for the association version when the association
-- version was created.
--
-- 'associationVersion', 'associationVersionInfo_associationVersion' - The association version.
--
-- 'documentVersion', 'associationVersionInfo_documentVersion' - The version of an Amazon Web Services Systems Manager document (SSM
-- document) used when the association version was created.
--
-- 'parameters', 'associationVersionInfo_parameters' - Parameters specified when the association version was created.
--
-- 'syncCompliance', 'associationVersionInfo_syncCompliance' - The mode for generating association compliance. You can specify @AUTO@
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
-- 'outputLocation', 'associationVersionInfo_outputLocation' - The location in Amazon S3 specified for the association when the
-- association version was created.
--
-- 'applyOnlyAtCronInterval', 'associationVersionInfo_applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
newAssociationVersionInfo ::
  AssociationVersionInfo
newAssociationVersionInfo =
  AssociationVersionInfo'
    { maxErrors =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      targets = Prelude.Nothing,
      name = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      associationId = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      calendarNames = Prelude.Nothing,
      associationName = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      syncCompliance = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      applyOnlyAtCronInterval = Prelude.Nothing
    }

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
associationVersionInfo_maxErrors :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_maxErrors = Lens.lens (\AssociationVersionInfo' {maxErrors} -> maxErrors) (\s@AssociationVersionInfo' {} a -> s {maxErrors = a} :: AssociationVersionInfo)

-- | The date the association version was created.
associationVersionInfo_createdDate :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.UTCTime)
associationVersionInfo_createdDate = Lens.lens (\AssociationVersionInfo' {createdDate} -> createdDate) (\s@AssociationVersionInfo' {} a -> s {createdDate = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Core._Time

-- | The severity level that is assigned to the association.
associationVersionInfo_complianceSeverity :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe AssociationComplianceSeverity)
associationVersionInfo_complianceSeverity = Lens.lens (\AssociationVersionInfo' {complianceSeverity} -> complianceSeverity) (\s@AssociationVersionInfo' {} a -> s {complianceSeverity = a} :: AssociationVersionInfo)

-- | The targets specified for the association when the association version
-- was created.
associationVersionInfo_targets :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe [Target])
associationVersionInfo_targets = Lens.lens (\AssociationVersionInfo' {targets} -> targets) (\s@AssociationVersionInfo' {} a -> s {targets = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens._Coerce

-- | The name specified when the association was created.
associationVersionInfo_name :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_name = Lens.lens (\AssociationVersionInfo' {name} -> name) (\s@AssociationVersionInfo' {} a -> s {name = a} :: AssociationVersionInfo)

-- | The cron or rate schedule specified for the association when the
-- association version was created.
associationVersionInfo_scheduleExpression :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_scheduleExpression = Lens.lens (\AssociationVersionInfo' {scheduleExpression} -> scheduleExpression) (\s@AssociationVersionInfo' {} a -> s {scheduleExpression = a} :: AssociationVersionInfo)

-- | The combination of Amazon Web Services Regions and Amazon Web Services
-- accounts where you wanted to run the association when this association
-- version was created.
associationVersionInfo_targetLocations :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
associationVersionInfo_targetLocations = Lens.lens (\AssociationVersionInfo' {targetLocations} -> targetLocations) (\s@AssociationVersionInfo' {} a -> s {targetLocations = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens._Coerce

-- | The ID created by the system when the association was created.
associationVersionInfo_associationId :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_associationId = Lens.lens (\AssociationVersionInfo' {associationId} -> associationId) (\s@AssociationVersionInfo' {} a -> s {associationId = a} :: AssociationVersionInfo)

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
associationVersionInfo_maxConcurrency :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_maxConcurrency = Lens.lens (\AssociationVersionInfo' {maxConcurrency} -> maxConcurrency) (\s@AssociationVersionInfo' {} a -> s {maxConcurrency = a} :: AssociationVersionInfo)

-- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents your associations are gated under. The associations for this
-- version only run when that Change Calendar is open. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
associationVersionInfo_calendarNames :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe [Prelude.Text])
associationVersionInfo_calendarNames = Lens.lens (\AssociationVersionInfo' {calendarNames} -> calendarNames) (\s@AssociationVersionInfo' {} a -> s {calendarNames = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens._Coerce

-- | The name specified for the association version when the association
-- version was created.
associationVersionInfo_associationName :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_associationName = Lens.lens (\AssociationVersionInfo' {associationName} -> associationName) (\s@AssociationVersionInfo' {} a -> s {associationName = a} :: AssociationVersionInfo)

-- | The association version.
associationVersionInfo_associationVersion :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_associationVersion = Lens.lens (\AssociationVersionInfo' {associationVersion} -> associationVersion) (\s@AssociationVersionInfo' {} a -> s {associationVersion = a} :: AssociationVersionInfo)

-- | The version of an Amazon Web Services Systems Manager document (SSM
-- document) used when the association version was created.
associationVersionInfo_documentVersion :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_documentVersion = Lens.lens (\AssociationVersionInfo' {documentVersion} -> documentVersion) (\s@AssociationVersionInfo' {} a -> s {documentVersion = a} :: AssociationVersionInfo)

-- | Parameters specified when the association version was created.
associationVersionInfo_parameters :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
associationVersionInfo_parameters = Lens.lens (\AssociationVersionInfo' {parameters} -> parameters) (\s@AssociationVersionInfo' {} a -> s {parameters = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens._Coerce

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
associationVersionInfo_syncCompliance :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe AssociationSyncCompliance)
associationVersionInfo_syncCompliance = Lens.lens (\AssociationVersionInfo' {syncCompliance} -> syncCompliance) (\s@AssociationVersionInfo' {} a -> s {syncCompliance = a} :: AssociationVersionInfo)

-- | The location in Amazon S3 specified for the association when the
-- association version was created.
associationVersionInfo_outputLocation :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe InstanceAssociationOutputLocation)
associationVersionInfo_outputLocation = Lens.lens (\AssociationVersionInfo' {outputLocation} -> outputLocation) (\s@AssociationVersionInfo' {} a -> s {outputLocation = a} :: AssociationVersionInfo)

-- | By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
associationVersionInfo_applyOnlyAtCronInterval :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Bool)
associationVersionInfo_applyOnlyAtCronInterval = Lens.lens (\AssociationVersionInfo' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@AssociationVersionInfo' {} a -> s {applyOnlyAtCronInterval = a} :: AssociationVersionInfo)

instance Core.FromJSON AssociationVersionInfo where
  parseJSON =
    Core.withObject
      "AssociationVersionInfo"
      ( \x ->
          AssociationVersionInfo'
            Prelude.<$> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "ComplianceSeverity")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "TargetLocations")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "CalendarNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AssociationName")
            Prelude.<*> (x Core..:? "AssociationVersion")
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SyncCompliance")
            Prelude.<*> (x Core..:? "OutputLocation")
            Prelude.<*> (x Core..:? "ApplyOnlyAtCronInterval")
      )

instance Prelude.Hashable AssociationVersionInfo

instance Prelude.NFData AssociationVersionInfo
