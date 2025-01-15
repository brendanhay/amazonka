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
-- Module      : Amazonka.SSM.Types.AssociationVersionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationVersionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AssociationComplianceSeverity
import Amazonka.SSM.Types.AssociationSyncCompliance
import Amazonka.SSM.Types.InstanceAssociationOutputLocation
import Amazonka.SSM.Types.Target
import Amazonka.SSM.Types.TargetLocation

-- | Information about the association version.
--
-- /See:/ 'newAssociationVersionInfo' smart constructor.
data AssociationVersionInfo = AssociationVersionInfo'
  { -- | By default, when you create a new associations, the system runs it
    -- immediately after it is created and then according to the schedule you
    -- specified. Specify this option if you don\'t want an association to run
    -- immediately after you create it. This parameter isn\'t supported for
    -- rate expressions.
    applyOnlyAtCronInterval :: Prelude.Maybe Prelude.Bool,
    -- | The ID created by the system when the association was created.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The name specified for the association version when the association
    -- version was created.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
    -- documents your associations are gated under. The associations for this
    -- version only run when that Change Calendar is open. For more
    -- information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
    calendarNames :: Prelude.Maybe [Prelude.Text],
    -- | The severity level that is assigned to the association.
    complianceSeverity :: Prelude.Maybe AssociationComplianceSeverity,
    -- | The date the association version was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The version of an Amazon Web Services Systems Manager document (SSM
    -- document) used when the association version was created.
    documentVersion :: Prelude.Maybe Prelude.Text,
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
    -- | The name specified when the association was created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The location in Amazon S3 specified for the association when the
    -- association version was created.
    outputLocation :: Prelude.Maybe InstanceAssociationOutputLocation,
    -- | Parameters specified when the association version was created.
    parameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text [Prelude.Text])),
    -- | The cron or rate schedule specified for the association when the
    -- association version was created.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | Number of days to wait after the scheduled day to run an association.
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
    -- isn\'t managed by State Manager, a capability of Amazon Web Services
    -- Systems Manager. It is managed by your direct call to the
    -- PutComplianceItems API operation.
    --
    -- By default, all associations use @AUTO@ mode.
    syncCompliance :: Prelude.Maybe AssociationSyncCompliance,
    -- | The combination of Amazon Web Services Regions and Amazon Web Services
    -- accounts where you wanted to run the association when this association
    -- version was created.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | A key-value mapping of document parameters to target resources. Both
    -- Targets and TargetMaps can\'t be specified together.
    targetMaps :: Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]],
    -- | The targets specified for the association when the association version
    -- was created.
    targets :: Prelude.Maybe [Target]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyOnlyAtCronInterval', 'associationVersionInfo_applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
--
-- 'associationId', 'associationVersionInfo_associationId' - The ID created by the system when the association was created.
--
-- 'associationName', 'associationVersionInfo_associationName' - The name specified for the association version when the association
-- version was created.
--
-- 'associationVersion', 'associationVersionInfo_associationVersion' - The association version.
--
-- 'calendarNames', 'associationVersionInfo_calendarNames' - The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents your associations are gated under. The associations for this
-- version only run when that Change Calendar is open. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
--
-- 'complianceSeverity', 'associationVersionInfo_complianceSeverity' - The severity level that is assigned to the association.
--
-- 'createdDate', 'associationVersionInfo_createdDate' - The date the association version was created.
--
-- 'documentVersion', 'associationVersionInfo_documentVersion' - The version of an Amazon Web Services Systems Manager document (SSM
-- document) used when the association version was created.
--
-- 'maxConcurrency', 'associationVersionInfo_maxConcurrency' - The maximum number of targets allowed to run the association at the same
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
-- 'maxErrors', 'associationVersionInfo_maxErrors' - The number of errors that are allowed before the system stops sending
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
-- 'name', 'associationVersionInfo_name' - The name specified when the association was created.
--
-- 'outputLocation', 'associationVersionInfo_outputLocation' - The location in Amazon S3 specified for the association when the
-- association version was created.
--
-- 'parameters', 'associationVersionInfo_parameters' - Parameters specified when the association version was created.
--
-- 'scheduleExpression', 'associationVersionInfo_scheduleExpression' - The cron or rate schedule specified for the association when the
-- association version was created.
--
-- 'scheduleOffset', 'associationVersionInfo_scheduleOffset' - Number of days to wait after the scheduled day to run an association.
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
-- 'targetLocations', 'associationVersionInfo_targetLocations' - The combination of Amazon Web Services Regions and Amazon Web Services
-- accounts where you wanted to run the association when this association
-- version was created.
--
-- 'targetMaps', 'associationVersionInfo_targetMaps' - A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
--
-- 'targets', 'associationVersionInfo_targets' - The targets specified for the association when the association version
-- was created.
newAssociationVersionInfo ::
  AssociationVersionInfo
newAssociationVersionInfo =
  AssociationVersionInfo'
    { applyOnlyAtCronInterval =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      associationName = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      calendarNames = Prelude.Nothing,
      complianceSeverity = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      name = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      parameters = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      syncCompliance = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      targetMaps = Prelude.Nothing,
      targets = Prelude.Nothing
    }

-- | By default, when you create a new associations, the system runs it
-- immediately after it is created and then according to the schedule you
-- specified. Specify this option if you don\'t want an association to run
-- immediately after you create it. This parameter isn\'t supported for
-- rate expressions.
associationVersionInfo_applyOnlyAtCronInterval :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Bool)
associationVersionInfo_applyOnlyAtCronInterval = Lens.lens (\AssociationVersionInfo' {applyOnlyAtCronInterval} -> applyOnlyAtCronInterval) (\s@AssociationVersionInfo' {} a -> s {applyOnlyAtCronInterval = a} :: AssociationVersionInfo)

-- | The ID created by the system when the association was created.
associationVersionInfo_associationId :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_associationId = Lens.lens (\AssociationVersionInfo' {associationId} -> associationId) (\s@AssociationVersionInfo' {} a -> s {associationId = a} :: AssociationVersionInfo)

-- | The name specified for the association version when the association
-- version was created.
associationVersionInfo_associationName :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_associationName = Lens.lens (\AssociationVersionInfo' {associationName} -> associationName) (\s@AssociationVersionInfo' {} a -> s {associationName = a} :: AssociationVersionInfo)

-- | The association version.
associationVersionInfo_associationVersion :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_associationVersion = Lens.lens (\AssociationVersionInfo' {associationVersion} -> associationVersion) (\s@AssociationVersionInfo' {} a -> s {associationVersion = a} :: AssociationVersionInfo)

-- | The names or Amazon Resource Names (ARNs) of the Change Calendar type
-- documents your associations are gated under. The associations for this
-- version only run when that Change Calendar is open. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar Amazon Web Services Systems Manager Change Calendar>.
associationVersionInfo_calendarNames :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe [Prelude.Text])
associationVersionInfo_calendarNames = Lens.lens (\AssociationVersionInfo' {calendarNames} -> calendarNames) (\s@AssociationVersionInfo' {} a -> s {calendarNames = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The severity level that is assigned to the association.
associationVersionInfo_complianceSeverity :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe AssociationComplianceSeverity)
associationVersionInfo_complianceSeverity = Lens.lens (\AssociationVersionInfo' {complianceSeverity} -> complianceSeverity) (\s@AssociationVersionInfo' {} a -> s {complianceSeverity = a} :: AssociationVersionInfo)

-- | The date the association version was created.
associationVersionInfo_createdDate :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.UTCTime)
associationVersionInfo_createdDate = Lens.lens (\AssociationVersionInfo' {createdDate} -> createdDate) (\s@AssociationVersionInfo' {} a -> s {createdDate = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Data._Time

-- | The version of an Amazon Web Services Systems Manager document (SSM
-- document) used when the association version was created.
associationVersionInfo_documentVersion :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_documentVersion = Lens.lens (\AssociationVersionInfo' {documentVersion} -> documentVersion) (\s@AssociationVersionInfo' {} a -> s {documentVersion = a} :: AssociationVersionInfo)

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
associationVersionInfo_maxConcurrency :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_maxConcurrency = Lens.lens (\AssociationVersionInfo' {maxConcurrency} -> maxConcurrency) (\s@AssociationVersionInfo' {} a -> s {maxConcurrency = a} :: AssociationVersionInfo)

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
associationVersionInfo_maxErrors :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_maxErrors = Lens.lens (\AssociationVersionInfo' {maxErrors} -> maxErrors) (\s@AssociationVersionInfo' {} a -> s {maxErrors = a} :: AssociationVersionInfo)

-- | The name specified when the association was created.
associationVersionInfo_name :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_name = Lens.lens (\AssociationVersionInfo' {name} -> name) (\s@AssociationVersionInfo' {} a -> s {name = a} :: AssociationVersionInfo)

-- | The location in Amazon S3 specified for the association when the
-- association version was created.
associationVersionInfo_outputLocation :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe InstanceAssociationOutputLocation)
associationVersionInfo_outputLocation = Lens.lens (\AssociationVersionInfo' {outputLocation} -> outputLocation) (\s@AssociationVersionInfo' {} a -> s {outputLocation = a} :: AssociationVersionInfo)

-- | Parameters specified when the association version was created.
associationVersionInfo_parameters :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
associationVersionInfo_parameters = Lens.lens (\AssociationVersionInfo' {parameters} -> parameters) (\s@AssociationVersionInfo' {} a -> s {parameters = a} :: AssociationVersionInfo) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The cron or rate schedule specified for the association when the
-- association version was created.
associationVersionInfo_scheduleExpression :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Text)
associationVersionInfo_scheduleExpression = Lens.lens (\AssociationVersionInfo' {scheduleExpression} -> scheduleExpression) (\s@AssociationVersionInfo' {} a -> s {scheduleExpression = a} :: AssociationVersionInfo)

-- | Number of days to wait after the scheduled day to run an association.
associationVersionInfo_scheduleOffset :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe Prelude.Natural)
associationVersionInfo_scheduleOffset = Lens.lens (\AssociationVersionInfo' {scheduleOffset} -> scheduleOffset) (\s@AssociationVersionInfo' {} a -> s {scheduleOffset = a} :: AssociationVersionInfo)

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

-- | The combination of Amazon Web Services Regions and Amazon Web Services
-- accounts where you wanted to run the association when this association
-- version was created.
associationVersionInfo_targetLocations :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
associationVersionInfo_targetLocations = Lens.lens (\AssociationVersionInfo' {targetLocations} -> targetLocations) (\s@AssociationVersionInfo' {} a -> s {targetLocations = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens.coerced

-- | A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
associationVersionInfo_targetMaps :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]])
associationVersionInfo_targetMaps = Lens.lens (\AssociationVersionInfo' {targetMaps} -> targetMaps) (\s@AssociationVersionInfo' {} a -> s {targetMaps = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The targets specified for the association when the association version
-- was created.
associationVersionInfo_targets :: Lens.Lens' AssociationVersionInfo (Prelude.Maybe [Target])
associationVersionInfo_targets = Lens.lens (\AssociationVersionInfo' {targets} -> targets) (\s@AssociationVersionInfo' {} a -> s {targets = a} :: AssociationVersionInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AssociationVersionInfo where
  parseJSON =
    Data.withObject
      "AssociationVersionInfo"
      ( \x ->
          AssociationVersionInfo'
            Prelude.<$> (x Data..:? "ApplyOnlyAtCronInterval")
            Prelude.<*> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "AssociationName")
            Prelude.<*> (x Data..:? "AssociationVersion")
            Prelude.<*> (x Data..:? "CalendarNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ComplianceSeverity")
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "MaxConcurrency")
            Prelude.<*> (x Data..:? "MaxErrors")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutputLocation")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ScheduleExpression")
            Prelude.<*> (x Data..:? "ScheduleOffset")
            Prelude.<*> (x Data..:? "SyncCompliance")
            Prelude.<*> (x Data..:? "TargetLocations")
            Prelude.<*> (x Data..:? "TargetMaps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AssociationVersionInfo where
  hashWithSalt _salt AssociationVersionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` applyOnlyAtCronInterval
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` associationName
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` calendarNames
      `Prelude.hashWithSalt` complianceSeverity
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` syncCompliance
      `Prelude.hashWithSalt` targetLocations
      `Prelude.hashWithSalt` targetMaps
      `Prelude.hashWithSalt` targets

instance Prelude.NFData AssociationVersionInfo where
  rnf AssociationVersionInfo' {..} =
    Prelude.rnf applyOnlyAtCronInterval `Prelude.seq`
      Prelude.rnf associationId `Prelude.seq`
        Prelude.rnf associationName `Prelude.seq`
          Prelude.rnf associationVersion `Prelude.seq`
            Prelude.rnf calendarNames `Prelude.seq`
              Prelude.rnf complianceSeverity `Prelude.seq`
                Prelude.rnf createdDate `Prelude.seq`
                  Prelude.rnf documentVersion `Prelude.seq`
                    Prelude.rnf maxConcurrency `Prelude.seq`
                      Prelude.rnf maxErrors `Prelude.seq`
                        Prelude.rnf name `Prelude.seq`
                          Prelude.rnf outputLocation `Prelude.seq`
                            Prelude.rnf parameters `Prelude.seq`
                              Prelude.rnf scheduleExpression `Prelude.seq`
                                Prelude.rnf scheduleOffset `Prelude.seq`
                                  Prelude.rnf syncCompliance `Prelude.seq`
                                    Prelude.rnf targetLocations `Prelude.seq`
                                      Prelude.rnf targetMaps `Prelude.seq`
                                        Prelude.rnf targets
