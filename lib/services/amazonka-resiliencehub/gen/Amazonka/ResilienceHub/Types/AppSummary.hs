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
-- Module      : Amazonka.ResilienceHub.Types.AppSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AppSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.AppAssessmentScheduleType
import Amazonka.ResilienceHub.Types.AppComplianceStatusType
import Amazonka.ResilienceHub.Types.AppStatusType

-- | Defines an application summary.
--
-- /See:/ 'newAppSummary' smart constructor.
data AppSummary = AppSummary'
  { -- | The current resiliency score for the application.
    resiliencyScore :: Prelude.Maybe Prelude.Double,
    -- | The current status of compliance for the resiliency policy.
    complianceStatus :: Prelude.Maybe AppComplianceStatusType,
    -- | The status of the application.
    status :: Prelude.Maybe AppStatusType,
    -- | The optional description for an app.
    description :: Prelude.Maybe Prelude.Text,
    -- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
    assessmentSchedule :: Prelude.Maybe AppAssessmentScheduleType,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The timestamp for when the app was created.
    creationTime :: Data.POSIX,
    -- | The name of the application.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resiliencyScore', 'appSummary_resiliencyScore' - The current resiliency score for the application.
--
-- 'complianceStatus', 'appSummary_complianceStatus' - The current status of compliance for the resiliency policy.
--
-- 'status', 'appSummary_status' - The status of the application.
--
-- 'description', 'appSummary_description' - The optional description for an app.
--
-- 'assessmentSchedule', 'appSummary_assessmentSchedule' - Assessment execution schedule with \'Daily\' or \'Disabled\' values.
--
-- 'appArn', 'appSummary_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'creationTime', 'appSummary_creationTime' - The timestamp for when the app was created.
--
-- 'name', 'appSummary_name' - The name of the application.
newAppSummary ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  AppSummary
newAppSummary pAppArn_ pCreationTime_ pName_ =
  AppSummary'
    { resiliencyScore = Prelude.Nothing,
      complianceStatus = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      assessmentSchedule = Prelude.Nothing,
      appArn = pAppArn_,
      creationTime = Data._Time Lens.# pCreationTime_,
      name = pName_
    }

-- | The current resiliency score for the application.
appSummary_resiliencyScore :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Double)
appSummary_resiliencyScore = Lens.lens (\AppSummary' {resiliencyScore} -> resiliencyScore) (\s@AppSummary' {} a -> s {resiliencyScore = a} :: AppSummary)

-- | The current status of compliance for the resiliency policy.
appSummary_complianceStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppComplianceStatusType)
appSummary_complianceStatus = Lens.lens (\AppSummary' {complianceStatus} -> complianceStatus) (\s@AppSummary' {} a -> s {complianceStatus = a} :: AppSummary)

-- | The status of the application.
appSummary_status :: Lens.Lens' AppSummary (Prelude.Maybe AppStatusType)
appSummary_status = Lens.lens (\AppSummary' {status} -> status) (\s@AppSummary' {} a -> s {status = a} :: AppSummary)

-- | The optional description for an app.
appSummary_description :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_description = Lens.lens (\AppSummary' {description} -> description) (\s@AppSummary' {} a -> s {description = a} :: AppSummary)

-- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
appSummary_assessmentSchedule :: Lens.Lens' AppSummary (Prelude.Maybe AppAssessmentScheduleType)
appSummary_assessmentSchedule = Lens.lens (\AppSummary' {assessmentSchedule} -> assessmentSchedule) (\s@AppSummary' {} a -> s {assessmentSchedule = a} :: AppSummary)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
appSummary_appArn :: Lens.Lens' AppSummary Prelude.Text
appSummary_appArn = Lens.lens (\AppSummary' {appArn} -> appArn) (\s@AppSummary' {} a -> s {appArn = a} :: AppSummary)

-- | The timestamp for when the app was created.
appSummary_creationTime :: Lens.Lens' AppSummary Prelude.UTCTime
appSummary_creationTime = Lens.lens (\AppSummary' {creationTime} -> creationTime) (\s@AppSummary' {} a -> s {creationTime = a} :: AppSummary) Prelude.. Data._Time

-- | The name of the application.
appSummary_name :: Lens.Lens' AppSummary Prelude.Text
appSummary_name = Lens.lens (\AppSummary' {name} -> name) (\s@AppSummary' {} a -> s {name = a} :: AppSummary)

instance Data.FromJSON AppSummary where
  parseJSON =
    Data.withObject
      "AppSummary"
      ( \x ->
          AppSummary'
            Prelude.<$> (x Data..:? "resiliencyScore")
            Prelude.<*> (x Data..:? "complianceStatus")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "assessmentSchedule")
            Prelude.<*> (x Data..: "appArn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable AppSummary where
  hashWithSalt _salt AppSummary' {..} =
    _salt `Prelude.hashWithSalt` resiliencyScore
      `Prelude.hashWithSalt` complianceStatus
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` assessmentSchedule
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData AppSummary where
  rnf AppSummary' {..} =
    Prelude.rnf resiliencyScore
      `Prelude.seq` Prelude.rnf complianceStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf assessmentSchedule
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf name
