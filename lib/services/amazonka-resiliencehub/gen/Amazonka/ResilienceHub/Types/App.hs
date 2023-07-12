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
-- Module      : Amazonka.ResilienceHub.Types.App
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.App where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.AppAssessmentScheduleType
import Amazonka.ResilienceHub.Types.AppComplianceStatusType
import Amazonka.ResilienceHub.Types.AppStatusType

-- | Defines a Resilience Hub application.
--
-- /See:/ 'newApp' smart constructor.
data App = App'
  { -- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
    assessmentSchedule :: Prelude.Maybe AppAssessmentScheduleType,
    -- | The current status of compliance for the resiliency policy.
    complianceStatus :: Prelude.Maybe AppComplianceStatusType,
    -- | The optional description for an app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for the most recent compliance evaluation.
    lastAppComplianceEvaluationTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp for the most recent resiliency score evaluation.
    lastResiliencyScoreEvaluationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The current resiliency score for the application.
    resiliencyScore :: Prelude.Maybe Prelude.Double,
    -- | The status of the application.
    status :: Prelude.Maybe AppStatusType,
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The timestamp for when the app was created.
    creationTime :: Data.POSIX,
    -- | The name for the application.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'App' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentSchedule', 'app_assessmentSchedule' - Assessment execution schedule with \'Daily\' or \'Disabled\' values.
--
-- 'complianceStatus', 'app_complianceStatus' - The current status of compliance for the resiliency policy.
--
-- 'description', 'app_description' - The optional description for an app.
--
-- 'lastAppComplianceEvaluationTime', 'app_lastAppComplianceEvaluationTime' - The timestamp for the most recent compliance evaluation.
--
-- 'lastResiliencyScoreEvaluationTime', 'app_lastResiliencyScoreEvaluationTime' - The timestamp for the most recent resiliency score evaluation.
--
-- 'policyArn', 'app_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'resiliencyScore', 'app_resiliencyScore' - The current resiliency score for the application.
--
-- 'status', 'app_status' - The status of the application.
--
-- 'tags', 'app_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'appArn', 'app_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'creationTime', 'app_creationTime' - The timestamp for when the app was created.
--
-- 'name', 'app_name' - The name for the application.
newApp ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  App
newApp pAppArn_ pCreationTime_ pName_ =
  App'
    { assessmentSchedule = Prelude.Nothing,
      complianceStatus = Prelude.Nothing,
      description = Prelude.Nothing,
      lastAppComplianceEvaluationTime = Prelude.Nothing,
      lastResiliencyScoreEvaluationTime = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      resiliencyScore = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      appArn = pAppArn_,
      creationTime = Data._Time Lens.# pCreationTime_,
      name = pName_
    }

-- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
app_assessmentSchedule :: Lens.Lens' App (Prelude.Maybe AppAssessmentScheduleType)
app_assessmentSchedule = Lens.lens (\App' {assessmentSchedule} -> assessmentSchedule) (\s@App' {} a -> s {assessmentSchedule = a} :: App)

-- | The current status of compliance for the resiliency policy.
app_complianceStatus :: Lens.Lens' App (Prelude.Maybe AppComplianceStatusType)
app_complianceStatus = Lens.lens (\App' {complianceStatus} -> complianceStatus) (\s@App' {} a -> s {complianceStatus = a} :: App)

-- | The optional description for an app.
app_description :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_description = Lens.lens (\App' {description} -> description) (\s@App' {} a -> s {description = a} :: App)

-- | The timestamp for the most recent compliance evaluation.
app_lastAppComplianceEvaluationTime :: Lens.Lens' App (Prelude.Maybe Prelude.UTCTime)
app_lastAppComplianceEvaluationTime = Lens.lens (\App' {lastAppComplianceEvaluationTime} -> lastAppComplianceEvaluationTime) (\s@App' {} a -> s {lastAppComplianceEvaluationTime = a} :: App) Prelude.. Lens.mapping Data._Time

-- | The timestamp for the most recent resiliency score evaluation.
app_lastResiliencyScoreEvaluationTime :: Lens.Lens' App (Prelude.Maybe Prelude.UTCTime)
app_lastResiliencyScoreEvaluationTime = Lens.lens (\App' {lastResiliencyScoreEvaluationTime} -> lastResiliencyScoreEvaluationTime) (\s@App' {} a -> s {lastResiliencyScoreEvaluationTime = a} :: App) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
app_policyArn :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_policyArn = Lens.lens (\App' {policyArn} -> policyArn) (\s@App' {} a -> s {policyArn = a} :: App)

-- | The current resiliency score for the application.
app_resiliencyScore :: Lens.Lens' App (Prelude.Maybe Prelude.Double)
app_resiliencyScore = Lens.lens (\App' {resiliencyScore} -> resiliencyScore) (\s@App' {} a -> s {resiliencyScore = a} :: App)

-- | The status of the application.
app_status :: Lens.Lens' App (Prelude.Maybe AppStatusType)
app_status = Lens.lens (\App' {status} -> status) (\s@App' {} a -> s {status = a} :: App)

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
app_tags :: Lens.Lens' App (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
app_tags = Lens.lens (\App' {tags} -> tags) (\s@App' {} a -> s {tags = a} :: App) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
app_appArn :: Lens.Lens' App Prelude.Text
app_appArn = Lens.lens (\App' {appArn} -> appArn) (\s@App' {} a -> s {appArn = a} :: App)

-- | The timestamp for when the app was created.
app_creationTime :: Lens.Lens' App Prelude.UTCTime
app_creationTime = Lens.lens (\App' {creationTime} -> creationTime) (\s@App' {} a -> s {creationTime = a} :: App) Prelude.. Data._Time

-- | The name for the application.
app_name :: Lens.Lens' App Prelude.Text
app_name = Lens.lens (\App' {name} -> name) (\s@App' {} a -> s {name = a} :: App)

instance Data.FromJSON App where
  parseJSON =
    Data.withObject
      "App"
      ( \x ->
          App'
            Prelude.<$> (x Data..:? "assessmentSchedule")
            Prelude.<*> (x Data..:? "complianceStatus")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastAppComplianceEvaluationTime")
            Prelude.<*> (x Data..:? "lastResiliencyScoreEvaluationTime")
            Prelude.<*> (x Data..:? "policyArn")
            Prelude.<*> (x Data..:? "resiliencyScore")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "appArn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable App where
  hashWithSalt _salt App' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentSchedule
      `Prelude.hashWithSalt` complianceStatus
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastAppComplianceEvaluationTime
      `Prelude.hashWithSalt` lastResiliencyScoreEvaluationTime
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` resiliencyScore
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData App where
  rnf App' {..} =
    Prelude.rnf assessmentSchedule
      `Prelude.seq` Prelude.rnf complianceStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastAppComplianceEvaluationTime
      `Prelude.seq` Prelude.rnf lastResiliencyScoreEvaluationTime
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf resiliencyScore
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf name
