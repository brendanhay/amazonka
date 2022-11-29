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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.App where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.AppAssessmentScheduleType
import Amazonka.ResilienceHub.Types.AppComplianceStatusType
import Amazonka.ResilienceHub.Types.AppStatusType

-- | Defines a Resilience Hub application.
--
-- /See:/ 'newApp' smart constructor.
data App = App'
  { -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The current resiliency score for the application.
    resiliencyScore :: Prelude.Maybe Prelude.Double,
    -- | The current status of compliance for the resiliency policy.
    complianceStatus :: Prelude.Maybe AppComplianceStatusType,
    -- | The status of the application.
    status :: Prelude.Maybe AppStatusType,
    -- | The optional description for an app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for the most recent compliance evaluation.
    lastAppComplianceEvaluationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
    assessmentSchedule :: Prelude.Maybe AppAssessmentScheduleType,
    -- | The timestamp for the most recent resiliency score evaluation.
    lastResiliencyScoreEvaluationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The timestamp for when the app was created.
    creationTime :: Core.POSIX,
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
-- 'tags', 'app_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'resiliencyScore', 'app_resiliencyScore' - The current resiliency score for the application.
--
-- 'complianceStatus', 'app_complianceStatus' - The current status of compliance for the resiliency policy.
--
-- 'status', 'app_status' - The status of the application.
--
-- 'description', 'app_description' - The optional description for an app.
--
-- 'lastAppComplianceEvaluationTime', 'app_lastAppComplianceEvaluationTime' - The timestamp for the most recent compliance evaluation.
--
-- 'policyArn', 'app_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'assessmentSchedule', 'app_assessmentSchedule' - Assessment execution schedule with \'Daily\' or \'Disabled\' values.
--
-- 'lastResiliencyScoreEvaluationTime', 'app_lastResiliencyScoreEvaluationTime' - The timestamp for the most recent resiliency score evaluation.
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
    { tags = Prelude.Nothing,
      resiliencyScore = Prelude.Nothing,
      complianceStatus = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      lastAppComplianceEvaluationTime = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      assessmentSchedule = Prelude.Nothing,
      lastResiliencyScoreEvaluationTime = Prelude.Nothing,
      appArn = pAppArn_,
      creationTime = Core._Time Lens.# pCreationTime_,
      name = pName_
    }

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
app_tags :: Lens.Lens' App (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
app_tags = Lens.lens (\App' {tags} -> tags) (\s@App' {} a -> s {tags = a} :: App) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The current resiliency score for the application.
app_resiliencyScore :: Lens.Lens' App (Prelude.Maybe Prelude.Double)
app_resiliencyScore = Lens.lens (\App' {resiliencyScore} -> resiliencyScore) (\s@App' {} a -> s {resiliencyScore = a} :: App)

-- | The current status of compliance for the resiliency policy.
app_complianceStatus :: Lens.Lens' App (Prelude.Maybe AppComplianceStatusType)
app_complianceStatus = Lens.lens (\App' {complianceStatus} -> complianceStatus) (\s@App' {} a -> s {complianceStatus = a} :: App)

-- | The status of the application.
app_status :: Lens.Lens' App (Prelude.Maybe AppStatusType)
app_status = Lens.lens (\App' {status} -> status) (\s@App' {} a -> s {status = a} :: App)

-- | The optional description for an app.
app_description :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_description = Lens.lens (\App' {description} -> description) (\s@App' {} a -> s {description = a} :: App)

-- | The timestamp for the most recent compliance evaluation.
app_lastAppComplianceEvaluationTime :: Lens.Lens' App (Prelude.Maybe Prelude.UTCTime)
app_lastAppComplianceEvaluationTime = Lens.lens (\App' {lastAppComplianceEvaluationTime} -> lastAppComplianceEvaluationTime) (\s@App' {} a -> s {lastAppComplianceEvaluationTime = a} :: App) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
app_policyArn :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_policyArn = Lens.lens (\App' {policyArn} -> policyArn) (\s@App' {} a -> s {policyArn = a} :: App)

-- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
app_assessmentSchedule :: Lens.Lens' App (Prelude.Maybe AppAssessmentScheduleType)
app_assessmentSchedule = Lens.lens (\App' {assessmentSchedule} -> assessmentSchedule) (\s@App' {} a -> s {assessmentSchedule = a} :: App)

-- | The timestamp for the most recent resiliency score evaluation.
app_lastResiliencyScoreEvaluationTime :: Lens.Lens' App (Prelude.Maybe Prelude.UTCTime)
app_lastResiliencyScoreEvaluationTime = Lens.lens (\App' {lastResiliencyScoreEvaluationTime} -> lastResiliencyScoreEvaluationTime) (\s@App' {} a -> s {lastResiliencyScoreEvaluationTime = a} :: App) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
app_appArn :: Lens.Lens' App Prelude.Text
app_appArn = Lens.lens (\App' {appArn} -> appArn) (\s@App' {} a -> s {appArn = a} :: App)

-- | The timestamp for when the app was created.
app_creationTime :: Lens.Lens' App Prelude.UTCTime
app_creationTime = Lens.lens (\App' {creationTime} -> creationTime) (\s@App' {} a -> s {creationTime = a} :: App) Prelude.. Core._Time

-- | The name for the application.
app_name :: Lens.Lens' App Prelude.Text
app_name = Lens.lens (\App' {name} -> name) (\s@App' {} a -> s {name = a} :: App)

instance Core.FromJSON App where
  parseJSON =
    Core.withObject
      "App"
      ( \x ->
          App'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resiliencyScore")
            Prelude.<*> (x Core..:? "complianceStatus")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "lastAppComplianceEvaluationTime")
            Prelude.<*> (x Core..:? "policyArn")
            Prelude.<*> (x Core..:? "assessmentSchedule")
            Prelude.<*> (x Core..:? "lastResiliencyScoreEvaluationTime")
            Prelude.<*> (x Core..: "appArn")
            Prelude.<*> (x Core..: "creationTime")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable App where
  hashWithSalt _salt App' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resiliencyScore
      `Prelude.hashWithSalt` complianceStatus
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastAppComplianceEvaluationTime
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` assessmentSchedule
      `Prelude.hashWithSalt` lastResiliencyScoreEvaluationTime
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData App where
  rnf App' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resiliencyScore
      `Prelude.seq` Prelude.rnf complianceStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastAppComplianceEvaluationTime
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf assessmentSchedule
      `Prelude.seq` Prelude.rnf lastResiliencyScoreEvaluationTime
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf name
