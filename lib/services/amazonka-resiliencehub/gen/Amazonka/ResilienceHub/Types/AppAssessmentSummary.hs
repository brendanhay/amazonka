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
-- Module      : Amazonka.ResilienceHub.Types.AppAssessmentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AppAssessmentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.AssessmentInvoker
import Amazonka.ResilienceHub.Types.AssessmentStatus
import Amazonka.ResilienceHub.Types.ComplianceStatus
import Amazonka.ResilienceHub.Types.Cost

-- | Defines an application assessment summary.
--
-- /See:/ 'newAppAssessmentSummary' smart constructor.
data AppAssessmentSummary = AppAssessmentSummary'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the assessment.
    assessmentName :: Prelude.Maybe Prelude.Text,
    -- | The current status of compliance for the resiliency policy.
    complianceStatus :: Prelude.Maybe ComplianceStatus,
    -- | The cost for the application.
    cost :: Prelude.Maybe Cost,
    -- | The end time for the action.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The entity that invoked the assessment.
    invoker :: Prelude.Maybe AssessmentInvoker,
    -- | The message from the assessment run.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current resiliency score for the application.
    resiliencyScore :: Prelude.Maybe Prelude.Double,
    -- | The starting time for the action.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    assessmentArn :: Prelude.Text,
    -- | The current status of the assessment for the resiliency policy.
    assessmentStatus :: AssessmentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppAssessmentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'appAssessmentSummary_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'appAssessmentSummary_appVersion' - The version of the application.
--
-- 'assessmentName', 'appAssessmentSummary_assessmentName' - The name of the assessment.
--
-- 'complianceStatus', 'appAssessmentSummary_complianceStatus' - The current status of compliance for the resiliency policy.
--
-- 'cost', 'appAssessmentSummary_cost' - The cost for the application.
--
-- 'endTime', 'appAssessmentSummary_endTime' - The end time for the action.
--
-- 'invoker', 'appAssessmentSummary_invoker' - The entity that invoked the assessment.
--
-- 'message', 'appAssessmentSummary_message' - The message from the assessment run.
--
-- 'resiliencyScore', 'appAssessmentSummary_resiliencyScore' - The current resiliency score for the application.
--
-- 'startTime', 'appAssessmentSummary_startTime' - The starting time for the action.
--
-- 'assessmentArn', 'appAssessmentSummary_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'assessmentStatus', 'appAssessmentSummary_assessmentStatus' - The current status of the assessment for the resiliency policy.
newAppAssessmentSummary ::
  -- | 'assessmentArn'
  Prelude.Text ->
  -- | 'assessmentStatus'
  AssessmentStatus ->
  AppAssessmentSummary
newAppAssessmentSummary
  pAssessmentArn_
  pAssessmentStatus_ =
    AppAssessmentSummary'
      { appArn = Prelude.Nothing,
        appVersion = Prelude.Nothing,
        assessmentName = Prelude.Nothing,
        complianceStatus = Prelude.Nothing,
        cost = Prelude.Nothing,
        endTime = Prelude.Nothing,
        invoker = Prelude.Nothing,
        message = Prelude.Nothing,
        resiliencyScore = Prelude.Nothing,
        startTime = Prelude.Nothing,
        assessmentArn = pAssessmentArn_,
        assessmentStatus = pAssessmentStatus_
      }

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
appAssessmentSummary_appArn :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Prelude.Text)
appAssessmentSummary_appArn = Lens.lens (\AppAssessmentSummary' {appArn} -> appArn) (\s@AppAssessmentSummary' {} a -> s {appArn = a} :: AppAssessmentSummary)

-- | The version of the application.
appAssessmentSummary_appVersion :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Prelude.Text)
appAssessmentSummary_appVersion = Lens.lens (\AppAssessmentSummary' {appVersion} -> appVersion) (\s@AppAssessmentSummary' {} a -> s {appVersion = a} :: AppAssessmentSummary)

-- | The name of the assessment.
appAssessmentSummary_assessmentName :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Prelude.Text)
appAssessmentSummary_assessmentName = Lens.lens (\AppAssessmentSummary' {assessmentName} -> assessmentName) (\s@AppAssessmentSummary' {} a -> s {assessmentName = a} :: AppAssessmentSummary)

-- | The current status of compliance for the resiliency policy.
appAssessmentSummary_complianceStatus :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe ComplianceStatus)
appAssessmentSummary_complianceStatus = Lens.lens (\AppAssessmentSummary' {complianceStatus} -> complianceStatus) (\s@AppAssessmentSummary' {} a -> s {complianceStatus = a} :: AppAssessmentSummary)

-- | The cost for the application.
appAssessmentSummary_cost :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Cost)
appAssessmentSummary_cost = Lens.lens (\AppAssessmentSummary' {cost} -> cost) (\s@AppAssessmentSummary' {} a -> s {cost = a} :: AppAssessmentSummary)

-- | The end time for the action.
appAssessmentSummary_endTime :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Prelude.UTCTime)
appAssessmentSummary_endTime = Lens.lens (\AppAssessmentSummary' {endTime} -> endTime) (\s@AppAssessmentSummary' {} a -> s {endTime = a} :: AppAssessmentSummary) Prelude.. Lens.mapping Data._Time

-- | The entity that invoked the assessment.
appAssessmentSummary_invoker :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe AssessmentInvoker)
appAssessmentSummary_invoker = Lens.lens (\AppAssessmentSummary' {invoker} -> invoker) (\s@AppAssessmentSummary' {} a -> s {invoker = a} :: AppAssessmentSummary)

-- | The message from the assessment run.
appAssessmentSummary_message :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Prelude.Text)
appAssessmentSummary_message = Lens.lens (\AppAssessmentSummary' {message} -> message) (\s@AppAssessmentSummary' {} a -> s {message = a} :: AppAssessmentSummary)

-- | The current resiliency score for the application.
appAssessmentSummary_resiliencyScore :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Prelude.Double)
appAssessmentSummary_resiliencyScore = Lens.lens (\AppAssessmentSummary' {resiliencyScore} -> resiliencyScore) (\s@AppAssessmentSummary' {} a -> s {resiliencyScore = a} :: AppAssessmentSummary)

-- | The starting time for the action.
appAssessmentSummary_startTime :: Lens.Lens' AppAssessmentSummary (Prelude.Maybe Prelude.UTCTime)
appAssessmentSummary_startTime = Lens.lens (\AppAssessmentSummary' {startTime} -> startTime) (\s@AppAssessmentSummary' {} a -> s {startTime = a} :: AppAssessmentSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
appAssessmentSummary_assessmentArn :: Lens.Lens' AppAssessmentSummary Prelude.Text
appAssessmentSummary_assessmentArn = Lens.lens (\AppAssessmentSummary' {assessmentArn} -> assessmentArn) (\s@AppAssessmentSummary' {} a -> s {assessmentArn = a} :: AppAssessmentSummary)

-- | The current status of the assessment for the resiliency policy.
appAssessmentSummary_assessmentStatus :: Lens.Lens' AppAssessmentSummary AssessmentStatus
appAssessmentSummary_assessmentStatus = Lens.lens (\AppAssessmentSummary' {assessmentStatus} -> assessmentStatus) (\s@AppAssessmentSummary' {} a -> s {assessmentStatus = a} :: AppAssessmentSummary)

instance Data.FromJSON AppAssessmentSummary where
  parseJSON =
    Data.withObject
      "AppAssessmentSummary"
      ( \x ->
          AppAssessmentSummary'
            Prelude.<$> (x Data..:? "appArn")
            Prelude.<*> (x Data..:? "appVersion")
            Prelude.<*> (x Data..:? "assessmentName")
            Prelude.<*> (x Data..:? "complianceStatus")
            Prelude.<*> (x Data..:? "cost")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "invoker")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "resiliencyScore")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..: "assessmentArn")
            Prelude.<*> (x Data..: "assessmentStatus")
      )

instance Prelude.Hashable AppAssessmentSummary where
  hashWithSalt _salt AppAssessmentSummary' {..} =
    _salt `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion
      `Prelude.hashWithSalt` assessmentName
      `Prelude.hashWithSalt` complianceStatus
      `Prelude.hashWithSalt` cost
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` invoker
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` resiliencyScore
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` assessmentArn
      `Prelude.hashWithSalt` assessmentStatus

instance Prelude.NFData AppAssessmentSummary where
  rnf AppAssessmentSummary' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
      `Prelude.seq` Prelude.rnf assessmentName
      `Prelude.seq` Prelude.rnf complianceStatus
      `Prelude.seq` Prelude.rnf cost
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf invoker
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf resiliencyScore
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf assessmentArn
      `Prelude.seq` Prelude.rnf assessmentStatus
