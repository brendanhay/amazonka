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
-- Module      : Amazonka.Connect.Types.EvaluationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationSummary where

import Amazonka.Connect.Types.EvaluationScore
import Amazonka.Connect.Types.EvaluationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a contact evaluation.
--
-- /See:/ 'newEvaluationSummary' smart constructor.
data EvaluationSummary = EvaluationSummary'
  { -- | The overall score of the contact evaluation.
    score :: Prelude.Maybe EvaluationScore,
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contact evaluation resource.
    evaluationArn :: Prelude.Text,
    -- | A title of the evaluation form.
    evaluationFormTitle :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The status of the contact evaluation.
    status :: EvaluationStatus,
    -- | The Amazon Resource Name (ARN) of the user who last updated the
    -- evaluation.
    evaluatorArn :: Prelude.Text,
    -- | The timestamp for when the evaluation was created.
    createdTime :: Data.POSIX,
    -- | The timestamp for when the evaluation was last updated.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'evaluationSummary_score' - The overall score of the contact evaluation.
--
-- 'evaluationId', 'evaluationSummary_evaluationId' - A unique identifier for the contact evaluation.
--
-- 'evaluationArn', 'evaluationSummary_evaluationArn' - The Amazon Resource Name (ARN) for the contact evaluation resource.
--
-- 'evaluationFormTitle', 'evaluationSummary_evaluationFormTitle' - A title of the evaluation form.
--
-- 'evaluationFormId', 'evaluationSummary_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'status', 'evaluationSummary_status' - The status of the contact evaluation.
--
-- 'evaluatorArn', 'evaluationSummary_evaluatorArn' - The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation.
--
-- 'createdTime', 'evaluationSummary_createdTime' - The timestamp for when the evaluation was created.
--
-- 'lastModifiedTime', 'evaluationSummary_lastModifiedTime' - The timestamp for when the evaluation was last updated.
newEvaluationSummary ::
  -- | 'evaluationId'
  Prelude.Text ->
  -- | 'evaluationArn'
  Prelude.Text ->
  -- | 'evaluationFormTitle'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'status'
  EvaluationStatus ->
  -- | 'evaluatorArn'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  EvaluationSummary
newEvaluationSummary
  pEvaluationId_
  pEvaluationArn_
  pEvaluationFormTitle_
  pEvaluationFormId_
  pStatus_
  pEvaluatorArn_
  pCreatedTime_
  pLastModifiedTime_ =
    EvaluationSummary'
      { score = Prelude.Nothing,
        evaluationId = pEvaluationId_,
        evaluationArn = pEvaluationArn_,
        evaluationFormTitle = pEvaluationFormTitle_,
        evaluationFormId = pEvaluationFormId_,
        status = pStatus_,
        evaluatorArn = pEvaluatorArn_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The overall score of the contact evaluation.
evaluationSummary_score :: Lens.Lens' EvaluationSummary (Prelude.Maybe EvaluationScore)
evaluationSummary_score = Lens.lens (\EvaluationSummary' {score} -> score) (\s@EvaluationSummary' {} a -> s {score = a} :: EvaluationSummary)

-- | A unique identifier for the contact evaluation.
evaluationSummary_evaluationId :: Lens.Lens' EvaluationSummary Prelude.Text
evaluationSummary_evaluationId = Lens.lens (\EvaluationSummary' {evaluationId} -> evaluationId) (\s@EvaluationSummary' {} a -> s {evaluationId = a} :: EvaluationSummary)

-- | The Amazon Resource Name (ARN) for the contact evaluation resource.
evaluationSummary_evaluationArn :: Lens.Lens' EvaluationSummary Prelude.Text
evaluationSummary_evaluationArn = Lens.lens (\EvaluationSummary' {evaluationArn} -> evaluationArn) (\s@EvaluationSummary' {} a -> s {evaluationArn = a} :: EvaluationSummary)

-- | A title of the evaluation form.
evaluationSummary_evaluationFormTitle :: Lens.Lens' EvaluationSummary Prelude.Text
evaluationSummary_evaluationFormTitle = Lens.lens (\EvaluationSummary' {evaluationFormTitle} -> evaluationFormTitle) (\s@EvaluationSummary' {} a -> s {evaluationFormTitle = a} :: EvaluationSummary)

-- | The unique identifier for the evaluation form.
evaluationSummary_evaluationFormId :: Lens.Lens' EvaluationSummary Prelude.Text
evaluationSummary_evaluationFormId = Lens.lens (\EvaluationSummary' {evaluationFormId} -> evaluationFormId) (\s@EvaluationSummary' {} a -> s {evaluationFormId = a} :: EvaluationSummary)

-- | The status of the contact evaluation.
evaluationSummary_status :: Lens.Lens' EvaluationSummary EvaluationStatus
evaluationSummary_status = Lens.lens (\EvaluationSummary' {status} -> status) (\s@EvaluationSummary' {} a -> s {status = a} :: EvaluationSummary)

-- | The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation.
evaluationSummary_evaluatorArn :: Lens.Lens' EvaluationSummary Prelude.Text
evaluationSummary_evaluatorArn = Lens.lens (\EvaluationSummary' {evaluatorArn} -> evaluatorArn) (\s@EvaluationSummary' {} a -> s {evaluatorArn = a} :: EvaluationSummary)

-- | The timestamp for when the evaluation was created.
evaluationSummary_createdTime :: Lens.Lens' EvaluationSummary Prelude.UTCTime
evaluationSummary_createdTime = Lens.lens (\EvaluationSummary' {createdTime} -> createdTime) (\s@EvaluationSummary' {} a -> s {createdTime = a} :: EvaluationSummary) Prelude.. Data._Time

-- | The timestamp for when the evaluation was last updated.
evaluationSummary_lastModifiedTime :: Lens.Lens' EvaluationSummary Prelude.UTCTime
evaluationSummary_lastModifiedTime = Lens.lens (\EvaluationSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EvaluationSummary' {} a -> s {lastModifiedTime = a} :: EvaluationSummary) Prelude.. Data._Time

instance Data.FromJSON EvaluationSummary where
  parseJSON =
    Data.withObject
      "EvaluationSummary"
      ( \x ->
          EvaluationSummary'
            Prelude.<$> (x Data..:? "Score")
            Prelude.<*> (x Data..: "EvaluationId")
            Prelude.<*> (x Data..: "EvaluationArn")
            Prelude.<*> (x Data..: "EvaluationFormTitle")
            Prelude.<*> (x Data..: "EvaluationFormId")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "EvaluatorArn")
            Prelude.<*> (x Data..: "CreatedTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable EvaluationSummary where
  hashWithSalt _salt EvaluationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` evaluationId
      `Prelude.hashWithSalt` evaluationArn
      `Prelude.hashWithSalt` evaluationFormTitle
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` evaluatorArn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData EvaluationSummary where
  rnf EvaluationSummary' {..} =
    Prelude.rnf score
      `Prelude.seq` Prelude.rnf evaluationId
      `Prelude.seq` Prelude.rnf evaluationArn
      `Prelude.seq` Prelude.rnf evaluationFormTitle
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf evaluatorArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
