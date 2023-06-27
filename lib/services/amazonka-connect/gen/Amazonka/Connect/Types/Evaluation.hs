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
-- Module      : Amazonka.Connect.Types.Evaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Evaluation where

import Amazonka.Connect.Types.EvaluationAnswerOutput
import Amazonka.Connect.Types.EvaluationMetadata
import Amazonka.Connect.Types.EvaluationNote
import Amazonka.Connect.Types.EvaluationScore
import Amazonka.Connect.Types.EvaluationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a contact evaluation.
--
-- /See:/ 'newEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | A map of item (section or question) identifiers to score value.
    scores :: Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationScore),
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contact evaluation resource.
    evaluationArn :: Prelude.Text,
    -- | Metadata about the contact evaluation.
    metadata :: EvaluationMetadata,
    -- | A map of question identifiers to answer value.
    answers :: Prelude.HashMap Prelude.Text EvaluationAnswerOutput,
    -- | A map of question identifiers to note value.
    notes :: Prelude.HashMap Prelude.Text EvaluationNote,
    -- | The status of the contact evaluation.
    status :: EvaluationStatus,
    -- | The timestamp for when the evaluation was created.
    createdTime :: Data.POSIX,
    -- | The timestamp for when the evaluation was last updated.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Evaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scores', 'evaluation_scores' - A map of item (section or question) identifiers to score value.
--
-- 'tags', 'evaluation_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'evaluationId', 'evaluation_evaluationId' - A unique identifier for the contact evaluation.
--
-- 'evaluationArn', 'evaluation_evaluationArn' - The Amazon Resource Name (ARN) for the contact evaluation resource.
--
-- 'metadata', 'evaluation_metadata' - Metadata about the contact evaluation.
--
-- 'answers', 'evaluation_answers' - A map of question identifiers to answer value.
--
-- 'notes', 'evaluation_notes' - A map of question identifiers to note value.
--
-- 'status', 'evaluation_status' - The status of the contact evaluation.
--
-- 'createdTime', 'evaluation_createdTime' - The timestamp for when the evaluation was created.
--
-- 'lastModifiedTime', 'evaluation_lastModifiedTime' - The timestamp for when the evaluation was last updated.
newEvaluation ::
  -- | 'evaluationId'
  Prelude.Text ->
  -- | 'evaluationArn'
  Prelude.Text ->
  -- | 'metadata'
  EvaluationMetadata ->
  -- | 'status'
  EvaluationStatus ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  Evaluation
newEvaluation
  pEvaluationId_
  pEvaluationArn_
  pMetadata_
  pStatus_
  pCreatedTime_
  pLastModifiedTime_ =
    Evaluation'
      { scores = Prelude.Nothing,
        tags = Prelude.Nothing,
        evaluationId = pEvaluationId_,
        evaluationArn = pEvaluationArn_,
        metadata = pMetadata_,
        answers = Prelude.mempty,
        notes = Prelude.mempty,
        status = pStatus_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | A map of item (section or question) identifiers to score value.
evaluation_scores :: Lens.Lens' Evaluation (Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationScore))
evaluation_scores = Lens.lens (\Evaluation' {scores} -> scores) (\s@Evaluation' {} a -> s {scores = a} :: Evaluation) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
evaluation_tags :: Lens.Lens' Evaluation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evaluation_tags = Lens.lens (\Evaluation' {tags} -> tags) (\s@Evaluation' {} a -> s {tags = a} :: Evaluation) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the contact evaluation.
evaluation_evaluationId :: Lens.Lens' Evaluation Prelude.Text
evaluation_evaluationId = Lens.lens (\Evaluation' {evaluationId} -> evaluationId) (\s@Evaluation' {} a -> s {evaluationId = a} :: Evaluation)

-- | The Amazon Resource Name (ARN) for the contact evaluation resource.
evaluation_evaluationArn :: Lens.Lens' Evaluation Prelude.Text
evaluation_evaluationArn = Lens.lens (\Evaluation' {evaluationArn} -> evaluationArn) (\s@Evaluation' {} a -> s {evaluationArn = a} :: Evaluation)

-- | Metadata about the contact evaluation.
evaluation_metadata :: Lens.Lens' Evaluation EvaluationMetadata
evaluation_metadata = Lens.lens (\Evaluation' {metadata} -> metadata) (\s@Evaluation' {} a -> s {metadata = a} :: Evaluation)

-- | A map of question identifiers to answer value.
evaluation_answers :: Lens.Lens' Evaluation (Prelude.HashMap Prelude.Text EvaluationAnswerOutput)
evaluation_answers = Lens.lens (\Evaluation' {answers} -> answers) (\s@Evaluation' {} a -> s {answers = a} :: Evaluation) Prelude.. Lens.coerced

-- | A map of question identifiers to note value.
evaluation_notes :: Lens.Lens' Evaluation (Prelude.HashMap Prelude.Text EvaluationNote)
evaluation_notes = Lens.lens (\Evaluation' {notes} -> notes) (\s@Evaluation' {} a -> s {notes = a} :: Evaluation) Prelude.. Lens.coerced

-- | The status of the contact evaluation.
evaluation_status :: Lens.Lens' Evaluation EvaluationStatus
evaluation_status = Lens.lens (\Evaluation' {status} -> status) (\s@Evaluation' {} a -> s {status = a} :: Evaluation)

-- | The timestamp for when the evaluation was created.
evaluation_createdTime :: Lens.Lens' Evaluation Prelude.UTCTime
evaluation_createdTime = Lens.lens (\Evaluation' {createdTime} -> createdTime) (\s@Evaluation' {} a -> s {createdTime = a} :: Evaluation) Prelude.. Data._Time

-- | The timestamp for when the evaluation was last updated.
evaluation_lastModifiedTime :: Lens.Lens' Evaluation Prelude.UTCTime
evaluation_lastModifiedTime = Lens.lens (\Evaluation' {lastModifiedTime} -> lastModifiedTime) (\s@Evaluation' {} a -> s {lastModifiedTime = a} :: Evaluation) Prelude.. Data._Time

instance Data.FromJSON Evaluation where
  parseJSON =
    Data.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Prelude.<$> (x Data..:? "Scores" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "EvaluationId")
            Prelude.<*> (x Data..: "EvaluationArn")
            Prelude.<*> (x Data..: "Metadata")
            Prelude.<*> (x Data..:? "Answers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Notes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "CreatedTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable Evaluation where
  hashWithSalt _salt Evaluation' {..} =
    _salt
      `Prelude.hashWithSalt` scores
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` evaluationId
      `Prelude.hashWithSalt` evaluationArn
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` answers
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData Evaluation where
  rnf Evaluation' {..} =
    Prelude.rnf scores
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf evaluationId
      `Prelude.seq` Prelude.rnf evaluationArn
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf answers
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
