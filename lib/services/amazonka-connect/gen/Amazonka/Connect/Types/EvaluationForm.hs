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
-- Module      : Amazonka.Connect.Types.EvaluationForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationForm where

import Amazonka.Connect.Types.EvaluationFormItem
import Amazonka.Connect.Types.EvaluationFormScoringStrategy
import Amazonka.Connect.Types.EvaluationFormVersionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the evaluation form.
--
-- /See:/ 'newEvaluationForm' smart constructor.
data EvaluationForm = EvaluationForm'
  { -- | The description of the evaluation form.
    description :: Prelude.Maybe Prelude.Text,
    -- | A scoring strategy of the evaluation form.
    scoringStrategy :: Prelude.Maybe EvaluationFormScoringStrategy,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | A version of the evaluation form.
    evaluationFormVersion :: Prelude.Natural,
    -- | The flag indicating whether the evaluation form is locked for changes.
    locked :: Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the evaluation form resource.
    evaluationFormArn :: Prelude.Text,
    -- | A title of the evaluation form.
    title :: Prelude.Text,
    -- | The status of the evaluation form.
    status :: EvaluationFormVersionStatus,
    -- | Items that are part of the evaluation form. The total number of sections
    -- and questions must not exceed 100 each. Questions must be contained in a
    -- section.
    items :: Prelude.NonEmpty EvaluationFormItem,
    -- | The timestamp for when the evaluation form was created.
    createdTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who created the evaluation
    -- form.
    createdBy :: Prelude.Text,
    -- | The timestamp for when the evaluation form was last updated.
    lastModifiedTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who last updated the
    -- evaluation form.
    lastModifiedBy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'evaluationForm_description' - The description of the evaluation form.
--
-- 'scoringStrategy', 'evaluationForm_scoringStrategy' - A scoring strategy of the evaluation form.
--
-- 'tags', 'evaluationForm_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'evaluationFormId', 'evaluationForm_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormVersion', 'evaluationForm_evaluationFormVersion' - A version of the evaluation form.
--
-- 'locked', 'evaluationForm_locked' - The flag indicating whether the evaluation form is locked for changes.
--
-- 'evaluationFormArn', 'evaluationForm_evaluationFormArn' - The Amazon Resource Name (ARN) for the evaluation form resource.
--
-- 'title', 'evaluationForm_title' - A title of the evaluation form.
--
-- 'status', 'evaluationForm_status' - The status of the evaluation form.
--
-- 'items', 'evaluationForm_items' - Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
--
-- 'createdTime', 'evaluationForm_createdTime' - The timestamp for when the evaluation form was created.
--
-- 'createdBy', 'evaluationForm_createdBy' - The Amazon Resource Name (ARN) of the user who created the evaluation
-- form.
--
-- 'lastModifiedTime', 'evaluationForm_lastModifiedTime' - The timestamp for when the evaluation form was last updated.
--
-- 'lastModifiedBy', 'evaluationForm_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation form.
newEvaluationForm ::
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  -- | 'locked'
  Prelude.Bool ->
  -- | 'evaluationFormArn'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'status'
  EvaluationFormVersionStatus ->
  -- | 'items'
  Prelude.NonEmpty EvaluationFormItem ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'createdBy'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedBy'
  Prelude.Text ->
  EvaluationForm
newEvaluationForm
  pEvaluationFormId_
  pEvaluationFormVersion_
  pLocked_
  pEvaluationFormArn_
  pTitle_
  pStatus_
  pItems_
  pCreatedTime_
  pCreatedBy_
  pLastModifiedTime_
  pLastModifiedBy_ =
    EvaluationForm'
      { description = Prelude.Nothing,
        scoringStrategy = Prelude.Nothing,
        tags = Prelude.Nothing,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormVersion = pEvaluationFormVersion_,
        locked = pLocked_,
        evaluationFormArn = pEvaluationFormArn_,
        title = pTitle_,
        status = pStatus_,
        items = Lens.coerced Lens.# pItems_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        createdBy = pCreatedBy_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        lastModifiedBy = pLastModifiedBy_
      }

-- | The description of the evaluation form.
evaluationForm_description :: Lens.Lens' EvaluationForm (Prelude.Maybe Prelude.Text)
evaluationForm_description = Lens.lens (\EvaluationForm' {description} -> description) (\s@EvaluationForm' {} a -> s {description = a} :: EvaluationForm)

-- | A scoring strategy of the evaluation form.
evaluationForm_scoringStrategy :: Lens.Lens' EvaluationForm (Prelude.Maybe EvaluationFormScoringStrategy)
evaluationForm_scoringStrategy = Lens.lens (\EvaluationForm' {scoringStrategy} -> scoringStrategy) (\s@EvaluationForm' {} a -> s {scoringStrategy = a} :: EvaluationForm)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
evaluationForm_tags :: Lens.Lens' EvaluationForm (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evaluationForm_tags = Lens.lens (\EvaluationForm' {tags} -> tags) (\s@EvaluationForm' {} a -> s {tags = a} :: EvaluationForm) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the evaluation form.
evaluationForm_evaluationFormId :: Lens.Lens' EvaluationForm Prelude.Text
evaluationForm_evaluationFormId = Lens.lens (\EvaluationForm' {evaluationFormId} -> evaluationFormId) (\s@EvaluationForm' {} a -> s {evaluationFormId = a} :: EvaluationForm)

-- | A version of the evaluation form.
evaluationForm_evaluationFormVersion :: Lens.Lens' EvaluationForm Prelude.Natural
evaluationForm_evaluationFormVersion = Lens.lens (\EvaluationForm' {evaluationFormVersion} -> evaluationFormVersion) (\s@EvaluationForm' {} a -> s {evaluationFormVersion = a} :: EvaluationForm)

-- | The flag indicating whether the evaluation form is locked for changes.
evaluationForm_locked :: Lens.Lens' EvaluationForm Prelude.Bool
evaluationForm_locked = Lens.lens (\EvaluationForm' {locked} -> locked) (\s@EvaluationForm' {} a -> s {locked = a} :: EvaluationForm)

-- | The Amazon Resource Name (ARN) for the evaluation form resource.
evaluationForm_evaluationFormArn :: Lens.Lens' EvaluationForm Prelude.Text
evaluationForm_evaluationFormArn = Lens.lens (\EvaluationForm' {evaluationFormArn} -> evaluationFormArn) (\s@EvaluationForm' {} a -> s {evaluationFormArn = a} :: EvaluationForm)

-- | A title of the evaluation form.
evaluationForm_title :: Lens.Lens' EvaluationForm Prelude.Text
evaluationForm_title = Lens.lens (\EvaluationForm' {title} -> title) (\s@EvaluationForm' {} a -> s {title = a} :: EvaluationForm)

-- | The status of the evaluation form.
evaluationForm_status :: Lens.Lens' EvaluationForm EvaluationFormVersionStatus
evaluationForm_status = Lens.lens (\EvaluationForm' {status} -> status) (\s@EvaluationForm' {} a -> s {status = a} :: EvaluationForm)

-- | Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
evaluationForm_items :: Lens.Lens' EvaluationForm (Prelude.NonEmpty EvaluationFormItem)
evaluationForm_items = Lens.lens (\EvaluationForm' {items} -> items) (\s@EvaluationForm' {} a -> s {items = a} :: EvaluationForm) Prelude.. Lens.coerced

-- | The timestamp for when the evaluation form was created.
evaluationForm_createdTime :: Lens.Lens' EvaluationForm Prelude.UTCTime
evaluationForm_createdTime = Lens.lens (\EvaluationForm' {createdTime} -> createdTime) (\s@EvaluationForm' {} a -> s {createdTime = a} :: EvaluationForm) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the user who created the evaluation
-- form.
evaluationForm_createdBy :: Lens.Lens' EvaluationForm Prelude.Text
evaluationForm_createdBy = Lens.lens (\EvaluationForm' {createdBy} -> createdBy) (\s@EvaluationForm' {} a -> s {createdBy = a} :: EvaluationForm)

-- | The timestamp for when the evaluation form was last updated.
evaluationForm_lastModifiedTime :: Lens.Lens' EvaluationForm Prelude.UTCTime
evaluationForm_lastModifiedTime = Lens.lens (\EvaluationForm' {lastModifiedTime} -> lastModifiedTime) (\s@EvaluationForm' {} a -> s {lastModifiedTime = a} :: EvaluationForm) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation form.
evaluationForm_lastModifiedBy :: Lens.Lens' EvaluationForm Prelude.Text
evaluationForm_lastModifiedBy = Lens.lens (\EvaluationForm' {lastModifiedBy} -> lastModifiedBy) (\s@EvaluationForm' {} a -> s {lastModifiedBy = a} :: EvaluationForm)

instance Data.FromJSON EvaluationForm where
  parseJSON =
    Data.withObject
      "EvaluationForm"
      ( \x ->
          EvaluationForm'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ScoringStrategy")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "EvaluationFormId")
            Prelude.<*> (x Data..: "EvaluationFormVersion")
            Prelude.<*> (x Data..: "Locked")
            Prelude.<*> (x Data..: "EvaluationFormArn")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "Items")
            Prelude.<*> (x Data..: "CreatedTime")
            Prelude.<*> (x Data..: "CreatedBy")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "LastModifiedBy")
      )

instance Prelude.Hashable EvaluationForm where
  hashWithSalt _salt EvaluationForm' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` scoringStrategy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` evaluationFormVersion
      `Prelude.hashWithSalt` locked
      `Prelude.hashWithSalt` evaluationFormArn
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastModifiedBy

instance Prelude.NFData EvaluationForm where
  rnf EvaluationForm' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf scoringStrategy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormVersion
      `Prelude.seq` Prelude.rnf locked
      `Prelude.seq` Prelude.rnf evaluationFormArn
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
