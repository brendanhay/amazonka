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
-- Module      : Amazonka.Connect.Types.EvaluationFormSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an evaluation form.
--
-- /See:/ 'newEvaluationFormSummary' smart constructor.
data EvaluationFormSummary = EvaluationFormSummary'
  { -- | The version of the active evaluation form version.
    activeVersion :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the user who last activated the
    -- evaluation form.
    lastActivatedBy :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when the evaluation form was last activated.
    lastActivatedTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the evaluation form resource.
    evaluationFormArn :: Prelude.Text,
    -- | A title of the evaluation form.
    title :: Prelude.Text,
    -- | The timestamp for when the evaluation form was created.
    createdTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who created the evaluation
    -- form.
    createdBy :: Prelude.Text,
    -- | The timestamp for when the evaluation form was last updated.
    lastModifiedTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who last updated the
    -- evaluation form.
    lastModifiedBy :: Prelude.Text,
    -- | The version number of the latest evaluation form version.
    latestVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeVersion', 'evaluationFormSummary_activeVersion' - The version of the active evaluation form version.
--
-- 'lastActivatedBy', 'evaluationFormSummary_lastActivatedBy' - The Amazon Resource Name (ARN) of the user who last activated the
-- evaluation form.
--
-- 'lastActivatedTime', 'evaluationFormSummary_lastActivatedTime' - The timestamp for when the evaluation form was last activated.
--
-- 'evaluationFormId', 'evaluationFormSummary_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormArn', 'evaluationFormSummary_evaluationFormArn' - The Amazon Resource Name (ARN) for the evaluation form resource.
--
-- 'title', 'evaluationFormSummary_title' - A title of the evaluation form.
--
-- 'createdTime', 'evaluationFormSummary_createdTime' - The timestamp for when the evaluation form was created.
--
-- 'createdBy', 'evaluationFormSummary_createdBy' - The Amazon Resource Name (ARN) of the user who created the evaluation
-- form.
--
-- 'lastModifiedTime', 'evaluationFormSummary_lastModifiedTime' - The timestamp for when the evaluation form was last updated.
--
-- 'lastModifiedBy', 'evaluationFormSummary_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation form.
--
-- 'latestVersion', 'evaluationFormSummary_latestVersion' - The version number of the latest evaluation form version.
newEvaluationFormSummary ::
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormArn'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'createdBy'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedBy'
  Prelude.Text ->
  -- | 'latestVersion'
  Prelude.Natural ->
  EvaluationFormSummary
newEvaluationFormSummary
  pEvaluationFormId_
  pEvaluationFormArn_
  pTitle_
  pCreatedTime_
  pCreatedBy_
  pLastModifiedTime_
  pLastModifiedBy_
  pLatestVersion_ =
    EvaluationFormSummary'
      { activeVersion =
          Prelude.Nothing,
        lastActivatedBy = Prelude.Nothing,
        lastActivatedTime = Prelude.Nothing,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormArn = pEvaluationFormArn_,
        title = pTitle_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        createdBy = pCreatedBy_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        lastModifiedBy = pLastModifiedBy_,
        latestVersion = pLatestVersion_
      }

-- | The version of the active evaluation form version.
evaluationFormSummary_activeVersion :: Lens.Lens' EvaluationFormSummary (Prelude.Maybe Prelude.Natural)
evaluationFormSummary_activeVersion = Lens.lens (\EvaluationFormSummary' {activeVersion} -> activeVersion) (\s@EvaluationFormSummary' {} a -> s {activeVersion = a} :: EvaluationFormSummary)

-- | The Amazon Resource Name (ARN) of the user who last activated the
-- evaluation form.
evaluationFormSummary_lastActivatedBy :: Lens.Lens' EvaluationFormSummary (Prelude.Maybe Prelude.Text)
evaluationFormSummary_lastActivatedBy = Lens.lens (\EvaluationFormSummary' {lastActivatedBy} -> lastActivatedBy) (\s@EvaluationFormSummary' {} a -> s {lastActivatedBy = a} :: EvaluationFormSummary)

-- | The timestamp for when the evaluation form was last activated.
evaluationFormSummary_lastActivatedTime :: Lens.Lens' EvaluationFormSummary (Prelude.Maybe Prelude.UTCTime)
evaluationFormSummary_lastActivatedTime = Lens.lens (\EvaluationFormSummary' {lastActivatedTime} -> lastActivatedTime) (\s@EvaluationFormSummary' {} a -> s {lastActivatedTime = a} :: EvaluationFormSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the evaluation form.
evaluationFormSummary_evaluationFormId :: Lens.Lens' EvaluationFormSummary Prelude.Text
evaluationFormSummary_evaluationFormId = Lens.lens (\EvaluationFormSummary' {evaluationFormId} -> evaluationFormId) (\s@EvaluationFormSummary' {} a -> s {evaluationFormId = a} :: EvaluationFormSummary)

-- | The Amazon Resource Name (ARN) for the evaluation form resource.
evaluationFormSummary_evaluationFormArn :: Lens.Lens' EvaluationFormSummary Prelude.Text
evaluationFormSummary_evaluationFormArn = Lens.lens (\EvaluationFormSummary' {evaluationFormArn} -> evaluationFormArn) (\s@EvaluationFormSummary' {} a -> s {evaluationFormArn = a} :: EvaluationFormSummary)

-- | A title of the evaluation form.
evaluationFormSummary_title :: Lens.Lens' EvaluationFormSummary Prelude.Text
evaluationFormSummary_title = Lens.lens (\EvaluationFormSummary' {title} -> title) (\s@EvaluationFormSummary' {} a -> s {title = a} :: EvaluationFormSummary)

-- | The timestamp for when the evaluation form was created.
evaluationFormSummary_createdTime :: Lens.Lens' EvaluationFormSummary Prelude.UTCTime
evaluationFormSummary_createdTime = Lens.lens (\EvaluationFormSummary' {createdTime} -> createdTime) (\s@EvaluationFormSummary' {} a -> s {createdTime = a} :: EvaluationFormSummary) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the user who created the evaluation
-- form.
evaluationFormSummary_createdBy :: Lens.Lens' EvaluationFormSummary Prelude.Text
evaluationFormSummary_createdBy = Lens.lens (\EvaluationFormSummary' {createdBy} -> createdBy) (\s@EvaluationFormSummary' {} a -> s {createdBy = a} :: EvaluationFormSummary)

-- | The timestamp for when the evaluation form was last updated.
evaluationFormSummary_lastModifiedTime :: Lens.Lens' EvaluationFormSummary Prelude.UTCTime
evaluationFormSummary_lastModifiedTime = Lens.lens (\EvaluationFormSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EvaluationFormSummary' {} a -> s {lastModifiedTime = a} :: EvaluationFormSummary) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation form.
evaluationFormSummary_lastModifiedBy :: Lens.Lens' EvaluationFormSummary Prelude.Text
evaluationFormSummary_lastModifiedBy = Lens.lens (\EvaluationFormSummary' {lastModifiedBy} -> lastModifiedBy) (\s@EvaluationFormSummary' {} a -> s {lastModifiedBy = a} :: EvaluationFormSummary)

-- | The version number of the latest evaluation form version.
evaluationFormSummary_latestVersion :: Lens.Lens' EvaluationFormSummary Prelude.Natural
evaluationFormSummary_latestVersion = Lens.lens (\EvaluationFormSummary' {latestVersion} -> latestVersion) (\s@EvaluationFormSummary' {} a -> s {latestVersion = a} :: EvaluationFormSummary)

instance Data.FromJSON EvaluationFormSummary where
  parseJSON =
    Data.withObject
      "EvaluationFormSummary"
      ( \x ->
          EvaluationFormSummary'
            Prelude.<$> (x Data..:? "ActiveVersion")
            Prelude.<*> (x Data..:? "LastActivatedBy")
            Prelude.<*> (x Data..:? "LastActivatedTime")
            Prelude.<*> (x Data..: "EvaluationFormId")
            Prelude.<*> (x Data..: "EvaluationFormArn")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "CreatedTime")
            Prelude.<*> (x Data..: "CreatedBy")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "LastModifiedBy")
            Prelude.<*> (x Data..: "LatestVersion")
      )

instance Prelude.Hashable EvaluationFormSummary where
  hashWithSalt _salt EvaluationFormSummary' {..} =
    _salt
      `Prelude.hashWithSalt` activeVersion
      `Prelude.hashWithSalt` lastActivatedBy
      `Prelude.hashWithSalt` lastActivatedTime
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` evaluationFormArn
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` latestVersion

instance Prelude.NFData EvaluationFormSummary where
  rnf EvaluationFormSummary' {..} =
    Prelude.rnf activeVersion
      `Prelude.seq` Prelude.rnf lastActivatedBy
      `Prelude.seq` Prelude.rnf lastActivatedTime
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormArn
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf latestVersion
