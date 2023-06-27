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
-- Module      : Amazonka.Connect.Types.EvaluationFormVersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormVersionSummary where

import Amazonka.Connect.Types.EvaluationFormVersionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an evaluation form.
--
-- /See:/ 'newEvaluationFormVersionSummary' smart constructor.
data EvaluationFormVersionSummary = EvaluationFormVersionSummary'
  { -- | The Amazon Resource Name (ARN) for the evaluation form resource.
    evaluationFormArn :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | A version of the evaluation form.
    evaluationFormVersion :: Prelude.Natural,
    -- | The flag indicating whether the evaluation form is locked for changes.
    locked :: Prelude.Bool,
    -- | The status of the evaluation form.
    status :: EvaluationFormVersionStatus,
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
-- Create a value of 'EvaluationFormVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationFormArn', 'evaluationFormVersionSummary_evaluationFormArn' - The Amazon Resource Name (ARN) for the evaluation form resource.
--
-- 'evaluationFormId', 'evaluationFormVersionSummary_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormVersion', 'evaluationFormVersionSummary_evaluationFormVersion' - A version of the evaluation form.
--
-- 'locked', 'evaluationFormVersionSummary_locked' - The flag indicating whether the evaluation form is locked for changes.
--
-- 'status', 'evaluationFormVersionSummary_status' - The status of the evaluation form.
--
-- 'createdTime', 'evaluationFormVersionSummary_createdTime' - The timestamp for when the evaluation form was created.
--
-- 'createdBy', 'evaluationFormVersionSummary_createdBy' - The Amazon Resource Name (ARN) of the user who created the evaluation
-- form.
--
-- 'lastModifiedTime', 'evaluationFormVersionSummary_lastModifiedTime' - The timestamp for when the evaluation form was last updated.
--
-- 'lastModifiedBy', 'evaluationFormVersionSummary_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation form.
newEvaluationFormVersionSummary ::
  -- | 'evaluationFormArn'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  -- | 'locked'
  Prelude.Bool ->
  -- | 'status'
  EvaluationFormVersionStatus ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'createdBy'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedBy'
  Prelude.Text ->
  EvaluationFormVersionSummary
newEvaluationFormVersionSummary
  pEvaluationFormArn_
  pEvaluationFormId_
  pEvaluationFormVersion_
  pLocked_
  pStatus_
  pCreatedTime_
  pCreatedBy_
  pLastModifiedTime_
  pLastModifiedBy_ =
    EvaluationFormVersionSummary'
      { evaluationFormArn =
          pEvaluationFormArn_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormVersion =
          pEvaluationFormVersion_,
        locked = pLocked_,
        status = pStatus_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        createdBy = pCreatedBy_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        lastModifiedBy = pLastModifiedBy_
      }

-- | The Amazon Resource Name (ARN) for the evaluation form resource.
evaluationFormVersionSummary_evaluationFormArn :: Lens.Lens' EvaluationFormVersionSummary Prelude.Text
evaluationFormVersionSummary_evaluationFormArn = Lens.lens (\EvaluationFormVersionSummary' {evaluationFormArn} -> evaluationFormArn) (\s@EvaluationFormVersionSummary' {} a -> s {evaluationFormArn = a} :: EvaluationFormVersionSummary)

-- | The unique identifier for the evaluation form.
evaluationFormVersionSummary_evaluationFormId :: Lens.Lens' EvaluationFormVersionSummary Prelude.Text
evaluationFormVersionSummary_evaluationFormId = Lens.lens (\EvaluationFormVersionSummary' {evaluationFormId} -> evaluationFormId) (\s@EvaluationFormVersionSummary' {} a -> s {evaluationFormId = a} :: EvaluationFormVersionSummary)

-- | A version of the evaluation form.
evaluationFormVersionSummary_evaluationFormVersion :: Lens.Lens' EvaluationFormVersionSummary Prelude.Natural
evaluationFormVersionSummary_evaluationFormVersion = Lens.lens (\EvaluationFormVersionSummary' {evaluationFormVersion} -> evaluationFormVersion) (\s@EvaluationFormVersionSummary' {} a -> s {evaluationFormVersion = a} :: EvaluationFormVersionSummary)

-- | The flag indicating whether the evaluation form is locked for changes.
evaluationFormVersionSummary_locked :: Lens.Lens' EvaluationFormVersionSummary Prelude.Bool
evaluationFormVersionSummary_locked = Lens.lens (\EvaluationFormVersionSummary' {locked} -> locked) (\s@EvaluationFormVersionSummary' {} a -> s {locked = a} :: EvaluationFormVersionSummary)

-- | The status of the evaluation form.
evaluationFormVersionSummary_status :: Lens.Lens' EvaluationFormVersionSummary EvaluationFormVersionStatus
evaluationFormVersionSummary_status = Lens.lens (\EvaluationFormVersionSummary' {status} -> status) (\s@EvaluationFormVersionSummary' {} a -> s {status = a} :: EvaluationFormVersionSummary)

-- | The timestamp for when the evaluation form was created.
evaluationFormVersionSummary_createdTime :: Lens.Lens' EvaluationFormVersionSummary Prelude.UTCTime
evaluationFormVersionSummary_createdTime = Lens.lens (\EvaluationFormVersionSummary' {createdTime} -> createdTime) (\s@EvaluationFormVersionSummary' {} a -> s {createdTime = a} :: EvaluationFormVersionSummary) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the user who created the evaluation
-- form.
evaluationFormVersionSummary_createdBy :: Lens.Lens' EvaluationFormVersionSummary Prelude.Text
evaluationFormVersionSummary_createdBy = Lens.lens (\EvaluationFormVersionSummary' {createdBy} -> createdBy) (\s@EvaluationFormVersionSummary' {} a -> s {createdBy = a} :: EvaluationFormVersionSummary)

-- | The timestamp for when the evaluation form was last updated.
evaluationFormVersionSummary_lastModifiedTime :: Lens.Lens' EvaluationFormVersionSummary Prelude.UTCTime
evaluationFormVersionSummary_lastModifiedTime = Lens.lens (\EvaluationFormVersionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EvaluationFormVersionSummary' {} a -> s {lastModifiedTime = a} :: EvaluationFormVersionSummary) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation form.
evaluationFormVersionSummary_lastModifiedBy :: Lens.Lens' EvaluationFormVersionSummary Prelude.Text
evaluationFormVersionSummary_lastModifiedBy = Lens.lens (\EvaluationFormVersionSummary' {lastModifiedBy} -> lastModifiedBy) (\s@EvaluationFormVersionSummary' {} a -> s {lastModifiedBy = a} :: EvaluationFormVersionSummary)

instance Data.FromJSON EvaluationFormVersionSummary where
  parseJSON =
    Data.withObject
      "EvaluationFormVersionSummary"
      ( \x ->
          EvaluationFormVersionSummary'
            Prelude.<$> (x Data..: "EvaluationFormArn")
            Prelude.<*> (x Data..: "EvaluationFormId")
            Prelude.<*> (x Data..: "EvaluationFormVersion")
            Prelude.<*> (x Data..: "Locked")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "CreatedTime")
            Prelude.<*> (x Data..: "CreatedBy")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "LastModifiedBy")
      )

instance
  Prelude.Hashable
    EvaluationFormVersionSummary
  where
  hashWithSalt _salt EvaluationFormVersionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationFormArn
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` evaluationFormVersion
      `Prelude.hashWithSalt` locked
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastModifiedBy

instance Prelude.NFData EvaluationFormVersionSummary where
  rnf EvaluationFormVersionSummary' {..} =
    Prelude.rnf evaluationFormArn
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormVersion
      `Prelude.seq` Prelude.rnf locked
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
