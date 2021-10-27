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
-- Module      : Network.AWS.WellArchitected.Types.QuestionDifference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.QuestionDifference where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WellArchitected.Types.DifferenceStatus

-- | A question difference return object.
--
-- /See:/ 'newQuestionDifference' smart constructor.
data QuestionDifference = QuestionDifference'
  { questionTitle :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of change to the question.
    differenceStatus :: Prelude.Maybe DifferenceStatus,
    questionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuestionDifference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'questionTitle', 'questionDifference_questionTitle' - Undocumented member.
--
-- 'differenceStatus', 'questionDifference_differenceStatus' - Indicates the type of change to the question.
--
-- 'questionId', 'questionDifference_questionId' - Undocumented member.
newQuestionDifference ::
  QuestionDifference
newQuestionDifference =
  QuestionDifference'
    { questionTitle =
        Prelude.Nothing,
      differenceStatus = Prelude.Nothing,
      questionId = Prelude.Nothing
    }

-- | Undocumented member.
questionDifference_questionTitle :: Lens.Lens' QuestionDifference (Prelude.Maybe Prelude.Text)
questionDifference_questionTitle = Lens.lens (\QuestionDifference' {questionTitle} -> questionTitle) (\s@QuestionDifference' {} a -> s {questionTitle = a} :: QuestionDifference)

-- | Indicates the type of change to the question.
questionDifference_differenceStatus :: Lens.Lens' QuestionDifference (Prelude.Maybe DifferenceStatus)
questionDifference_differenceStatus = Lens.lens (\QuestionDifference' {differenceStatus} -> differenceStatus) (\s@QuestionDifference' {} a -> s {differenceStatus = a} :: QuestionDifference)

-- | Undocumented member.
questionDifference_questionId :: Lens.Lens' QuestionDifference (Prelude.Maybe Prelude.Text)
questionDifference_questionId = Lens.lens (\QuestionDifference' {questionId} -> questionId) (\s@QuestionDifference' {} a -> s {questionId = a} :: QuestionDifference)

instance Core.FromJSON QuestionDifference where
  parseJSON =
    Core.withObject
      "QuestionDifference"
      ( \x ->
          QuestionDifference'
            Prelude.<$> (x Core..:? "QuestionTitle")
            Prelude.<*> (x Core..:? "DifferenceStatus")
            Prelude.<*> (x Core..:? "QuestionId")
      )

instance Prelude.Hashable QuestionDifference

instance Prelude.NFData QuestionDifference
