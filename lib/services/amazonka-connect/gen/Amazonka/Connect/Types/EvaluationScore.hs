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
-- Module      : Amazonka.Connect.Types.EvaluationScore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationScore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about scores of a contact evaluation item (section or
-- question).
--
-- /See:/ 'newEvaluationScore' smart constructor.
data EvaluationScore = EvaluationScore'
  { -- | The flag that marks the item as automatic fail. If the item or a child
    -- item gets an automatic fail answer, this flag will be true.
    automaticFail :: Prelude.Maybe Prelude.Bool,
    -- | The flag to mark the item as not applicable for scoring.
    notApplicable :: Prelude.Maybe Prelude.Bool,
    -- | The score percentage for an item in a contact evaluation.
    percentage :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticFail', 'evaluationScore_automaticFail' - The flag that marks the item as automatic fail. If the item or a child
-- item gets an automatic fail answer, this flag will be true.
--
-- 'notApplicable', 'evaluationScore_notApplicable' - The flag to mark the item as not applicable for scoring.
--
-- 'percentage', 'evaluationScore_percentage' - The score percentage for an item in a contact evaluation.
newEvaluationScore ::
  EvaluationScore
newEvaluationScore =
  EvaluationScore'
    { automaticFail = Prelude.Nothing,
      notApplicable = Prelude.Nothing,
      percentage = Prelude.Nothing
    }

-- | The flag that marks the item as automatic fail. If the item or a child
-- item gets an automatic fail answer, this flag will be true.
evaluationScore_automaticFail :: Lens.Lens' EvaluationScore (Prelude.Maybe Prelude.Bool)
evaluationScore_automaticFail = Lens.lens (\EvaluationScore' {automaticFail} -> automaticFail) (\s@EvaluationScore' {} a -> s {automaticFail = a} :: EvaluationScore)

-- | The flag to mark the item as not applicable for scoring.
evaluationScore_notApplicable :: Lens.Lens' EvaluationScore (Prelude.Maybe Prelude.Bool)
evaluationScore_notApplicable = Lens.lens (\EvaluationScore' {notApplicable} -> notApplicable) (\s@EvaluationScore' {} a -> s {notApplicable = a} :: EvaluationScore)

-- | The score percentage for an item in a contact evaluation.
evaluationScore_percentage :: Lens.Lens' EvaluationScore (Prelude.Maybe Prelude.Double)
evaluationScore_percentage = Lens.lens (\EvaluationScore' {percentage} -> percentage) (\s@EvaluationScore' {} a -> s {percentage = a} :: EvaluationScore)

instance Data.FromJSON EvaluationScore where
  parseJSON =
    Data.withObject
      "EvaluationScore"
      ( \x ->
          EvaluationScore'
            Prelude.<$> (x Data..:? "AutomaticFail")
            Prelude.<*> (x Data..:? "NotApplicable")
            Prelude.<*> (x Data..:? "Percentage")
      )

instance Prelude.Hashable EvaluationScore where
  hashWithSalt _salt EvaluationScore' {..} =
    _salt
      `Prelude.hashWithSalt` automaticFail
      `Prelude.hashWithSalt` notApplicable
      `Prelude.hashWithSalt` percentage

instance Prelude.NFData EvaluationScore where
  rnf EvaluationScore' {..} =
    Prelude.rnf automaticFail
      `Prelude.seq` Prelude.rnf notApplicable
      `Prelude.seq` Prelude.rnf percentage
