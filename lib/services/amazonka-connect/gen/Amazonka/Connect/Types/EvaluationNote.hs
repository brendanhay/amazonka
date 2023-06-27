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
-- Module      : Amazonka.Connect.Types.EvaluationNote
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationNote where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about notes for a contact evaluation.
--
-- /See:/ 'newEvaluationNote' smart constructor.
data EvaluationNote = EvaluationNote'
  { -- | The note for an item (section or question) in a contact evaluation.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationNote' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'evaluationNote_value' - The note for an item (section or question) in a contact evaluation.
newEvaluationNote ::
  EvaluationNote
newEvaluationNote =
  EvaluationNote' {value = Prelude.Nothing}

-- | The note for an item (section or question) in a contact evaluation.
evaluationNote_value :: Lens.Lens' EvaluationNote (Prelude.Maybe Prelude.Text)
evaluationNote_value = Lens.lens (\EvaluationNote' {value} -> value) (\s@EvaluationNote' {} a -> s {value = a} :: EvaluationNote)

instance Data.FromJSON EvaluationNote where
  parseJSON =
    Data.withObject
      "EvaluationNote"
      ( \x ->
          EvaluationNote' Prelude.<$> (x Data..:? "Value")
      )

instance Prelude.Hashable EvaluationNote where
  hashWithSalt _salt EvaluationNote' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData EvaluationNote where
  rnf EvaluationNote' {..} = Prelude.rnf value

instance Data.ToJSON EvaluationNote where
  toJSON EvaluationNote' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Value" Data..=) Prelude.<$> value]
      )
