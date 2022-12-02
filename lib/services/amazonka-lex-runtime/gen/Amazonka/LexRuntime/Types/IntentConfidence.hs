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
-- Module      : Amazonka.LexRuntime.Types.IntentConfidence
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.IntentConfidence where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a score that indicates the confidence that Amazon Lex has that
-- an intent is the one that satisfies the user\'s intent.
--
-- /See:/ 'newIntentConfidence' smart constructor.
data IntentConfidence = IntentConfidence'
  { -- | A score that indicates how confident Amazon Lex is that an intent
    -- satisfies the user\'s intent. Ranges between 0.00 and 1.00. Higher
    -- scores indicate higher confidence.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentConfidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'intentConfidence_score' - A score that indicates how confident Amazon Lex is that an intent
-- satisfies the user\'s intent. Ranges between 0.00 and 1.00. Higher
-- scores indicate higher confidence.
newIntentConfidence ::
  IntentConfidence
newIntentConfidence =
  IntentConfidence' {score = Prelude.Nothing}

-- | A score that indicates how confident Amazon Lex is that an intent
-- satisfies the user\'s intent. Ranges between 0.00 and 1.00. Higher
-- scores indicate higher confidence.
intentConfidence_score :: Lens.Lens' IntentConfidence (Prelude.Maybe Prelude.Double)
intentConfidence_score = Lens.lens (\IntentConfidence' {score} -> score) (\s@IntentConfidence' {} a -> s {score = a} :: IntentConfidence)

instance Data.FromJSON IntentConfidence where
  parseJSON =
    Data.withObject
      "IntentConfidence"
      ( \x ->
          IntentConfidence' Prelude.<$> (x Data..:? "score")
      )

instance Prelude.Hashable IntentConfidence where
  hashWithSalt _salt IntentConfidence' {..} =
    _salt `Prelude.hashWithSalt` score

instance Prelude.NFData IntentConfidence where
  rnf IntentConfidence' {..} = Prelude.rnf score
