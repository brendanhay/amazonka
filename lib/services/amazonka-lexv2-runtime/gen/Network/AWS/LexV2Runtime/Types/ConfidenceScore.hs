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
-- Module      : Network.AWS.LexV2Runtime.Types.ConfidenceScore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.ConfidenceScore where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a score that indicates the confidence that Amazon Lex V2 has
-- that an intent is the one that satisfies the user\'s intent.
--
-- /See:/ 'newConfidenceScore' smart constructor.
data ConfidenceScore = ConfidenceScore'
  { -- | A score that indicates how confident Amazon Lex V2 is that an intent
    -- satisfies the user\'s intent. Ranges between 0.00 and 1.00. Higher
    -- scores indicate higher confidence.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfidenceScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'confidenceScore_score' - A score that indicates how confident Amazon Lex V2 is that an intent
-- satisfies the user\'s intent. Ranges between 0.00 and 1.00. Higher
-- scores indicate higher confidence.
newConfidenceScore ::
  ConfidenceScore
newConfidenceScore =
  ConfidenceScore' {score = Prelude.Nothing}

-- | A score that indicates how confident Amazon Lex V2 is that an intent
-- satisfies the user\'s intent. Ranges between 0.00 and 1.00. Higher
-- scores indicate higher confidence.
confidenceScore_score :: Lens.Lens' ConfidenceScore (Prelude.Maybe Prelude.Double)
confidenceScore_score = Lens.lens (\ConfidenceScore' {score} -> score) (\s@ConfidenceScore' {} a -> s {score = a} :: ConfidenceScore)

instance Core.FromJSON ConfidenceScore where
  parseJSON =
    Core.withObject
      "ConfidenceScore"
      ( \x ->
          ConfidenceScore' Prelude.<$> (x Core..:? "score")
      )

instance Prelude.Hashable ConfidenceScore

instance Prelude.NFData ConfidenceScore
