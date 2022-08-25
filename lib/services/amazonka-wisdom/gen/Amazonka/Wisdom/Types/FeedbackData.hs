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
-- Module      : Amazonka.Wisdom.Types.FeedbackData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.FeedbackData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.Relevance

-- | The feedback to submit to Wisdom.
--
-- /See:/ 'newFeedbackData' smart constructor.
data FeedbackData = FeedbackData'
  { -- | The relevance of the target this feedback is for.
    relevance :: Relevance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeedbackData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relevance', 'feedbackData_relevance' - The relevance of the target this feedback is for.
newFeedbackData ::
  -- | 'relevance'
  Relevance ->
  FeedbackData
newFeedbackData pRelevance_ =
  FeedbackData' {relevance = pRelevance_}

-- | The relevance of the target this feedback is for.
feedbackData_relevance :: Lens.Lens' FeedbackData Relevance
feedbackData_relevance = Lens.lens (\FeedbackData' {relevance} -> relevance) (\s@FeedbackData' {} a -> s {relevance = a} :: FeedbackData)

instance Core.FromJSON FeedbackData where
  parseJSON =
    Core.withObject
      "FeedbackData"
      ( \x ->
          FeedbackData' Prelude.<$> (x Core..: "relevance")
      )

instance Prelude.Hashable FeedbackData where
  hashWithSalt _salt FeedbackData' {..} =
    _salt `Prelude.hashWithSalt` relevance

instance Prelude.NFData FeedbackData where
  rnf FeedbackData' {..} = Prelude.rnf relevance

instance Core.ToJSON FeedbackData where
  toJSON FeedbackData' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("relevance" Core..= relevance)]
      )
