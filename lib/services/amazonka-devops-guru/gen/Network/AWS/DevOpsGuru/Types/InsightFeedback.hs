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
-- Module      : Network.AWS.DevOpsGuru.Types.InsightFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.InsightFeedback where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.InsightFeedbackOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about insight feedback received from a customer.
--
-- /See:/ 'newInsightFeedback' smart constructor.
data InsightFeedback = InsightFeedback'
  { -- | The insight feedback ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The feedback provided by the customer.
    feedback :: Prelude.Maybe InsightFeedbackOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'insightFeedback_id' - The insight feedback ID.
--
-- 'feedback', 'insightFeedback_feedback' - The feedback provided by the customer.
newInsightFeedback ::
  InsightFeedback
newInsightFeedback =
  InsightFeedback'
    { id = Prelude.Nothing,
      feedback = Prelude.Nothing
    }

-- | The insight feedback ID.
insightFeedback_id :: Lens.Lens' InsightFeedback (Prelude.Maybe Prelude.Text)
insightFeedback_id = Lens.lens (\InsightFeedback' {id} -> id) (\s@InsightFeedback' {} a -> s {id = a} :: InsightFeedback)

-- | The feedback provided by the customer.
insightFeedback_feedback :: Lens.Lens' InsightFeedback (Prelude.Maybe InsightFeedbackOption)
insightFeedback_feedback = Lens.lens (\InsightFeedback' {feedback} -> feedback) (\s@InsightFeedback' {} a -> s {feedback = a} :: InsightFeedback)

instance Core.FromJSON InsightFeedback where
  parseJSON =
    Core.withObject
      "InsightFeedback"
      ( \x ->
          InsightFeedback'
            Prelude.<$> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Feedback")
      )

instance Prelude.Hashable InsightFeedback

instance Prelude.NFData InsightFeedback

instance Core.ToJSON InsightFeedback where
  toJSON InsightFeedback' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Id" Core..=) Prelude.<$> id,
            ("Feedback" Core..=) Prelude.<$> feedback
          ]
      )
