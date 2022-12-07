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
-- Module      : Amazonka.DevOpsGuru.Types.InsightFeedback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.InsightFeedback where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightFeedbackOption
import qualified Amazonka.Prelude as Prelude

-- | Information about insight feedback received from a customer.
--
-- /See:/ 'newInsightFeedback' smart constructor.
data InsightFeedback = InsightFeedback'
  { -- | The feedback provided by the customer.
    feedback :: Prelude.Maybe InsightFeedbackOption,
    -- | The insight feedback ID.
    id :: Prelude.Maybe Prelude.Text
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
-- 'feedback', 'insightFeedback_feedback' - The feedback provided by the customer.
--
-- 'id', 'insightFeedback_id' - The insight feedback ID.
newInsightFeedback ::
  InsightFeedback
newInsightFeedback =
  InsightFeedback'
    { feedback = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The feedback provided by the customer.
insightFeedback_feedback :: Lens.Lens' InsightFeedback (Prelude.Maybe InsightFeedbackOption)
insightFeedback_feedback = Lens.lens (\InsightFeedback' {feedback} -> feedback) (\s@InsightFeedback' {} a -> s {feedback = a} :: InsightFeedback)

-- | The insight feedback ID.
insightFeedback_id :: Lens.Lens' InsightFeedback (Prelude.Maybe Prelude.Text)
insightFeedback_id = Lens.lens (\InsightFeedback' {id} -> id) (\s@InsightFeedback' {} a -> s {id = a} :: InsightFeedback)

instance Data.FromJSON InsightFeedback where
  parseJSON =
    Data.withObject
      "InsightFeedback"
      ( \x ->
          InsightFeedback'
            Prelude.<$> (x Data..:? "Feedback")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable InsightFeedback where
  hashWithSalt _salt InsightFeedback' {..} =
    _salt `Prelude.hashWithSalt` feedback
      `Prelude.hashWithSalt` id

instance Prelude.NFData InsightFeedback where
  rnf InsightFeedback' {..} =
    Prelude.rnf feedback `Prelude.seq` Prelude.rnf id

instance Data.ToJSON InsightFeedback where
  toJSON InsightFeedback' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Feedback" Data..=) Prelude.<$> feedback,
            ("Id" Data..=) Prelude.<$> id
          ]
      )
