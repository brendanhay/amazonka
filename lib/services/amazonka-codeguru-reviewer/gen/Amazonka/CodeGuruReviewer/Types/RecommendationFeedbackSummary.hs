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
-- Module      : Amazonka.CodeGuruReviewer.Types.RecommendationFeedbackSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RecommendationFeedbackSummary where

import Amazonka.CodeGuruReviewer.Types.Reaction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about recommendation feedback summaries.
--
-- /See:/ 'newRecommendationFeedbackSummary' smart constructor.
data RecommendationFeedbackSummary = RecommendationFeedbackSummary'
  { -- | List for storing reactions. Reactions are utf-8 text code for emojis.
    reactions :: Prelude.Maybe [Reaction],
    -- | The recommendation ID that can be used to track the provided
    -- recommendations. Later on it can be used to collect the feedback.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user that gave the feedback.
    --
    -- The @UserId@ is an IAM principal that can be specified as an Amazon Web
    -- Services account ID or an Amazon Resource Name (ARN). For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
    -- in the /Amazon Web Services Identity and Access Management User Guide/.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationFeedbackSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reactions', 'recommendationFeedbackSummary_reactions' - List for storing reactions. Reactions are utf-8 text code for emojis.
--
-- 'recommendationId', 'recommendationFeedbackSummary_recommendationId' - The recommendation ID that can be used to track the provided
-- recommendations. Later on it can be used to collect the feedback.
--
-- 'userId', 'recommendationFeedbackSummary_userId' - The ID of the user that gave the feedback.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
newRecommendationFeedbackSummary ::
  RecommendationFeedbackSummary
newRecommendationFeedbackSummary =
  RecommendationFeedbackSummary'
    { reactions =
        Prelude.Nothing,
      recommendationId = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | List for storing reactions. Reactions are utf-8 text code for emojis.
recommendationFeedbackSummary_reactions :: Lens.Lens' RecommendationFeedbackSummary (Prelude.Maybe [Reaction])
recommendationFeedbackSummary_reactions = Lens.lens (\RecommendationFeedbackSummary' {reactions} -> reactions) (\s@RecommendationFeedbackSummary' {} a -> s {reactions = a} :: RecommendationFeedbackSummary) Prelude.. Lens.mapping Lens.coerced

-- | The recommendation ID that can be used to track the provided
-- recommendations. Later on it can be used to collect the feedback.
recommendationFeedbackSummary_recommendationId :: Lens.Lens' RecommendationFeedbackSummary (Prelude.Maybe Prelude.Text)
recommendationFeedbackSummary_recommendationId = Lens.lens (\RecommendationFeedbackSummary' {recommendationId} -> recommendationId) (\s@RecommendationFeedbackSummary' {} a -> s {recommendationId = a} :: RecommendationFeedbackSummary)

-- | The ID of the user that gave the feedback.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
recommendationFeedbackSummary_userId :: Lens.Lens' RecommendationFeedbackSummary (Prelude.Maybe Prelude.Text)
recommendationFeedbackSummary_userId = Lens.lens (\RecommendationFeedbackSummary' {userId} -> userId) (\s@RecommendationFeedbackSummary' {} a -> s {userId = a} :: RecommendationFeedbackSummary)

instance Data.FromJSON RecommendationFeedbackSummary where
  parseJSON =
    Data.withObject
      "RecommendationFeedbackSummary"
      ( \x ->
          RecommendationFeedbackSummary'
            Prelude.<$> (x Data..:? "Reactions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RecommendationId")
            Prelude.<*> (x Data..:? "UserId")
      )

instance
  Prelude.Hashable
    RecommendationFeedbackSummary
  where
  hashWithSalt _salt RecommendationFeedbackSummary' {..} =
    _salt `Prelude.hashWithSalt` reactions
      `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData RecommendationFeedbackSummary where
  rnf RecommendationFeedbackSummary' {..} =
    Prelude.rnf reactions
      `Prelude.seq` Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf userId
