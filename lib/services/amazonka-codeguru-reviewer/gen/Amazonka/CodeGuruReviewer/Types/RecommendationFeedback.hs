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
-- Module      : Amazonka.CodeGuruReviewer.Types.RecommendationFeedback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RecommendationFeedback where

import Amazonka.CodeGuruReviewer.Types.Reaction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the recommendation feedback.
--
-- /See:/ 'newRecommendationFeedback' smart constructor.
data RecommendationFeedback = RecommendationFeedback'
  { -- | The time at which the feedback was last updated.
    lastUpdatedTimeStamp :: Prelude.Maybe Core.POSIX,
    -- | List for storing reactions. Reactions are utf-8 text code for emojis.
    -- You can send an empty list to clear off all your feedback.
    reactions :: Prelude.Maybe [Reaction],
    -- | The time at which the feedback was created.
    createdTimeStamp :: Prelude.Maybe Core.POSIX,
    -- | The recommendation ID that can be used to track the provided
    -- recommendations. Later on it can be used to collect the feedback.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user that made the API call.
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
-- Create a value of 'RecommendationFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimeStamp', 'recommendationFeedback_lastUpdatedTimeStamp' - The time at which the feedback was last updated.
--
-- 'reactions', 'recommendationFeedback_reactions' - List for storing reactions. Reactions are utf-8 text code for emojis.
-- You can send an empty list to clear off all your feedback.
--
-- 'createdTimeStamp', 'recommendationFeedback_createdTimeStamp' - The time at which the feedback was created.
--
-- 'recommendationId', 'recommendationFeedback_recommendationId' - The recommendation ID that can be used to track the provided
-- recommendations. Later on it can be used to collect the feedback.
--
-- 'codeReviewArn', 'recommendationFeedback_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
--
-- 'userId', 'recommendationFeedback_userId' - The ID of the user that made the API call.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
newRecommendationFeedback ::
  RecommendationFeedback
newRecommendationFeedback =
  RecommendationFeedback'
    { lastUpdatedTimeStamp =
        Prelude.Nothing,
      reactions = Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing,
      recommendationId = Prelude.Nothing,
      codeReviewArn = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The time at which the feedback was last updated.
recommendationFeedback_lastUpdatedTimeStamp :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.UTCTime)
recommendationFeedback_lastUpdatedTimeStamp = Lens.lens (\RecommendationFeedback' {lastUpdatedTimeStamp} -> lastUpdatedTimeStamp) (\s@RecommendationFeedback' {} a -> s {lastUpdatedTimeStamp = a} :: RecommendationFeedback) Prelude.. Lens.mapping Core._Time

-- | List for storing reactions. Reactions are utf-8 text code for emojis.
-- You can send an empty list to clear off all your feedback.
recommendationFeedback_reactions :: Lens.Lens' RecommendationFeedback (Prelude.Maybe [Reaction])
recommendationFeedback_reactions = Lens.lens (\RecommendationFeedback' {reactions} -> reactions) (\s@RecommendationFeedback' {} a -> s {reactions = a} :: RecommendationFeedback) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the feedback was created.
recommendationFeedback_createdTimeStamp :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.UTCTime)
recommendationFeedback_createdTimeStamp = Lens.lens (\RecommendationFeedback' {createdTimeStamp} -> createdTimeStamp) (\s@RecommendationFeedback' {} a -> s {createdTimeStamp = a} :: RecommendationFeedback) Prelude.. Lens.mapping Core._Time

-- | The recommendation ID that can be used to track the provided
-- recommendations. Later on it can be used to collect the feedback.
recommendationFeedback_recommendationId :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.Text)
recommendationFeedback_recommendationId = Lens.lens (\RecommendationFeedback' {recommendationId} -> recommendationId) (\s@RecommendationFeedback' {} a -> s {recommendationId = a} :: RecommendationFeedback)

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
recommendationFeedback_codeReviewArn :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.Text)
recommendationFeedback_codeReviewArn = Lens.lens (\RecommendationFeedback' {codeReviewArn} -> codeReviewArn) (\s@RecommendationFeedback' {} a -> s {codeReviewArn = a} :: RecommendationFeedback)

-- | The ID of the user that made the API call.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
recommendationFeedback_userId :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.Text)
recommendationFeedback_userId = Lens.lens (\RecommendationFeedback' {userId} -> userId) (\s@RecommendationFeedback' {} a -> s {userId = a} :: RecommendationFeedback)

instance Core.FromJSON RecommendationFeedback where
  parseJSON =
    Core.withObject
      "RecommendationFeedback"
      ( \x ->
          RecommendationFeedback'
            Prelude.<$> (x Core..:? "LastUpdatedTimeStamp")
            Prelude.<*> (x Core..:? "Reactions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreatedTimeStamp")
            Prelude.<*> (x Core..:? "RecommendationId")
            Prelude.<*> (x Core..:? "CodeReviewArn")
            Prelude.<*> (x Core..:? "UserId")
      )

instance Prelude.Hashable RecommendationFeedback where
  hashWithSalt _salt RecommendationFeedback' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimeStamp
      `Prelude.hashWithSalt` reactions
      `Prelude.hashWithSalt` createdTimeStamp
      `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` codeReviewArn
      `Prelude.hashWithSalt` userId

instance Prelude.NFData RecommendationFeedback where
  rnf RecommendationFeedback' {..} =
    Prelude.rnf lastUpdatedTimeStamp
      `Prelude.seq` Prelude.rnf reactions
      `Prelude.seq` Prelude.rnf createdTimeStamp
      `Prelude.seq` Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf codeReviewArn
      `Prelude.seq` Prelude.rnf userId
