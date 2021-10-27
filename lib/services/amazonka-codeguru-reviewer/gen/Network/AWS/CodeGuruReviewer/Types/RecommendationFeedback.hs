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
-- Module      : Network.AWS.CodeGuruReviewer.Types.RecommendationFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruReviewer.Types.RecommendationFeedback where

import Network.AWS.CodeGuruReviewer.Types.Reaction
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the recommendation feedback.
--
-- /See:/ 'newRecommendationFeedback' smart constructor.
data RecommendationFeedback = RecommendationFeedback'
  { -- | The recommendation ID that can be used to track the provided
    -- recommendations. Later on it can be used to collect the feedback.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user that made the API call.
    --
    -- The @UserId@ is an IAM principal that can be specified as an Amazon Web
    -- Services account ID or an Amazon Resource Name (ARN). For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
    -- in the /Amazon Web Services Identity and Access Management User Guide/.
    userId :: Prelude.Maybe Prelude.Text,
    -- | List for storing reactions. Reactions are utf-8 text code for emojis.
    -- You can send an empty list to clear off all your feedback.
    reactions :: Prelude.Maybe [Reaction],
    -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the feedback was created.
    createdTimeStamp :: Prelude.Maybe Core.POSIX,
    -- | The time at which the feedback was last updated.
    lastUpdatedTimeStamp :: Prelude.Maybe Core.POSIX
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
-- 'recommendationId', 'recommendationFeedback_recommendationId' - The recommendation ID that can be used to track the provided
-- recommendations. Later on it can be used to collect the feedback.
--
-- 'userId', 'recommendationFeedback_userId' - The ID of the user that made the API call.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
--
-- 'reactions', 'recommendationFeedback_reactions' - List for storing reactions. Reactions are utf-8 text code for emojis.
-- You can send an empty list to clear off all your feedback.
--
-- 'codeReviewArn', 'recommendationFeedback_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
--
-- 'createdTimeStamp', 'recommendationFeedback_createdTimeStamp' - The time at which the feedback was created.
--
-- 'lastUpdatedTimeStamp', 'recommendationFeedback_lastUpdatedTimeStamp' - The time at which the feedback was last updated.
newRecommendationFeedback ::
  RecommendationFeedback
newRecommendationFeedback =
  RecommendationFeedback'
    { recommendationId =
        Prelude.Nothing,
      userId = Prelude.Nothing,
      reactions = Prelude.Nothing,
      codeReviewArn = Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing,
      lastUpdatedTimeStamp = Prelude.Nothing
    }

-- | The recommendation ID that can be used to track the provided
-- recommendations. Later on it can be used to collect the feedback.
recommendationFeedback_recommendationId :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.Text)
recommendationFeedback_recommendationId = Lens.lens (\RecommendationFeedback' {recommendationId} -> recommendationId) (\s@RecommendationFeedback' {} a -> s {recommendationId = a} :: RecommendationFeedback)

-- | The ID of the user that made the API call.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
recommendationFeedback_userId :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.Text)
recommendationFeedback_userId = Lens.lens (\RecommendationFeedback' {userId} -> userId) (\s@RecommendationFeedback' {} a -> s {userId = a} :: RecommendationFeedback)

-- | List for storing reactions. Reactions are utf-8 text code for emojis.
-- You can send an empty list to clear off all your feedback.
recommendationFeedback_reactions :: Lens.Lens' RecommendationFeedback (Prelude.Maybe [Reaction])
recommendationFeedback_reactions = Lens.lens (\RecommendationFeedback' {reactions} -> reactions) (\s@RecommendationFeedback' {} a -> s {reactions = a} :: RecommendationFeedback) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
recommendationFeedback_codeReviewArn :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.Text)
recommendationFeedback_codeReviewArn = Lens.lens (\RecommendationFeedback' {codeReviewArn} -> codeReviewArn) (\s@RecommendationFeedback' {} a -> s {codeReviewArn = a} :: RecommendationFeedback)

-- | The time at which the feedback was created.
recommendationFeedback_createdTimeStamp :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.UTCTime)
recommendationFeedback_createdTimeStamp = Lens.lens (\RecommendationFeedback' {createdTimeStamp} -> createdTimeStamp) (\s@RecommendationFeedback' {} a -> s {createdTimeStamp = a} :: RecommendationFeedback) Prelude.. Lens.mapping Core._Time

-- | The time at which the feedback was last updated.
recommendationFeedback_lastUpdatedTimeStamp :: Lens.Lens' RecommendationFeedback (Prelude.Maybe Prelude.UTCTime)
recommendationFeedback_lastUpdatedTimeStamp = Lens.lens (\RecommendationFeedback' {lastUpdatedTimeStamp} -> lastUpdatedTimeStamp) (\s@RecommendationFeedback' {} a -> s {lastUpdatedTimeStamp = a} :: RecommendationFeedback) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON RecommendationFeedback where
  parseJSON =
    Core.withObject
      "RecommendationFeedback"
      ( \x ->
          RecommendationFeedback'
            Prelude.<$> (x Core..:? "RecommendationId")
            Prelude.<*> (x Core..:? "UserId")
            Prelude.<*> (x Core..:? "Reactions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CodeReviewArn")
            Prelude.<*> (x Core..:? "CreatedTimeStamp")
            Prelude.<*> (x Core..:? "LastUpdatedTimeStamp")
      )

instance Prelude.Hashable RecommendationFeedback

instance Prelude.NFData RecommendationFeedback
