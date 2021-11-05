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
-- Module      : Network.AWS.Wisdom.Types.NotifyRecommendationsReceivedError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Wisdom.Types.NotifyRecommendationsReceivedError where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An error occurred when creating a recommendation.
--
-- /See:/ 'newNotifyRecommendationsReceivedError' smart constructor.
data NotifyRecommendationsReceivedError = NotifyRecommendationsReceivedError'
  { -- | The identifier of the recommendation that is in error.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | A recommendation is causing an error.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyRecommendationsReceivedError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationId', 'notifyRecommendationsReceivedError_recommendationId' - The identifier of the recommendation that is in error.
--
-- 'message', 'notifyRecommendationsReceivedError_message' - A recommendation is causing an error.
newNotifyRecommendationsReceivedError ::
  NotifyRecommendationsReceivedError
newNotifyRecommendationsReceivedError =
  NotifyRecommendationsReceivedError'
    { recommendationId =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The identifier of the recommendation that is in error.
notifyRecommendationsReceivedError_recommendationId :: Lens.Lens' NotifyRecommendationsReceivedError (Prelude.Maybe Prelude.Text)
notifyRecommendationsReceivedError_recommendationId = Lens.lens (\NotifyRecommendationsReceivedError' {recommendationId} -> recommendationId) (\s@NotifyRecommendationsReceivedError' {} a -> s {recommendationId = a} :: NotifyRecommendationsReceivedError)

-- | A recommendation is causing an error.
notifyRecommendationsReceivedError_message :: Lens.Lens' NotifyRecommendationsReceivedError (Prelude.Maybe Prelude.Text)
notifyRecommendationsReceivedError_message = Lens.lens (\NotifyRecommendationsReceivedError' {message} -> message) (\s@NotifyRecommendationsReceivedError' {} a -> s {message = a} :: NotifyRecommendationsReceivedError)

instance
  Core.FromJSON
    NotifyRecommendationsReceivedError
  where
  parseJSON =
    Core.withObject
      "NotifyRecommendationsReceivedError"
      ( \x ->
          NotifyRecommendationsReceivedError'
            Prelude.<$> (x Core..:? "recommendationId")
            Prelude.<*> (x Core..:? "message")
      )

instance
  Prelude.Hashable
    NotifyRecommendationsReceivedError

instance
  Prelude.NFData
    NotifyRecommendationsReceivedError
