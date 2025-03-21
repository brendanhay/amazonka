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
-- Module      : Amazonka.Wisdom.Types.NotifyRecommendationsReceivedError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.NotifyRecommendationsReceivedError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An error occurred when creating a recommendation.
--
-- /See:/ 'newNotifyRecommendationsReceivedError' smart constructor.
data NotifyRecommendationsReceivedError = NotifyRecommendationsReceivedError'
  { -- | A recommendation is causing an error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the recommendation that is in error.
    recommendationId :: Prelude.Maybe Prelude.Text
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
-- 'message', 'notifyRecommendationsReceivedError_message' - A recommendation is causing an error.
--
-- 'recommendationId', 'notifyRecommendationsReceivedError_recommendationId' - The identifier of the recommendation that is in error.
newNotifyRecommendationsReceivedError ::
  NotifyRecommendationsReceivedError
newNotifyRecommendationsReceivedError =
  NotifyRecommendationsReceivedError'
    { message =
        Prelude.Nothing,
      recommendationId = Prelude.Nothing
    }

-- | A recommendation is causing an error.
notifyRecommendationsReceivedError_message :: Lens.Lens' NotifyRecommendationsReceivedError (Prelude.Maybe Prelude.Text)
notifyRecommendationsReceivedError_message = Lens.lens (\NotifyRecommendationsReceivedError' {message} -> message) (\s@NotifyRecommendationsReceivedError' {} a -> s {message = a} :: NotifyRecommendationsReceivedError)

-- | The identifier of the recommendation that is in error.
notifyRecommendationsReceivedError_recommendationId :: Lens.Lens' NotifyRecommendationsReceivedError (Prelude.Maybe Prelude.Text)
notifyRecommendationsReceivedError_recommendationId = Lens.lens (\NotifyRecommendationsReceivedError' {recommendationId} -> recommendationId) (\s@NotifyRecommendationsReceivedError' {} a -> s {recommendationId = a} :: NotifyRecommendationsReceivedError)

instance
  Data.FromJSON
    NotifyRecommendationsReceivedError
  where
  parseJSON =
    Data.withObject
      "NotifyRecommendationsReceivedError"
      ( \x ->
          NotifyRecommendationsReceivedError'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "recommendationId")
      )

instance
  Prelude.Hashable
    NotifyRecommendationsReceivedError
  where
  hashWithSalt
    _salt
    NotifyRecommendationsReceivedError' {..} =
      _salt
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` recommendationId

instance
  Prelude.NFData
    NotifyRecommendationsReceivedError
  where
  rnf NotifyRecommendationsReceivedError' {..} =
    Prelude.rnf message `Prelude.seq`
      Prelude.rnf recommendationId
