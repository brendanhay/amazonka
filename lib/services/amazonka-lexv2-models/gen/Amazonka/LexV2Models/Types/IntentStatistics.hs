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
-- Module      : Amazonka.LexV2Models.Types.IntentStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The object that contains the statistical summary of recommended intents
-- associated with the bot recommendation.
--
-- /See:/ 'newIntentStatistics' smart constructor.
data IntentStatistics = IntentStatistics'
  { -- | The number of recommended intents associated with the bot
    -- recommendation.
    discoveredIntentCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveredIntentCount', 'intentStatistics_discoveredIntentCount' - The number of recommended intents associated with the bot
-- recommendation.
newIntentStatistics ::
  IntentStatistics
newIntentStatistics =
  IntentStatistics'
    { discoveredIntentCount =
        Prelude.Nothing
    }

-- | The number of recommended intents associated with the bot
-- recommendation.
intentStatistics_discoveredIntentCount :: Lens.Lens' IntentStatistics (Prelude.Maybe Prelude.Int)
intentStatistics_discoveredIntentCount = Lens.lens (\IntentStatistics' {discoveredIntentCount} -> discoveredIntentCount) (\s@IntentStatistics' {} a -> s {discoveredIntentCount = a} :: IntentStatistics)

instance Data.FromJSON IntentStatistics where
  parseJSON =
    Data.withObject
      "IntentStatistics"
      ( \x ->
          IntentStatistics'
            Prelude.<$> (x Data..:? "discoveredIntentCount")
      )

instance Prelude.Hashable IntentStatistics where
  hashWithSalt _salt IntentStatistics' {..} =
    _salt `Prelude.hashWithSalt` discoveredIntentCount

instance Prelude.NFData IntentStatistics where
  rnf IntentStatistics' {..} =
    Prelude.rnf discoveredIntentCount
