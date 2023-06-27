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
-- Module      : Amazonka.VoiceId.Types.RegistrationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.RegistrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.DuplicateRegistrationAction

-- | The registration configuration to be used during the batch fraudster
-- registration job.
--
-- /See:/ 'newRegistrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { -- | The action to take when a fraudster is identified as a duplicate. The
    -- default action is @SKIP@, which skips registering the duplicate
    -- fraudster. Setting the value to @REGISTER_AS_NEW@ always registers a new
    -- fraudster into the specified domain.
    duplicateRegistrationAction :: Prelude.Maybe DuplicateRegistrationAction,
    -- | The minimum similarity score between the new and old fraudsters in order
    -- to consider the new fraudster a duplicate.
    fraudsterSimilarityThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The identifiers of watchlists that a fraudster is registered to. If a
    -- watchlist isn\'t provided, the fraudsters are registered to the default
    -- watchlist.
    watchlistIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duplicateRegistrationAction', 'registrationConfig_duplicateRegistrationAction' - The action to take when a fraudster is identified as a duplicate. The
-- default action is @SKIP@, which skips registering the duplicate
-- fraudster. Setting the value to @REGISTER_AS_NEW@ always registers a new
-- fraudster into the specified domain.
--
-- 'fraudsterSimilarityThreshold', 'registrationConfig_fraudsterSimilarityThreshold' - The minimum similarity score between the new and old fraudsters in order
-- to consider the new fraudster a duplicate.
--
-- 'watchlistIds', 'registrationConfig_watchlistIds' - The identifiers of watchlists that a fraudster is registered to. If a
-- watchlist isn\'t provided, the fraudsters are registered to the default
-- watchlist.
newRegistrationConfig ::
  RegistrationConfig
newRegistrationConfig =
  RegistrationConfig'
    { duplicateRegistrationAction =
        Prelude.Nothing,
      fraudsterSimilarityThreshold = Prelude.Nothing,
      watchlistIds = Prelude.Nothing
    }

-- | The action to take when a fraudster is identified as a duplicate. The
-- default action is @SKIP@, which skips registering the duplicate
-- fraudster. Setting the value to @REGISTER_AS_NEW@ always registers a new
-- fraudster into the specified domain.
registrationConfig_duplicateRegistrationAction :: Lens.Lens' RegistrationConfig (Prelude.Maybe DuplicateRegistrationAction)
registrationConfig_duplicateRegistrationAction = Lens.lens (\RegistrationConfig' {duplicateRegistrationAction} -> duplicateRegistrationAction) (\s@RegistrationConfig' {} a -> s {duplicateRegistrationAction = a} :: RegistrationConfig)

-- | The minimum similarity score between the new and old fraudsters in order
-- to consider the new fraudster a duplicate.
registrationConfig_fraudsterSimilarityThreshold :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Natural)
registrationConfig_fraudsterSimilarityThreshold = Lens.lens (\RegistrationConfig' {fraudsterSimilarityThreshold} -> fraudsterSimilarityThreshold) (\s@RegistrationConfig' {} a -> s {fraudsterSimilarityThreshold = a} :: RegistrationConfig)

-- | The identifiers of watchlists that a fraudster is registered to. If a
-- watchlist isn\'t provided, the fraudsters are registered to the default
-- watchlist.
registrationConfig_watchlistIds :: Lens.Lens' RegistrationConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
registrationConfig_watchlistIds = Lens.lens (\RegistrationConfig' {watchlistIds} -> watchlistIds) (\s@RegistrationConfig' {} a -> s {watchlistIds = a} :: RegistrationConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RegistrationConfig where
  parseJSON =
    Data.withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            Prelude.<$> (x Data..:? "DuplicateRegistrationAction")
            Prelude.<*> (x Data..:? "FraudsterSimilarityThreshold")
            Prelude.<*> (x Data..:? "WatchlistIds")
      )

instance Prelude.Hashable RegistrationConfig where
  hashWithSalt _salt RegistrationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` duplicateRegistrationAction
      `Prelude.hashWithSalt` fraudsterSimilarityThreshold
      `Prelude.hashWithSalt` watchlistIds

instance Prelude.NFData RegistrationConfig where
  rnf RegistrationConfig' {..} =
    Prelude.rnf duplicateRegistrationAction
      `Prelude.seq` Prelude.rnf fraudsterSimilarityThreshold
      `Prelude.seq` Prelude.rnf watchlistIds

instance Data.ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DuplicateRegistrationAction" Data..=)
              Prelude.<$> duplicateRegistrationAction,
            ("FraudsterSimilarityThreshold" Data..=)
              Prelude.<$> fraudsterSimilarityThreshold,
            ("WatchlistIds" Data..=) Prelude.<$> watchlistIds
          ]
      )
