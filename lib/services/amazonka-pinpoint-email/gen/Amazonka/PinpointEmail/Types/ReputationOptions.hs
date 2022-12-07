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
-- Module      : Amazonka.PinpointEmail.Types.ReputationOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.ReputationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enable or disable collection of reputation metrics for emails that you
-- send using this configuration set in the current AWS Region.
--
-- /See:/ 'newReputationOptions' smart constructor.
data ReputationOptions = ReputationOptions'
  { -- | If @true@, tracking of reputation metrics is enabled for the
    -- configuration set. If @false@, tracking of reputation metrics is
    -- disabled for the configuration set.
    reputationMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The date and time (in Unix time) when the reputation metrics were last
    -- given a fresh start. When your account is given a fresh start, your
    -- reputation metrics are calculated starting from the date of the fresh
    -- start.
    lastFreshStart :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReputationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reputationMetricsEnabled', 'reputationOptions_reputationMetricsEnabled' - If @true@, tracking of reputation metrics is enabled for the
-- configuration set. If @false@, tracking of reputation metrics is
-- disabled for the configuration set.
--
-- 'lastFreshStart', 'reputationOptions_lastFreshStart' - The date and time (in Unix time) when the reputation metrics were last
-- given a fresh start. When your account is given a fresh start, your
-- reputation metrics are calculated starting from the date of the fresh
-- start.
newReputationOptions ::
  ReputationOptions
newReputationOptions =
  ReputationOptions'
    { reputationMetricsEnabled =
        Prelude.Nothing,
      lastFreshStart = Prelude.Nothing
    }

-- | If @true@, tracking of reputation metrics is enabled for the
-- configuration set. If @false@, tracking of reputation metrics is
-- disabled for the configuration set.
reputationOptions_reputationMetricsEnabled :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.Bool)
reputationOptions_reputationMetricsEnabled = Lens.lens (\ReputationOptions' {reputationMetricsEnabled} -> reputationMetricsEnabled) (\s@ReputationOptions' {} a -> s {reputationMetricsEnabled = a} :: ReputationOptions)

-- | The date and time (in Unix time) when the reputation metrics were last
-- given a fresh start. When your account is given a fresh start, your
-- reputation metrics are calculated starting from the date of the fresh
-- start.
reputationOptions_lastFreshStart :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.UTCTime)
reputationOptions_lastFreshStart = Lens.lens (\ReputationOptions' {lastFreshStart} -> lastFreshStart) (\s@ReputationOptions' {} a -> s {lastFreshStart = a} :: ReputationOptions) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ReputationOptions where
  parseJSON =
    Data.withObject
      "ReputationOptions"
      ( \x ->
          ReputationOptions'
            Prelude.<$> (x Data..:? "ReputationMetricsEnabled")
            Prelude.<*> (x Data..:? "LastFreshStart")
      )

instance Prelude.Hashable ReputationOptions where
  hashWithSalt _salt ReputationOptions' {..} =
    _salt
      `Prelude.hashWithSalt` reputationMetricsEnabled
      `Prelude.hashWithSalt` lastFreshStart

instance Prelude.NFData ReputationOptions where
  rnf ReputationOptions' {..} =
    Prelude.rnf reputationMetricsEnabled
      `Prelude.seq` Prelude.rnf lastFreshStart

instance Data.ToJSON ReputationOptions where
  toJSON ReputationOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReputationMetricsEnabled" Data..=)
              Prelude.<$> reputationMetricsEnabled,
            ("LastFreshStart" Data..=)
              Prelude.<$> lastFreshStart
          ]
      )
