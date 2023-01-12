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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The date and time (in Unix time) when the reputation metrics were last
    -- given a fresh start. When your account is given a fresh start, your
    -- reputation metrics are calculated starting from the date of the fresh
    -- start.
    lastFreshStart :: Prelude.Maybe Data.POSIX,
    -- | If @true@, tracking of reputation metrics is enabled for the
    -- configuration set. If @false@, tracking of reputation metrics is
    -- disabled for the configuration set.
    reputationMetricsEnabled :: Prelude.Maybe Prelude.Bool
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
-- 'lastFreshStart', 'reputationOptions_lastFreshStart' - The date and time (in Unix time) when the reputation metrics were last
-- given a fresh start. When your account is given a fresh start, your
-- reputation metrics are calculated starting from the date of the fresh
-- start.
--
-- 'reputationMetricsEnabled', 'reputationOptions_reputationMetricsEnabled' - If @true@, tracking of reputation metrics is enabled for the
-- configuration set. If @false@, tracking of reputation metrics is
-- disabled for the configuration set.
newReputationOptions ::
  ReputationOptions
newReputationOptions =
  ReputationOptions'
    { lastFreshStart =
        Prelude.Nothing,
      reputationMetricsEnabled = Prelude.Nothing
    }

-- | The date and time (in Unix time) when the reputation metrics were last
-- given a fresh start. When your account is given a fresh start, your
-- reputation metrics are calculated starting from the date of the fresh
-- start.
reputationOptions_lastFreshStart :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.UTCTime)
reputationOptions_lastFreshStart = Lens.lens (\ReputationOptions' {lastFreshStart} -> lastFreshStart) (\s@ReputationOptions' {} a -> s {lastFreshStart = a} :: ReputationOptions) Prelude.. Lens.mapping Data._Time

-- | If @true@, tracking of reputation metrics is enabled for the
-- configuration set. If @false@, tracking of reputation metrics is
-- disabled for the configuration set.
reputationOptions_reputationMetricsEnabled :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.Bool)
reputationOptions_reputationMetricsEnabled = Lens.lens (\ReputationOptions' {reputationMetricsEnabled} -> reputationMetricsEnabled) (\s@ReputationOptions' {} a -> s {reputationMetricsEnabled = a} :: ReputationOptions)

instance Data.FromJSON ReputationOptions where
  parseJSON =
    Data.withObject
      "ReputationOptions"
      ( \x ->
          ReputationOptions'
            Prelude.<$> (x Data..:? "LastFreshStart")
            Prelude.<*> (x Data..:? "ReputationMetricsEnabled")
      )

instance Prelude.Hashable ReputationOptions where
  hashWithSalt _salt ReputationOptions' {..} =
    _salt `Prelude.hashWithSalt` lastFreshStart
      `Prelude.hashWithSalt` reputationMetricsEnabled

instance Prelude.NFData ReputationOptions where
  rnf ReputationOptions' {..} =
    Prelude.rnf lastFreshStart
      `Prelude.seq` Prelude.rnf reputationMetricsEnabled

instance Data.ToJSON ReputationOptions where
  toJSON ReputationOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LastFreshStart" Data..=)
              Prelude.<$> lastFreshStart,
            ("ReputationMetricsEnabled" Data..=)
              Prelude.<$> reputationMetricsEnabled
          ]
      )
