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
-- Module      : Network.AWS.SESv2.Types.ReputationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.ReputationOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Enable or disable collection of reputation metrics for emails that you
-- send using this configuration set in the current Amazon Web Services
-- Region.
--
-- /See:/ 'newReputationOptions' smart constructor.
data ReputationOptions = ReputationOptions'
  { -- | The date and time (in Unix time) when the reputation metrics were last
    -- given a fresh start. When your account is given a fresh start, your
    -- reputation metrics are calculated starting from the date of the fresh
    -- start.
    lastFreshStart :: Prelude.Maybe Core.POSIX,
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
reputationOptions_lastFreshStart = Lens.lens (\ReputationOptions' {lastFreshStart} -> lastFreshStart) (\s@ReputationOptions' {} a -> s {lastFreshStart = a} :: ReputationOptions) Prelude.. Lens.mapping Core._Time

-- | If @true@, tracking of reputation metrics is enabled for the
-- configuration set. If @false@, tracking of reputation metrics is
-- disabled for the configuration set.
reputationOptions_reputationMetricsEnabled :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.Bool)
reputationOptions_reputationMetricsEnabled = Lens.lens (\ReputationOptions' {reputationMetricsEnabled} -> reputationMetricsEnabled) (\s@ReputationOptions' {} a -> s {reputationMetricsEnabled = a} :: ReputationOptions)

instance Core.FromJSON ReputationOptions where
  parseJSON =
    Core.withObject
      "ReputationOptions"
      ( \x ->
          ReputationOptions'
            Prelude.<$> (x Core..:? "LastFreshStart")
            Prelude.<*> (x Core..:? "ReputationMetricsEnabled")
      )

instance Prelude.Hashable ReputationOptions

instance Prelude.NFData ReputationOptions

instance Core.ToJSON ReputationOptions where
  toJSON ReputationOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LastFreshStart" Core..=)
              Prelude.<$> lastFreshStart,
            ("ReputationMetricsEnabled" Core..=)
              Prelude.<$> reputationMetricsEnabled
          ]
      )
