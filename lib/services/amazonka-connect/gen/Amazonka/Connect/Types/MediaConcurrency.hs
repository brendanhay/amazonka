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
-- Module      : Amazonka.Connect.Types.MediaConcurrency
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.MediaConcurrency where

import Amazonka.Connect.Types.Channel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about which channels are supported, and how many
-- contacts an agent can have on a channel simultaneously.
--
-- /See:/ 'newMediaConcurrency' smart constructor.
data MediaConcurrency = MediaConcurrency'
  { -- | The channels that agents can handle in the Contact Control Panel (CCP).
    channel :: Channel,
    -- | The number of contacts an agent can have on a channel simultaneously.
    --
    -- Valid Range for @VOICE@: Minimum value of 1. Maximum value of 1.
    --
    -- Valid Range for @CHAT@: Minimum value of 1. Maximum value of 10.
    --
    -- Valid Range for @TASK@: Minimum value of 1. Maximum value of 10.
    concurrency :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaConcurrency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'mediaConcurrency_channel' - The channels that agents can handle in the Contact Control Panel (CCP).
--
-- 'concurrency', 'mediaConcurrency_concurrency' - The number of contacts an agent can have on a channel simultaneously.
--
-- Valid Range for @VOICE@: Minimum value of 1. Maximum value of 1.
--
-- Valid Range for @CHAT@: Minimum value of 1. Maximum value of 10.
--
-- Valid Range for @TASK@: Minimum value of 1. Maximum value of 10.
newMediaConcurrency ::
  -- | 'channel'
  Channel ->
  -- | 'concurrency'
  Prelude.Natural ->
  MediaConcurrency
newMediaConcurrency pChannel_ pConcurrency_ =
  MediaConcurrency'
    { channel = pChannel_,
      concurrency = pConcurrency_
    }

-- | The channels that agents can handle in the Contact Control Panel (CCP).
mediaConcurrency_channel :: Lens.Lens' MediaConcurrency Channel
mediaConcurrency_channel = Lens.lens (\MediaConcurrency' {channel} -> channel) (\s@MediaConcurrency' {} a -> s {channel = a} :: MediaConcurrency)

-- | The number of contacts an agent can have on a channel simultaneously.
--
-- Valid Range for @VOICE@: Minimum value of 1. Maximum value of 1.
--
-- Valid Range for @CHAT@: Minimum value of 1. Maximum value of 10.
--
-- Valid Range for @TASK@: Minimum value of 1. Maximum value of 10.
mediaConcurrency_concurrency :: Lens.Lens' MediaConcurrency Prelude.Natural
mediaConcurrency_concurrency = Lens.lens (\MediaConcurrency' {concurrency} -> concurrency) (\s@MediaConcurrency' {} a -> s {concurrency = a} :: MediaConcurrency)

instance Core.FromJSON MediaConcurrency where
  parseJSON =
    Core.withObject
      "MediaConcurrency"
      ( \x ->
          MediaConcurrency'
            Prelude.<$> (x Core..: "Channel")
            Prelude.<*> (x Core..: "Concurrency")
      )

instance Prelude.Hashable MediaConcurrency where
  hashWithSalt _salt MediaConcurrency' {..} =
    _salt `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` concurrency

instance Prelude.NFData MediaConcurrency where
  rnf MediaConcurrency' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf concurrency

instance Core.ToJSON MediaConcurrency where
  toJSON MediaConcurrency' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Channel" Core..= channel),
            Prelude.Just ("Concurrency" Core..= concurrency)
          ]
      )
