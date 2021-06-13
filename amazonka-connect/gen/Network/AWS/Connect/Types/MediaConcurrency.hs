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
-- Module      : Network.AWS.Connect.Types.MediaConcurrency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.MediaConcurrency where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about which channels are supported, and how many
-- contacts an agent can have on a channel simultaneously.
--
-- /See:/ 'newMediaConcurrency' smart constructor.
data MediaConcurrency = MediaConcurrency'
  { -- | The channels that agents can handle in the Contact Control Panel (CCP).
    channel :: Channel,
    -- | The number of contacts an agent can have on a channel simultaneously.
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

instance Prelude.Hashable MediaConcurrency

instance Prelude.NFData MediaConcurrency

instance Core.ToJSON MediaConcurrency where
  toJSON MediaConcurrency' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Channel" Core..= channel),
            Prelude.Just ("Concurrency" Core..= concurrency)
          ]
      )
