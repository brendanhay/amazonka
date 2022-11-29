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
-- Module      : Amazonka.MediaLive.Types.MultiplexOutputDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexOutputDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
import qualified Amazonka.Prelude as Prelude

-- | Multiplex output destination settings
--
-- /See:/ 'newMultiplexOutputDestination' smart constructor.
data MultiplexOutputDestination = MultiplexOutputDestination'
  { -- | Multiplex MediaConnect output destination settings.
    mediaConnectSettings :: Prelude.Maybe MultiplexMediaConnectOutputDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexOutputDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaConnectSettings', 'multiplexOutputDestination_mediaConnectSettings' - Multiplex MediaConnect output destination settings.
newMultiplexOutputDestination ::
  MultiplexOutputDestination
newMultiplexOutputDestination =
  MultiplexOutputDestination'
    { mediaConnectSettings =
        Prelude.Nothing
    }

-- | Multiplex MediaConnect output destination settings.
multiplexOutputDestination_mediaConnectSettings :: Lens.Lens' MultiplexOutputDestination (Prelude.Maybe MultiplexMediaConnectOutputDestinationSettings)
multiplexOutputDestination_mediaConnectSettings = Lens.lens (\MultiplexOutputDestination' {mediaConnectSettings} -> mediaConnectSettings) (\s@MultiplexOutputDestination' {} a -> s {mediaConnectSettings = a} :: MultiplexOutputDestination)

instance Core.FromJSON MultiplexOutputDestination where
  parseJSON =
    Core.withObject
      "MultiplexOutputDestination"
      ( \x ->
          MultiplexOutputDestination'
            Prelude.<$> (x Core..:? "mediaConnectSettings")
      )

instance Prelude.Hashable MultiplexOutputDestination where
  hashWithSalt _salt MultiplexOutputDestination' {..} =
    _salt `Prelude.hashWithSalt` mediaConnectSettings

instance Prelude.NFData MultiplexOutputDestination where
  rnf MultiplexOutputDestination' {..} =
    Prelude.rnf mediaConnectSettings
