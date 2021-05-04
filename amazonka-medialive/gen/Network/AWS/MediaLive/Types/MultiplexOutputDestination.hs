{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexOutputDestination where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
import qualified Network.AWS.Prelude as Prelude

-- | Multiplex output destination settings
--
-- /See:/ 'newMultiplexOutputDestination' smart constructor.
data MultiplexOutputDestination = MultiplexOutputDestination'
  { -- | Multiplex MediaConnect output destination settings.
    mediaConnectSettings :: Prelude.Maybe MultiplexMediaConnectOutputDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON MultiplexOutputDestination where
  parseJSON =
    Prelude.withObject
      "MultiplexOutputDestination"
      ( \x ->
          MultiplexOutputDestination'
            Prelude.<$> (x Prelude..:? "mediaConnectSettings")
      )

instance Prelude.Hashable MultiplexOutputDestination

instance Prelude.NFData MultiplexOutputDestination
