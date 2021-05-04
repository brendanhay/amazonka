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
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexOutputSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import qualified Network.AWS.Prelude as Prelude

-- | Multiplex Output Settings
--
-- /See:/ 'newMultiplexOutputSettings' smart constructor.
data MultiplexOutputSettings = MultiplexOutputSettings'
  { -- | Destination is a Multiplex.
    destination :: OutputLocationRef
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MultiplexOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'multiplexOutputSettings_destination' - Destination is a Multiplex.
newMultiplexOutputSettings ::
  -- | 'destination'
  OutputLocationRef ->
  MultiplexOutputSettings
newMultiplexOutputSettings pDestination_ =
  MultiplexOutputSettings'
    { destination =
        pDestination_
    }

-- | Destination is a Multiplex.
multiplexOutputSettings_destination :: Lens.Lens' MultiplexOutputSettings OutputLocationRef
multiplexOutputSettings_destination = Lens.lens (\MultiplexOutputSettings' {destination} -> destination) (\s@MultiplexOutputSettings' {} a -> s {destination = a} :: MultiplexOutputSettings)

instance Prelude.FromJSON MultiplexOutputSettings where
  parseJSON =
    Prelude.withObject
      "MultiplexOutputSettings"
      ( \x ->
          MultiplexOutputSettings'
            Prelude.<$> (x Prelude..: "destination")
      )

instance Prelude.Hashable MultiplexOutputSettings

instance Prelude.NFData MultiplexOutputSettings

instance Prelude.ToJSON MultiplexOutputSettings where
  toJSON MultiplexOutputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("destination" Prelude..= destination)
          ]
      )
