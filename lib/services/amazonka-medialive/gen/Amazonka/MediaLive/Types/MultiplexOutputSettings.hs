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
-- Module      : Amazonka.MediaLive.Types.MultiplexOutputSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.OutputLocationRef
import qualified Amazonka.Prelude as Prelude

-- | Multiplex Output Settings
--
-- /See:/ 'newMultiplexOutputSettings' smart constructor.
data MultiplexOutputSettings = MultiplexOutputSettings'
  { -- | Destination is a Multiplex.
    destination :: OutputLocationRef
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON MultiplexOutputSettings where
  parseJSON =
    Data.withObject
      "MultiplexOutputSettings"
      ( \x ->
          MultiplexOutputSettings'
            Prelude.<$> (x Data..: "destination")
      )

instance Prelude.Hashable MultiplexOutputSettings where
  hashWithSalt _salt MultiplexOutputSettings' {..} =
    _salt `Prelude.hashWithSalt` destination

instance Prelude.NFData MultiplexOutputSettings where
  rnf MultiplexOutputSettings' {..} =
    Prelude.rnf destination

instance Data.ToJSON MultiplexOutputSettings where
  toJSON MultiplexOutputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("destination" Data..= destination)]
      )
