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
-- Module      : Amazonka.MediaLive.Types.InputChannelLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputChannelLevel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Input Channel Level
--
-- /See:/ 'newInputChannelLevel' smart constructor.
data InputChannelLevel = InputChannelLevel'
  { -- | The index of the input channel used as a source.
    inputChannel :: Prelude.Natural,
    -- | Remixing value. Units are in dB and acceptable values are within the
    -- range from -60 (mute) and 6 dB.
    gain :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputChannelLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputChannel', 'inputChannelLevel_inputChannel' - The index of the input channel used as a source.
--
-- 'gain', 'inputChannelLevel_gain' - Remixing value. Units are in dB and acceptable values are within the
-- range from -60 (mute) and 6 dB.
newInputChannelLevel ::
  -- | 'inputChannel'
  Prelude.Natural ->
  -- | 'gain'
  Prelude.Int ->
  InputChannelLevel
newInputChannelLevel pInputChannel_ pGain_ =
  InputChannelLevel'
    { inputChannel = pInputChannel_,
      gain = pGain_
    }

-- | The index of the input channel used as a source.
inputChannelLevel_inputChannel :: Lens.Lens' InputChannelLevel Prelude.Natural
inputChannelLevel_inputChannel = Lens.lens (\InputChannelLevel' {inputChannel} -> inputChannel) (\s@InputChannelLevel' {} a -> s {inputChannel = a} :: InputChannelLevel)

-- | Remixing value. Units are in dB and acceptable values are within the
-- range from -60 (mute) and 6 dB.
inputChannelLevel_gain :: Lens.Lens' InputChannelLevel Prelude.Int
inputChannelLevel_gain = Lens.lens (\InputChannelLevel' {gain} -> gain) (\s@InputChannelLevel' {} a -> s {gain = a} :: InputChannelLevel)

instance Core.FromJSON InputChannelLevel where
  parseJSON =
    Core.withObject
      "InputChannelLevel"
      ( \x ->
          InputChannelLevel'
            Prelude.<$> (x Core..: "inputChannel")
            Prelude.<*> (x Core..: "gain")
      )

instance Prelude.Hashable InputChannelLevel where
  hashWithSalt _salt InputChannelLevel' {..} =
    _salt `Prelude.hashWithSalt` inputChannel
      `Prelude.hashWithSalt` gain

instance Prelude.NFData InputChannelLevel where
  rnf InputChannelLevel' {..} =
    Prelude.rnf inputChannel
      `Prelude.seq` Prelude.rnf gain

instance Core.ToJSON InputChannelLevel where
  toJSON InputChannelLevel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("inputChannel" Core..= inputChannel),
            Prelude.Just ("gain" Core..= gain)
          ]
      )
