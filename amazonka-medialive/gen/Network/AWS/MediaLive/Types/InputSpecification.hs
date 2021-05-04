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
-- Module      : Network.AWS.MediaLive.Types.InputSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSpecification where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputCodec
import Network.AWS.MediaLive.Types.InputMaximumBitrate
import Network.AWS.MediaLive.Types.InputResolution
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for InputSpecification
--
-- /See:/ 'newInputSpecification' smart constructor.
data InputSpecification = InputSpecification'
  { -- | Input codec
    codec :: Prelude.Maybe InputCodec,
    -- | Maximum input bitrate, categorized coarsely
    maximumBitrate :: Prelude.Maybe InputMaximumBitrate,
    -- | Input resolution, categorized coarsely
    resolution :: Prelude.Maybe InputResolution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codec', 'inputSpecification_codec' - Input codec
--
-- 'maximumBitrate', 'inputSpecification_maximumBitrate' - Maximum input bitrate, categorized coarsely
--
-- 'resolution', 'inputSpecification_resolution' - Input resolution, categorized coarsely
newInputSpecification ::
  InputSpecification
newInputSpecification =
  InputSpecification'
    { codec = Prelude.Nothing,
      maximumBitrate = Prelude.Nothing,
      resolution = Prelude.Nothing
    }

-- | Input codec
inputSpecification_codec :: Lens.Lens' InputSpecification (Prelude.Maybe InputCodec)
inputSpecification_codec = Lens.lens (\InputSpecification' {codec} -> codec) (\s@InputSpecification' {} a -> s {codec = a} :: InputSpecification)

-- | Maximum input bitrate, categorized coarsely
inputSpecification_maximumBitrate :: Lens.Lens' InputSpecification (Prelude.Maybe InputMaximumBitrate)
inputSpecification_maximumBitrate = Lens.lens (\InputSpecification' {maximumBitrate} -> maximumBitrate) (\s@InputSpecification' {} a -> s {maximumBitrate = a} :: InputSpecification)

-- | Input resolution, categorized coarsely
inputSpecification_resolution :: Lens.Lens' InputSpecification (Prelude.Maybe InputResolution)
inputSpecification_resolution = Lens.lens (\InputSpecification' {resolution} -> resolution) (\s@InputSpecification' {} a -> s {resolution = a} :: InputSpecification)

instance Prelude.FromJSON InputSpecification where
  parseJSON =
    Prelude.withObject
      "InputSpecification"
      ( \x ->
          InputSpecification'
            Prelude.<$> (x Prelude..:? "codec")
            Prelude.<*> (x Prelude..:? "maximumBitrate")
            Prelude.<*> (x Prelude..:? "resolution")
      )

instance Prelude.Hashable InputSpecification

instance Prelude.NFData InputSpecification

instance Prelude.ToJSON InputSpecification where
  toJSON InputSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("codec" Prelude..=) Prelude.<$> codec,
            ("maximumBitrate" Prelude..=)
              Prelude.<$> maximumBitrate,
            ("resolution" Prelude..=) Prelude.<$> resolution
          ]
      )
