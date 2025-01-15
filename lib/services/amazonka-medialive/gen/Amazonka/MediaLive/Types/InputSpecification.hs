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
-- Module      : Amazonka.MediaLive.Types.InputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputCodec
import Amazonka.MediaLive.Types.InputMaximumBitrate
import Amazonka.MediaLive.Types.InputResolution
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON InputSpecification where
  parseJSON =
    Data.withObject
      "InputSpecification"
      ( \x ->
          InputSpecification'
            Prelude.<$> (x Data..:? "codec")
            Prelude.<*> (x Data..:? "maximumBitrate")
            Prelude.<*> (x Data..:? "resolution")
      )

instance Prelude.Hashable InputSpecification where
  hashWithSalt _salt InputSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` maximumBitrate
      `Prelude.hashWithSalt` resolution

instance Prelude.NFData InputSpecification where
  rnf InputSpecification' {..} =
    Prelude.rnf codec `Prelude.seq`
      Prelude.rnf maximumBitrate `Prelude.seq`
        Prelude.rnf resolution

instance Data.ToJSON InputSpecification where
  toJSON InputSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("codec" Data..=) Prelude.<$> codec,
            ("maximumBitrate" Data..=)
              Prelude.<$> maximumBitrate,
            ("resolution" Data..=) Prelude.<$> resolution
          ]
      )
