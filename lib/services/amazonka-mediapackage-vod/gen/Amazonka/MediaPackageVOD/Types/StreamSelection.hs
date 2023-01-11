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
-- Module      : Amazonka.MediaPackageVOD.Types.StreamSelection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.StreamSelection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types.StreamOrder
import qualified Amazonka.Prelude as Prelude

-- | A StreamSelection configuration.
--
-- /See:/ 'newStreamSelection' smart constructor.
data StreamSelection = StreamSelection'
  { -- | The maximum video bitrate (bps) to include in output.
    maxVideoBitsPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The minimum video bitrate (bps) to include in output.
    minVideoBitsPerSecond :: Prelude.Maybe Prelude.Int,
    -- | A directive that determines the order of streams in the output.
    streamOrder :: Prelude.Maybe StreamOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxVideoBitsPerSecond', 'streamSelection_maxVideoBitsPerSecond' - The maximum video bitrate (bps) to include in output.
--
-- 'minVideoBitsPerSecond', 'streamSelection_minVideoBitsPerSecond' - The minimum video bitrate (bps) to include in output.
--
-- 'streamOrder', 'streamSelection_streamOrder' - A directive that determines the order of streams in the output.
newStreamSelection ::
  StreamSelection
newStreamSelection =
  StreamSelection'
    { maxVideoBitsPerSecond =
        Prelude.Nothing,
      minVideoBitsPerSecond = Prelude.Nothing,
      streamOrder = Prelude.Nothing
    }

-- | The maximum video bitrate (bps) to include in output.
streamSelection_maxVideoBitsPerSecond :: Lens.Lens' StreamSelection (Prelude.Maybe Prelude.Int)
streamSelection_maxVideoBitsPerSecond = Lens.lens (\StreamSelection' {maxVideoBitsPerSecond} -> maxVideoBitsPerSecond) (\s@StreamSelection' {} a -> s {maxVideoBitsPerSecond = a} :: StreamSelection)

-- | The minimum video bitrate (bps) to include in output.
streamSelection_minVideoBitsPerSecond :: Lens.Lens' StreamSelection (Prelude.Maybe Prelude.Int)
streamSelection_minVideoBitsPerSecond = Lens.lens (\StreamSelection' {minVideoBitsPerSecond} -> minVideoBitsPerSecond) (\s@StreamSelection' {} a -> s {minVideoBitsPerSecond = a} :: StreamSelection)

-- | A directive that determines the order of streams in the output.
streamSelection_streamOrder :: Lens.Lens' StreamSelection (Prelude.Maybe StreamOrder)
streamSelection_streamOrder = Lens.lens (\StreamSelection' {streamOrder} -> streamOrder) (\s@StreamSelection' {} a -> s {streamOrder = a} :: StreamSelection)

instance Data.FromJSON StreamSelection where
  parseJSON =
    Data.withObject
      "StreamSelection"
      ( \x ->
          StreamSelection'
            Prelude.<$> (x Data..:? "maxVideoBitsPerSecond")
            Prelude.<*> (x Data..:? "minVideoBitsPerSecond")
            Prelude.<*> (x Data..:? "streamOrder")
      )

instance Prelude.Hashable StreamSelection where
  hashWithSalt _salt StreamSelection' {..} =
    _salt `Prelude.hashWithSalt` maxVideoBitsPerSecond
      `Prelude.hashWithSalt` minVideoBitsPerSecond
      `Prelude.hashWithSalt` streamOrder

instance Prelude.NFData StreamSelection where
  rnf StreamSelection' {..} =
    Prelude.rnf maxVideoBitsPerSecond
      `Prelude.seq` Prelude.rnf minVideoBitsPerSecond
      `Prelude.seq` Prelude.rnf streamOrder

instance Data.ToJSON StreamSelection where
  toJSON StreamSelection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxVideoBitsPerSecond" Data..=)
              Prelude.<$> maxVideoBitsPerSecond,
            ("minVideoBitsPerSecond" Data..=)
              Prelude.<$> minVideoBitsPerSecond,
            ("streamOrder" Data..=) Prelude.<$> streamOrder
          ]
      )
