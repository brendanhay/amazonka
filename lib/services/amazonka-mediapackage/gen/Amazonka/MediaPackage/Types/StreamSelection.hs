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
-- Module      : Amazonka.MediaPackage.Types.StreamSelection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.StreamSelection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackage.Types.StreamOrder
import qualified Amazonka.Prelude as Prelude

-- | A StreamSelection configuration.
--
-- /See:/ 'newStreamSelection' smart constructor.
data StreamSelection = StreamSelection'
  { -- | A directive that determines the order of streams in the output.
    streamOrder :: Prelude.Maybe StreamOrder,
    -- | The minimum video bitrate (bps) to include in output.
    minVideoBitsPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The maximum video bitrate (bps) to include in output.
    maxVideoBitsPerSecond :: Prelude.Maybe Prelude.Int
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
-- 'streamOrder', 'streamSelection_streamOrder' - A directive that determines the order of streams in the output.
--
-- 'minVideoBitsPerSecond', 'streamSelection_minVideoBitsPerSecond' - The minimum video bitrate (bps) to include in output.
--
-- 'maxVideoBitsPerSecond', 'streamSelection_maxVideoBitsPerSecond' - The maximum video bitrate (bps) to include in output.
newStreamSelection ::
  StreamSelection
newStreamSelection =
  StreamSelection'
    { streamOrder = Prelude.Nothing,
      minVideoBitsPerSecond = Prelude.Nothing,
      maxVideoBitsPerSecond = Prelude.Nothing
    }

-- | A directive that determines the order of streams in the output.
streamSelection_streamOrder :: Lens.Lens' StreamSelection (Prelude.Maybe StreamOrder)
streamSelection_streamOrder = Lens.lens (\StreamSelection' {streamOrder} -> streamOrder) (\s@StreamSelection' {} a -> s {streamOrder = a} :: StreamSelection)

-- | The minimum video bitrate (bps) to include in output.
streamSelection_minVideoBitsPerSecond :: Lens.Lens' StreamSelection (Prelude.Maybe Prelude.Int)
streamSelection_minVideoBitsPerSecond = Lens.lens (\StreamSelection' {minVideoBitsPerSecond} -> minVideoBitsPerSecond) (\s@StreamSelection' {} a -> s {minVideoBitsPerSecond = a} :: StreamSelection)

-- | The maximum video bitrate (bps) to include in output.
streamSelection_maxVideoBitsPerSecond :: Lens.Lens' StreamSelection (Prelude.Maybe Prelude.Int)
streamSelection_maxVideoBitsPerSecond = Lens.lens (\StreamSelection' {maxVideoBitsPerSecond} -> maxVideoBitsPerSecond) (\s@StreamSelection' {} a -> s {maxVideoBitsPerSecond = a} :: StreamSelection)

instance Core.FromJSON StreamSelection where
  parseJSON =
    Core.withObject
      "StreamSelection"
      ( \x ->
          StreamSelection'
            Prelude.<$> (x Core..:? "streamOrder")
            Prelude.<*> (x Core..:? "minVideoBitsPerSecond")
            Prelude.<*> (x Core..:? "maxVideoBitsPerSecond")
      )

instance Prelude.Hashable StreamSelection where
  hashWithSalt _salt StreamSelection' {..} =
    _salt `Prelude.hashWithSalt` streamOrder
      `Prelude.hashWithSalt` minVideoBitsPerSecond
      `Prelude.hashWithSalt` maxVideoBitsPerSecond

instance Prelude.NFData StreamSelection where
  rnf StreamSelection' {..} =
    Prelude.rnf streamOrder
      `Prelude.seq` Prelude.rnf minVideoBitsPerSecond
      `Prelude.seq` Prelude.rnf maxVideoBitsPerSecond

instance Core.ToJSON StreamSelection where
  toJSON StreamSelection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("streamOrder" Core..=) Prelude.<$> streamOrder,
            ("minVideoBitsPerSecond" Core..=)
              Prelude.<$> minVideoBitsPerSecond,
            ("maxVideoBitsPerSecond" Core..=)
              Prelude.<$> maxVideoBitsPerSecond
          ]
      )
