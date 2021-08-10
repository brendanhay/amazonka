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
-- Module      : Network.AWS.MediaPackage.Types.StreamSelection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.StreamSelection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.StreamOrder
import qualified Network.AWS.Prelude as Prelude

-- | A StreamSelection configuration.
--
-- /See:/ 'newStreamSelection' smart constructor.
data StreamSelection = StreamSelection'
  { -- | The minimum video bitrate (bps) to include in output.
    minVideoBitsPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The maximum video bitrate (bps) to include in output.
    maxVideoBitsPerSecond :: Prelude.Maybe Prelude.Int,
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
-- 'minVideoBitsPerSecond', 'streamSelection_minVideoBitsPerSecond' - The minimum video bitrate (bps) to include in output.
--
-- 'maxVideoBitsPerSecond', 'streamSelection_maxVideoBitsPerSecond' - The maximum video bitrate (bps) to include in output.
--
-- 'streamOrder', 'streamSelection_streamOrder' - A directive that determines the order of streams in the output.
newStreamSelection ::
  StreamSelection
newStreamSelection =
  StreamSelection'
    { minVideoBitsPerSecond =
        Prelude.Nothing,
      maxVideoBitsPerSecond = Prelude.Nothing,
      streamOrder = Prelude.Nothing
    }

-- | The minimum video bitrate (bps) to include in output.
streamSelection_minVideoBitsPerSecond :: Lens.Lens' StreamSelection (Prelude.Maybe Prelude.Int)
streamSelection_minVideoBitsPerSecond = Lens.lens (\StreamSelection' {minVideoBitsPerSecond} -> minVideoBitsPerSecond) (\s@StreamSelection' {} a -> s {minVideoBitsPerSecond = a} :: StreamSelection)

-- | The maximum video bitrate (bps) to include in output.
streamSelection_maxVideoBitsPerSecond :: Lens.Lens' StreamSelection (Prelude.Maybe Prelude.Int)
streamSelection_maxVideoBitsPerSecond = Lens.lens (\StreamSelection' {maxVideoBitsPerSecond} -> maxVideoBitsPerSecond) (\s@StreamSelection' {} a -> s {maxVideoBitsPerSecond = a} :: StreamSelection)

-- | A directive that determines the order of streams in the output.
streamSelection_streamOrder :: Lens.Lens' StreamSelection (Prelude.Maybe StreamOrder)
streamSelection_streamOrder = Lens.lens (\StreamSelection' {streamOrder} -> streamOrder) (\s@StreamSelection' {} a -> s {streamOrder = a} :: StreamSelection)

instance Core.FromJSON StreamSelection where
  parseJSON =
    Core.withObject
      "StreamSelection"
      ( \x ->
          StreamSelection'
            Prelude.<$> (x Core..:? "minVideoBitsPerSecond")
            Prelude.<*> (x Core..:? "maxVideoBitsPerSecond")
            Prelude.<*> (x Core..:? "streamOrder")
      )

instance Prelude.Hashable StreamSelection

instance Prelude.NFData StreamSelection

instance Core.ToJSON StreamSelection where
  toJSON StreamSelection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("minVideoBitsPerSecond" Core..=)
              Prelude.<$> minVideoBitsPerSecond,
            ("maxVideoBitsPerSecond" Core..=)
              Prelude.<$> maxVideoBitsPerSecond,
            ("streamOrder" Core..=) Prelude.<$> streamOrder
          ]
      )
