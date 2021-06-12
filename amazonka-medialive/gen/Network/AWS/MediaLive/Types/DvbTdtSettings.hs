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
-- Module      : Network.AWS.MediaLive.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbTdtSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | DVB Time and Date Table (SDT)
--
-- /See:/ 'newDvbTdtSettings' smart constructor.
data DvbTdtSettings = DvbTdtSettings'
  { -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    repInterval :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DvbTdtSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repInterval', 'dvbTdtSettings_repInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
newDvbTdtSettings ::
  DvbTdtSettings
newDvbTdtSettings =
  DvbTdtSettings' {repInterval = Core.Nothing}

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
dvbTdtSettings_repInterval :: Lens.Lens' DvbTdtSettings (Core.Maybe Core.Natural)
dvbTdtSettings_repInterval = Lens.lens (\DvbTdtSettings' {repInterval} -> repInterval) (\s@DvbTdtSettings' {} a -> s {repInterval = a} :: DvbTdtSettings)

instance Core.FromJSON DvbTdtSettings where
  parseJSON =
    Core.withObject
      "DvbTdtSettings"
      ( \x ->
          DvbTdtSettings' Core.<$> (x Core..:? "repInterval")
      )

instance Core.Hashable DvbTdtSettings

instance Core.NFData DvbTdtSettings

instance Core.ToJSON DvbTdtSettings where
  toJSON DvbTdtSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("repInterval" Core..=) Core.<$> repInterval]
      )
