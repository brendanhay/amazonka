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
-- Module      : Network.AWS.MediaConvert.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbTdtSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition
-- interval.
--
-- /See:/ 'newDvbTdtSettings' smart constructor.
data DvbTdtSettings = DvbTdtSettings'
  { -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    tdtInterval :: Core.Maybe Core.Natural
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
-- 'tdtInterval', 'dvbTdtSettings_tdtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
newDvbTdtSettings ::
  DvbTdtSettings
newDvbTdtSettings =
  DvbTdtSettings' {tdtInterval = Core.Nothing}

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
dvbTdtSettings_tdtInterval :: Lens.Lens' DvbTdtSettings (Core.Maybe Core.Natural)
dvbTdtSettings_tdtInterval = Lens.lens (\DvbTdtSettings' {tdtInterval} -> tdtInterval) (\s@DvbTdtSettings' {} a -> s {tdtInterval = a} :: DvbTdtSettings)

instance Core.FromJSON DvbTdtSettings where
  parseJSON =
    Core.withObject
      "DvbTdtSettings"
      ( \x ->
          DvbTdtSettings' Core.<$> (x Core..:? "tdtInterval")
      )

instance Core.Hashable DvbTdtSettings

instance Core.NFData DvbTdtSettings

instance Core.ToJSON DvbTdtSettings where
  toJSON DvbTdtSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("tdtInterval" Core..=) Core.<$> tdtInterval]
      )
