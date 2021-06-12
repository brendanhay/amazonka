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
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettingsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettingsSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary configuration for a Multiplex event.
--
-- /See:/ 'newMultiplexSettingsSummary' smart constructor.
data MultiplexSettingsSummary = MultiplexSettingsSummary'
  { -- | Transport stream bit rate.
    transportStreamBitrate :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexSettingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transportStreamBitrate', 'multiplexSettingsSummary_transportStreamBitrate' - Transport stream bit rate.
newMultiplexSettingsSummary ::
  MultiplexSettingsSummary
newMultiplexSettingsSummary =
  MultiplexSettingsSummary'
    { transportStreamBitrate =
        Core.Nothing
    }

-- | Transport stream bit rate.
multiplexSettingsSummary_transportStreamBitrate :: Lens.Lens' MultiplexSettingsSummary (Core.Maybe Core.Natural)
multiplexSettingsSummary_transportStreamBitrate = Lens.lens (\MultiplexSettingsSummary' {transportStreamBitrate} -> transportStreamBitrate) (\s@MultiplexSettingsSummary' {} a -> s {transportStreamBitrate = a} :: MultiplexSettingsSummary)

instance Core.FromJSON MultiplexSettingsSummary where
  parseJSON =
    Core.withObject
      "MultiplexSettingsSummary"
      ( \x ->
          MultiplexSettingsSummary'
            Core.<$> (x Core..:? "transportStreamBitrate")
      )

instance Core.Hashable MultiplexSettingsSummary

instance Core.NFData MultiplexSettingsSummary
