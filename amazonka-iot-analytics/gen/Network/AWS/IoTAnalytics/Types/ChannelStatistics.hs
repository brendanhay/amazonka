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
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStatistics where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import qualified Network.AWS.Lens as Lens

-- | Statistics information about the channel.
--
-- /See:/ 'newChannelStatistics' smart constructor.
data ChannelStatistics = ChannelStatistics'
  { -- | The estimated size of the channel.
    size :: Core.Maybe EstimatedResourceSize
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChannelStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'channelStatistics_size' - The estimated size of the channel.
newChannelStatistics ::
  ChannelStatistics
newChannelStatistics =
  ChannelStatistics' {size = Core.Nothing}

-- | The estimated size of the channel.
channelStatistics_size :: Lens.Lens' ChannelStatistics (Core.Maybe EstimatedResourceSize)
channelStatistics_size = Lens.lens (\ChannelStatistics' {size} -> size) (\s@ChannelStatistics' {} a -> s {size = a} :: ChannelStatistics)

instance Core.FromJSON ChannelStatistics where
  parseJSON =
    Core.withObject
      "ChannelStatistics"
      ( \x ->
          ChannelStatistics' Core.<$> (x Core..:? "size")
      )

instance Core.Hashable ChannelStatistics

instance Core.NFData ChannelStatistics
