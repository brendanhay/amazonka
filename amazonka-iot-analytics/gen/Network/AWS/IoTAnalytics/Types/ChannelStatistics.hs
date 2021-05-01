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
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStatistics where

import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Statistics information about the channel.
--
-- /See:/ 'newChannelStatistics' smart constructor.
data ChannelStatistics = ChannelStatistics'
  { -- | The estimated size of the channel.
    size :: Prelude.Maybe EstimatedResourceSize
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ChannelStatistics' {size = Prelude.Nothing}

-- | The estimated size of the channel.
channelStatistics_size :: Lens.Lens' ChannelStatistics (Prelude.Maybe EstimatedResourceSize)
channelStatistics_size = Lens.lens (\ChannelStatistics' {size} -> size) (\s@ChannelStatistics' {} a -> s {size = a} :: ChannelStatistics)

instance Prelude.FromJSON ChannelStatistics where
  parseJSON =
    Prelude.withObject
      "ChannelStatistics"
      ( \x ->
          ChannelStatistics'
            Prelude.<$> (x Prelude..:? "size")
      )

instance Prelude.Hashable ChannelStatistics

instance Prelude.NFData ChannelStatistics
