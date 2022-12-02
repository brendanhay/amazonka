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
-- Module      : Amazonka.IoTAnalytics.Types.ChannelStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ChannelStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.EstimatedResourceSize
import qualified Amazonka.Prelude as Prelude

-- | Statistics information about the channel.
--
-- /See:/ 'newChannelStatistics' smart constructor.
data ChannelStatistics = ChannelStatistics'
  { -- | The estimated size of the channel.
    size :: Prelude.Maybe EstimatedResourceSize
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON ChannelStatistics where
  parseJSON =
    Data.withObject
      "ChannelStatistics"
      ( \x ->
          ChannelStatistics' Prelude.<$> (x Data..:? "size")
      )

instance Prelude.Hashable ChannelStatistics where
  hashWithSalt _salt ChannelStatistics' {..} =
    _salt `Prelude.hashWithSalt` size

instance Prelude.NFData ChannelStatistics where
  rnf ChannelStatistics' {..} = Prelude.rnf size
