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
-- Module      : Network.AWS.Kinesis.Types.ShardFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types.ShardFilterType
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newShardFilter' smart constructor.
data ShardFilter = ShardFilter'
  { shardId :: Core.Maybe Core.Text,
    timestamp :: Core.Maybe Core.POSIX,
    type' :: ShardFilterType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ShardFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardId', 'shardFilter_shardId' - Undocumented member.
--
-- 'timestamp', 'shardFilter_timestamp' - Undocumented member.
--
-- 'type'', 'shardFilter_type' - Undocumented member.
newShardFilter ::
  -- | 'type''
  ShardFilterType ->
  ShardFilter
newShardFilter pType_ =
  ShardFilter'
    { shardId = Core.Nothing,
      timestamp = Core.Nothing,
      type' = pType_
    }

-- | Undocumented member.
shardFilter_shardId :: Lens.Lens' ShardFilter (Core.Maybe Core.Text)
shardFilter_shardId = Lens.lens (\ShardFilter' {shardId} -> shardId) (\s@ShardFilter' {} a -> s {shardId = a} :: ShardFilter)

-- | Undocumented member.
shardFilter_timestamp :: Lens.Lens' ShardFilter (Core.Maybe Core.UTCTime)
shardFilter_timestamp = Lens.lens (\ShardFilter' {timestamp} -> timestamp) (\s@ShardFilter' {} a -> s {timestamp = a} :: ShardFilter) Core.. Lens.mapping Core._Time

-- | Undocumented member.
shardFilter_type :: Lens.Lens' ShardFilter ShardFilterType
shardFilter_type = Lens.lens (\ShardFilter' {type'} -> type') (\s@ShardFilter' {} a -> s {type' = a} :: ShardFilter)

instance Core.Hashable ShardFilter

instance Core.NFData ShardFilter

instance Core.ToJSON ShardFilter where
  toJSON ShardFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ShardId" Core..=) Core.<$> shardId,
            ("Timestamp" Core..=) Core.<$> timestamp,
            Core.Just ("Type" Core..= type')
          ]
      )
