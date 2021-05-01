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
-- Module      : Network.AWS.Kinesis.Types.ShardFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardFilter where

import Network.AWS.Kinesis.Types.ShardFilterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newShardFilter' smart constructor.
data ShardFilter = ShardFilter'
  { shardId :: Prelude.Maybe Prelude.Text,
    timestamp :: Prelude.Maybe Prelude.POSIX,
    type' :: ShardFilterType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { shardId = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      type' = pType_
    }

-- | Undocumented member.
shardFilter_shardId :: Lens.Lens' ShardFilter (Prelude.Maybe Prelude.Text)
shardFilter_shardId = Lens.lens (\ShardFilter' {shardId} -> shardId) (\s@ShardFilter' {} a -> s {shardId = a} :: ShardFilter)

-- | Undocumented member.
shardFilter_timestamp :: Lens.Lens' ShardFilter (Prelude.Maybe Prelude.UTCTime)
shardFilter_timestamp = Lens.lens (\ShardFilter' {timestamp} -> timestamp) (\s@ShardFilter' {} a -> s {timestamp = a} :: ShardFilter) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
shardFilter_type :: Lens.Lens' ShardFilter ShardFilterType
shardFilter_type = Lens.lens (\ShardFilter' {type'} -> type') (\s@ShardFilter' {} a -> s {type' = a} :: ShardFilter)

instance Prelude.Hashable ShardFilter

instance Prelude.NFData ShardFilter

instance Prelude.ToJSON ShardFilter where
  toJSON ShardFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ShardId" Prelude..=) Prelude.<$> shardId,
            ("Timestamp" Prelude..=) Prelude.<$> timestamp,
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
