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
-- Module      : Amazonka.Kinesis.Types.ShardFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.ShardFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.ShardFilterType
import qualified Amazonka.Prelude as Prelude

-- | The request parameter used to filter out the response of the
-- @ListShards@ API.
--
-- /See:/ 'newShardFilter' smart constructor.
data ShardFilter = ShardFilter'
  { -- | The timestamps specified in the @ShardFilter@ parameter. A timestamp is
    -- a Unix epoch date with precision in milliseconds. For example,
    -- 2016-04-04T19:58:46.480-00:00 or 1459799926.480. This property can only
    -- be used if @FROM_TIMESTAMP@ or @AT_TIMESTAMP@ shard types are specified.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The exclusive start @shardID@ speified in the @ShardFilter@ parameter.
    -- This property can only be used if the @AFTER_SHARD_ID@ shard type is
    -- specified.
    shardId :: Prelude.Maybe Prelude.Text,
    -- | The shard type specified in the @ShardFilter@ parameter. This is a
    -- required property of the @ShardFilter@ parameter.
    --
    -- You can specify the following valid values:
    --
    -- -   @AFTER_SHARD_ID@ - the response includes all the shards, starting
    --     with the shard whose ID immediately follows the @ShardId@ that you
    --     provided.
    --
    -- -   @AT_TRIM_HORIZON@ - the response includes all the shards that were
    --     open at @TRIM_HORIZON@.
    --
    -- -   @FROM_TRIM_HORIZON@ - (default), the response includes all the
    --     shards within the retention period of the data stream (trim to tip).
    --
    -- -   @AT_LATEST@ - the response includes only the currently open shards
    --     of the data stream.
    --
    -- -   @AT_TIMESTAMP@ - the response includes all shards whose start
    --     timestamp is less than or equal to the given timestamp and end
    --     timestamp is greater than or equal to the given timestamp or still
    --     open.
    --
    -- -   @FROM_TIMESTAMP@ - the response incldues all closed shards whose end
    --     timestamp is greater than or equal to the given timestamp and also
    --     all open shards. Corrected to @TRIM_HORIZON@ of the data stream if
    --     @FROM_TIMESTAMP@ is less than the @TRIM_HORIZON@ value.
    type' :: ShardFilterType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShardFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'shardFilter_timestamp' - The timestamps specified in the @ShardFilter@ parameter. A timestamp is
-- a Unix epoch date with precision in milliseconds. For example,
-- 2016-04-04T19:58:46.480-00:00 or 1459799926.480. This property can only
-- be used if @FROM_TIMESTAMP@ or @AT_TIMESTAMP@ shard types are specified.
--
-- 'shardId', 'shardFilter_shardId' - The exclusive start @shardID@ speified in the @ShardFilter@ parameter.
-- This property can only be used if the @AFTER_SHARD_ID@ shard type is
-- specified.
--
-- 'type'', 'shardFilter_type' - The shard type specified in the @ShardFilter@ parameter. This is a
-- required property of the @ShardFilter@ parameter.
--
-- You can specify the following valid values:
--
-- -   @AFTER_SHARD_ID@ - the response includes all the shards, starting
--     with the shard whose ID immediately follows the @ShardId@ that you
--     provided.
--
-- -   @AT_TRIM_HORIZON@ - the response includes all the shards that were
--     open at @TRIM_HORIZON@.
--
-- -   @FROM_TRIM_HORIZON@ - (default), the response includes all the
--     shards within the retention period of the data stream (trim to tip).
--
-- -   @AT_LATEST@ - the response includes only the currently open shards
--     of the data stream.
--
-- -   @AT_TIMESTAMP@ - the response includes all shards whose start
--     timestamp is less than or equal to the given timestamp and end
--     timestamp is greater than or equal to the given timestamp or still
--     open.
--
-- -   @FROM_TIMESTAMP@ - the response incldues all closed shards whose end
--     timestamp is greater than or equal to the given timestamp and also
--     all open shards. Corrected to @TRIM_HORIZON@ of the data stream if
--     @FROM_TIMESTAMP@ is less than the @TRIM_HORIZON@ value.
newShardFilter ::
  -- | 'type''
  ShardFilterType ->
  ShardFilter
newShardFilter pType_ =
  ShardFilter'
    { timestamp = Prelude.Nothing,
      shardId = Prelude.Nothing,
      type' = pType_
    }

-- | The timestamps specified in the @ShardFilter@ parameter. A timestamp is
-- a Unix epoch date with precision in milliseconds. For example,
-- 2016-04-04T19:58:46.480-00:00 or 1459799926.480. This property can only
-- be used if @FROM_TIMESTAMP@ or @AT_TIMESTAMP@ shard types are specified.
shardFilter_timestamp :: Lens.Lens' ShardFilter (Prelude.Maybe Prelude.UTCTime)
shardFilter_timestamp = Lens.lens (\ShardFilter' {timestamp} -> timestamp) (\s@ShardFilter' {} a -> s {timestamp = a} :: ShardFilter) Prelude.. Lens.mapping Data._Time

-- | The exclusive start @shardID@ speified in the @ShardFilter@ parameter.
-- This property can only be used if the @AFTER_SHARD_ID@ shard type is
-- specified.
shardFilter_shardId :: Lens.Lens' ShardFilter (Prelude.Maybe Prelude.Text)
shardFilter_shardId = Lens.lens (\ShardFilter' {shardId} -> shardId) (\s@ShardFilter' {} a -> s {shardId = a} :: ShardFilter)

-- | The shard type specified in the @ShardFilter@ parameter. This is a
-- required property of the @ShardFilter@ parameter.
--
-- You can specify the following valid values:
--
-- -   @AFTER_SHARD_ID@ - the response includes all the shards, starting
--     with the shard whose ID immediately follows the @ShardId@ that you
--     provided.
--
-- -   @AT_TRIM_HORIZON@ - the response includes all the shards that were
--     open at @TRIM_HORIZON@.
--
-- -   @FROM_TRIM_HORIZON@ - (default), the response includes all the
--     shards within the retention period of the data stream (trim to tip).
--
-- -   @AT_LATEST@ - the response includes only the currently open shards
--     of the data stream.
--
-- -   @AT_TIMESTAMP@ - the response includes all shards whose start
--     timestamp is less than or equal to the given timestamp and end
--     timestamp is greater than or equal to the given timestamp or still
--     open.
--
-- -   @FROM_TIMESTAMP@ - the response incldues all closed shards whose end
--     timestamp is greater than or equal to the given timestamp and also
--     all open shards. Corrected to @TRIM_HORIZON@ of the data stream if
--     @FROM_TIMESTAMP@ is less than the @TRIM_HORIZON@ value.
shardFilter_type :: Lens.Lens' ShardFilter ShardFilterType
shardFilter_type = Lens.lens (\ShardFilter' {type'} -> type') (\s@ShardFilter' {} a -> s {type' = a} :: ShardFilter)

instance Prelude.Hashable ShardFilter where
  hashWithSalt _salt ShardFilter' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` shardId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ShardFilter where
  rnf ShardFilter' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf shardId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ShardFilter where
  toJSON ShardFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Timestamp" Data..=) Prelude.<$> timestamp,
            ("ShardId" Data..=) Prelude.<$> shardId,
            Prelude.Just ("Type" Data..= type')
          ]
      )
