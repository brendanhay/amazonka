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
-- Module      : Network.AWS.Firehose.Types.HiveJsonSerDe
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HiveJsonSerDe where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The native Hive \/ HCatalog JsonSerDe. Used by Kinesis Data Firehose for
-- deserializing data, which means converting it from the JSON format in
-- preparation for serializing it to the Parquet or ORC format. This is one
-- of two deserializers you can choose, depending on which one offers the
-- functionality you need. The other option is the OpenX SerDe.
--
-- /See:/ 'newHiveJsonSerDe' smart constructor.
data HiveJsonSerDe = HiveJsonSerDe'
  { -- | Indicates how you want Kinesis Data Firehose to parse the date and
    -- timestamps that may be present in your input data JSON. To specify these
    -- format strings, follow the pattern syntax of JodaTime\'s DateTimeFormat
    -- format strings. For more information, see
    -- <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat>.
    -- You can also use the special value @millis@ to parse timestamps in epoch
    -- milliseconds. If you don\'t specify a format, Kinesis Data Firehose uses
    -- @java.sql.Timestamp::valueOf@ by default.
    timestampFormats :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HiveJsonSerDe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestampFormats', 'hiveJsonSerDe_timestampFormats' - Indicates how you want Kinesis Data Firehose to parse the date and
-- timestamps that may be present in your input data JSON. To specify these
-- format strings, follow the pattern syntax of JodaTime\'s DateTimeFormat
-- format strings. For more information, see
-- <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat>.
-- You can also use the special value @millis@ to parse timestamps in epoch
-- milliseconds. If you don\'t specify a format, Kinesis Data Firehose uses
-- @java.sql.Timestamp::valueOf@ by default.
newHiveJsonSerDe ::
  HiveJsonSerDe
newHiveJsonSerDe =
  HiveJsonSerDe' {timestampFormats = Core.Nothing}

-- | Indicates how you want Kinesis Data Firehose to parse the date and
-- timestamps that may be present in your input data JSON. To specify these
-- format strings, follow the pattern syntax of JodaTime\'s DateTimeFormat
-- format strings. For more information, see
-- <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat>.
-- You can also use the special value @millis@ to parse timestamps in epoch
-- milliseconds. If you don\'t specify a format, Kinesis Data Firehose uses
-- @java.sql.Timestamp::valueOf@ by default.
hiveJsonSerDe_timestampFormats :: Lens.Lens' HiveJsonSerDe (Core.Maybe [Core.Text])
hiveJsonSerDe_timestampFormats = Lens.lens (\HiveJsonSerDe' {timestampFormats} -> timestampFormats) (\s@HiveJsonSerDe' {} a -> s {timestampFormats = a} :: HiveJsonSerDe) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON HiveJsonSerDe where
  parseJSON =
    Core.withObject
      "HiveJsonSerDe"
      ( \x ->
          HiveJsonSerDe'
            Core.<$> (x Core..:? "TimestampFormats" Core..!= Core.mempty)
      )

instance Core.Hashable HiveJsonSerDe

instance Core.NFData HiveJsonSerDe

instance Core.ToJSON HiveJsonSerDe where
  toJSON HiveJsonSerDe' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TimestampFormats" Core..=)
              Core.<$> timestampFormats
          ]
      )
