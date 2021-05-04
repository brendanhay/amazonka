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
-- Module      : Network.AWS.Firehose.Types.HiveJsonSerDe
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HiveJsonSerDe where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    timestampFormats :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  HiveJsonSerDe' {timestampFormats = Prelude.Nothing}

-- | Indicates how you want Kinesis Data Firehose to parse the date and
-- timestamps that may be present in your input data JSON. To specify these
-- format strings, follow the pattern syntax of JodaTime\'s DateTimeFormat
-- format strings. For more information, see
-- <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat>.
-- You can also use the special value @millis@ to parse timestamps in epoch
-- milliseconds. If you don\'t specify a format, Kinesis Data Firehose uses
-- @java.sql.Timestamp::valueOf@ by default.
hiveJsonSerDe_timestampFormats :: Lens.Lens' HiveJsonSerDe (Prelude.Maybe [Prelude.Text])
hiveJsonSerDe_timestampFormats = Lens.lens (\HiveJsonSerDe' {timestampFormats} -> timestampFormats) (\s@HiveJsonSerDe' {} a -> s {timestampFormats = a} :: HiveJsonSerDe) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON HiveJsonSerDe where
  parseJSON =
    Prelude.withObject
      "HiveJsonSerDe"
      ( \x ->
          HiveJsonSerDe'
            Prelude.<$> ( x Prelude..:? "TimestampFormats"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HiveJsonSerDe

instance Prelude.NFData HiveJsonSerDe

instance Prelude.ToJSON HiveJsonSerDe where
  toJSON HiveJsonSerDe' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TimestampFormats" Prelude..=)
              Prelude.<$> timestampFormats
          ]
      )
