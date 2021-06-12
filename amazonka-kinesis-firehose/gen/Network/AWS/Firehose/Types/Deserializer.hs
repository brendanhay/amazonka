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
-- Module      : Network.AWS.Firehose.Types.Deserializer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Deserializer where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.HiveJsonSerDe
import Network.AWS.Firehose.Types.OpenXJsonSerDe
import qualified Network.AWS.Lens as Lens

-- | The deserializer you want Kinesis Data Firehose to use for converting
-- the input data from JSON. Kinesis Data Firehose then serializes the data
-- to its final format using the Serializer. Kinesis Data Firehose supports
-- two types of deserializers: the
-- <https://cwiki.apache.org/confluence/display/Hive/LanguageManual+DDL#LanguageManualDDL-JSON Apache Hive JSON SerDe>
-- and the <https://github.com/rcongiu/Hive-JSON-Serde OpenX JSON SerDe>.
--
-- /See:/ 'newDeserializer' smart constructor.
data Deserializer = Deserializer'
  { -- | The native Hive \/ HCatalog JsonSerDe. Used by Kinesis Data Firehose for
    -- deserializing data, which means converting it from the JSON format in
    -- preparation for serializing it to the Parquet or ORC format. This is one
    -- of two deserializers you can choose, depending on which one offers the
    -- functionality you need. The other option is the OpenX SerDe.
    hiveJsonSerDe :: Core.Maybe HiveJsonSerDe,
    -- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data,
    -- which means converting it from the JSON format in preparation for
    -- serializing it to the Parquet or ORC format. This is one of two
    -- deserializers you can choose, depending on which one offers the
    -- functionality you need. The other option is the native Hive \/ HCatalog
    -- JsonSerDe.
    openXJsonSerDe :: Core.Maybe OpenXJsonSerDe
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Deserializer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hiveJsonSerDe', 'deserializer_hiveJsonSerDe' - The native Hive \/ HCatalog JsonSerDe. Used by Kinesis Data Firehose for
-- deserializing data, which means converting it from the JSON format in
-- preparation for serializing it to the Parquet or ORC format. This is one
-- of two deserializers you can choose, depending on which one offers the
-- functionality you need. The other option is the OpenX SerDe.
--
-- 'openXJsonSerDe', 'deserializer_openXJsonSerDe' - The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data,
-- which means converting it from the JSON format in preparation for
-- serializing it to the Parquet or ORC format. This is one of two
-- deserializers you can choose, depending on which one offers the
-- functionality you need. The other option is the native Hive \/ HCatalog
-- JsonSerDe.
newDeserializer ::
  Deserializer
newDeserializer =
  Deserializer'
    { hiveJsonSerDe = Core.Nothing,
      openXJsonSerDe = Core.Nothing
    }

-- | The native Hive \/ HCatalog JsonSerDe. Used by Kinesis Data Firehose for
-- deserializing data, which means converting it from the JSON format in
-- preparation for serializing it to the Parquet or ORC format. This is one
-- of two deserializers you can choose, depending on which one offers the
-- functionality you need. The other option is the OpenX SerDe.
deserializer_hiveJsonSerDe :: Lens.Lens' Deserializer (Core.Maybe HiveJsonSerDe)
deserializer_hiveJsonSerDe = Lens.lens (\Deserializer' {hiveJsonSerDe} -> hiveJsonSerDe) (\s@Deserializer' {} a -> s {hiveJsonSerDe = a} :: Deserializer)

-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data,
-- which means converting it from the JSON format in preparation for
-- serializing it to the Parquet or ORC format. This is one of two
-- deserializers you can choose, depending on which one offers the
-- functionality you need. The other option is the native Hive \/ HCatalog
-- JsonSerDe.
deserializer_openXJsonSerDe :: Lens.Lens' Deserializer (Core.Maybe OpenXJsonSerDe)
deserializer_openXJsonSerDe = Lens.lens (\Deserializer' {openXJsonSerDe} -> openXJsonSerDe) (\s@Deserializer' {} a -> s {openXJsonSerDe = a} :: Deserializer)

instance Core.FromJSON Deserializer where
  parseJSON =
    Core.withObject
      "Deserializer"
      ( \x ->
          Deserializer'
            Core.<$> (x Core..:? "HiveJsonSerDe")
            Core.<*> (x Core..:? "OpenXJsonSerDe")
      )

instance Core.Hashable Deserializer

instance Core.NFData Deserializer

instance Core.ToJSON Deserializer where
  toJSON Deserializer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("HiveJsonSerDe" Core..=) Core.<$> hiveJsonSerDe,
            ("OpenXJsonSerDe" Core..=) Core.<$> openXJsonSerDe
          ]
      )
