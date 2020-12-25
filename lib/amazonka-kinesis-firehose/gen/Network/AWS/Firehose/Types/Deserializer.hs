{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Deserializer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Deserializer
  ( Deserializer (..),

    -- * Smart constructor
    mkDeserializer,

    -- * Lenses
    dHiveJsonSerDe,
    dOpenXJsonSerDe,
  )
where

import qualified Network.AWS.Firehose.Types.HiveJsonSerDe as Types
import qualified Network.AWS.Firehose.Types.OpenXJsonSerDe as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The deserializer you want Kinesis Data Firehose to use for converting the input data from JSON. Kinesis Data Firehose then serializes the data to its final format using the 'Serializer' . Kinesis Data Firehose supports two types of deserializers: the <https://cwiki.apache.org/confluence/display/Hive/LanguageManual+DDL#LanguageManualDDL-JSON Apache Hive JSON SerDe> and the <https://github.com/rcongiu/Hive-JSON-Serde OpenX JSON SerDe> .
--
-- /See:/ 'mkDeserializer' smart constructor.
data Deserializer = Deserializer'
  { -- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
    hiveJsonSerDe :: Core.Maybe Types.HiveJsonSerDe,
    -- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
    openXJsonSerDe :: Core.Maybe Types.OpenXJsonSerDe
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Deserializer' value with any optional fields omitted.
mkDeserializer ::
  Deserializer
mkDeserializer =
  Deserializer'
    { hiveJsonSerDe = Core.Nothing,
      openXJsonSerDe = Core.Nothing
    }

-- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
--
-- /Note:/ Consider using 'hiveJsonSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHiveJsonSerDe :: Lens.Lens' Deserializer (Core.Maybe Types.HiveJsonSerDe)
dHiveJsonSerDe = Lens.field @"hiveJsonSerDe"
{-# DEPRECATED dHiveJsonSerDe "Use generic-lens or generic-optics with 'hiveJsonSerDe' instead." #-}

-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
--
-- /Note:/ Consider using 'openXJsonSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOpenXJsonSerDe :: Lens.Lens' Deserializer (Core.Maybe Types.OpenXJsonSerDe)
dOpenXJsonSerDe = Lens.field @"openXJsonSerDe"
{-# DEPRECATED dOpenXJsonSerDe "Use generic-lens or generic-optics with 'openXJsonSerDe' instead." #-}

instance Core.FromJSON Deserializer where
  toJSON Deserializer {..} =
    Core.object
      ( Core.catMaybes
          [ ("HiveJsonSerDe" Core..=) Core.<$> hiveJsonSerDe,
            ("OpenXJsonSerDe" Core..=) Core.<$> openXJsonSerDe
          ]
      )

instance Core.FromJSON Deserializer where
  parseJSON =
    Core.withObject "Deserializer" Core.$
      \x ->
        Deserializer'
          Core.<$> (x Core..:? "HiveJsonSerDe") Core.<*> (x Core..:? "OpenXJsonSerDe")
