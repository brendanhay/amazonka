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
    dOpenXJSONSerDe,
    dHiveJSONSerDe,
  )
where

import Network.AWS.Firehose.Types.HiveJSONSerDe
import Network.AWS.Firehose.Types.OpenXJSONSerDe
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The deserializer you want Kinesis Data Firehose to use for converting the input data from JSON. Kinesis Data Firehose then serializes the data to its final format using the 'Serializer' . Kinesis Data Firehose supports two types of deserializers: the <https://cwiki.apache.org/confluence/display/Hive/LanguageManual+DDL#LanguageManualDDL-JSON Apache Hive JSON SerDe> and the <https://github.com/rcongiu/Hive-JSON-Serde OpenX JSON SerDe> .
--
-- /See:/ 'mkDeserializer' smart constructor.
data Deserializer = Deserializer'
  { openXJSONSerDe ::
      Lude.Maybe OpenXJSONSerDe,
    hiveJSONSerDe :: Lude.Maybe HiveJSONSerDe
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Deserializer' with the minimum fields required to make a request.
--
-- * 'hiveJSONSerDe' - The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
-- * 'openXJSONSerDe' - The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
mkDeserializer ::
  Deserializer
mkDeserializer =
  Deserializer'
    { openXJSONSerDe = Lude.Nothing,
      hiveJSONSerDe = Lude.Nothing
    }

-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
--
-- /Note:/ Consider using 'openXJSONSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOpenXJSONSerDe :: Lens.Lens' Deserializer (Lude.Maybe OpenXJSONSerDe)
dOpenXJSONSerDe = Lens.lens (openXJSONSerDe :: Deserializer -> Lude.Maybe OpenXJSONSerDe) (\s a -> s {openXJSONSerDe = a} :: Deserializer)
{-# DEPRECATED dOpenXJSONSerDe "Use generic-lens or generic-optics with 'openXJSONSerDe' instead." #-}

-- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
--
-- /Note:/ Consider using 'hiveJSONSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHiveJSONSerDe :: Lens.Lens' Deserializer (Lude.Maybe HiveJSONSerDe)
dHiveJSONSerDe = Lens.lens (hiveJSONSerDe :: Deserializer -> Lude.Maybe HiveJSONSerDe) (\s a -> s {hiveJSONSerDe = a} :: Deserializer)
{-# DEPRECATED dHiveJSONSerDe "Use generic-lens or generic-optics with 'hiveJSONSerDe' instead." #-}

instance Lude.FromJSON Deserializer where
  parseJSON =
    Lude.withObject
      "Deserializer"
      ( \x ->
          Deserializer'
            Lude.<$> (x Lude..:? "OpenXJsonSerDe")
            Lude.<*> (x Lude..:? "HiveJsonSerDe")
      )

instance Lude.ToJSON Deserializer where
  toJSON Deserializer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OpenXJsonSerDe" Lude..=) Lude.<$> openXJSONSerDe,
            ("HiveJsonSerDe" Lude..=) Lude.<$> hiveJSONSerDe
          ]
      )
