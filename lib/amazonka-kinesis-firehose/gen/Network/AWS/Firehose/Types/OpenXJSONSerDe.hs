{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OpenXJSONSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OpenXJSONSerDe
  ( OpenXJSONSerDe (..),

    -- * Smart constructor
    mkOpenXJSONSerDe,

    -- * Lenses
    oxjsdColumnToJSONKeyMappings,
    oxjsdCaseInsensitive,
    oxjsdConvertDotsInJSONKeysToUnderscores,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
--
-- /See:/ 'mkOpenXJSONSerDe' smart constructor.
data OpenXJSONSerDe = OpenXJSONSerDe'
  { columnToJSONKeyMappings ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    caseInsensitive :: Lude.Maybe Lude.Bool,
    convertDotsInJSONKeysToUnderscores :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpenXJSONSerDe' with the minimum fields required to make a request.
--
-- * 'caseInsensitive' - When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
-- * 'columnToJSONKeyMappings' - Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
-- * 'convertDotsInJSONKeysToUnderscores' - When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option.
--
-- The default is @false@ .
mkOpenXJSONSerDe ::
  OpenXJSONSerDe
mkOpenXJSONSerDe =
  OpenXJSONSerDe'
    { columnToJSONKeyMappings = Lude.Nothing,
      caseInsensitive = Lude.Nothing,
      convertDotsInJSONKeysToUnderscores = Lude.Nothing
    }

-- | Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
--
-- /Note:/ Consider using 'columnToJSONKeyMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oxjsdColumnToJSONKeyMappings :: Lens.Lens' OpenXJSONSerDe (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
oxjsdColumnToJSONKeyMappings = Lens.lens (columnToJSONKeyMappings :: OpenXJSONSerDe -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {columnToJSONKeyMappings = a} :: OpenXJSONSerDe)
{-# DEPRECATED oxjsdColumnToJSONKeyMappings "Use generic-lens or generic-optics with 'columnToJSONKeyMappings' instead." #-}

-- | When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
--
-- /Note:/ Consider using 'caseInsensitive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oxjsdCaseInsensitive :: Lens.Lens' OpenXJSONSerDe (Lude.Maybe Lude.Bool)
oxjsdCaseInsensitive = Lens.lens (caseInsensitive :: OpenXJSONSerDe -> Lude.Maybe Lude.Bool) (\s a -> s {caseInsensitive = a} :: OpenXJSONSerDe)
{-# DEPRECATED oxjsdCaseInsensitive "Use generic-lens or generic-optics with 'caseInsensitive' instead." #-}

-- | When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option.
--
-- The default is @false@ .
--
-- /Note:/ Consider using 'convertDotsInJSONKeysToUnderscores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oxjsdConvertDotsInJSONKeysToUnderscores :: Lens.Lens' OpenXJSONSerDe (Lude.Maybe Lude.Bool)
oxjsdConvertDotsInJSONKeysToUnderscores = Lens.lens (convertDotsInJSONKeysToUnderscores :: OpenXJSONSerDe -> Lude.Maybe Lude.Bool) (\s a -> s {convertDotsInJSONKeysToUnderscores = a} :: OpenXJSONSerDe)
{-# DEPRECATED oxjsdConvertDotsInJSONKeysToUnderscores "Use generic-lens or generic-optics with 'convertDotsInJSONKeysToUnderscores' instead." #-}

instance Lude.FromJSON OpenXJSONSerDe where
  parseJSON =
    Lude.withObject
      "OpenXJSONSerDe"
      ( \x ->
          OpenXJSONSerDe'
            Lude.<$> (x Lude..:? "ColumnToJsonKeyMappings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CaseInsensitive")
            Lude.<*> (x Lude..:? "ConvertDotsInJsonKeysToUnderscores")
      )

instance Lude.ToJSON OpenXJSONSerDe where
  toJSON OpenXJSONSerDe' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ColumnToJsonKeyMappings" Lude..=)
              Lude.<$> columnToJSONKeyMappings,
            ("CaseInsensitive" Lude..=) Lude.<$> caseInsensitive,
            ("ConvertDotsInJsonKeysToUnderscores" Lude..=)
              Lude.<$> convertDotsInJSONKeysToUnderscores
          ]
      )
