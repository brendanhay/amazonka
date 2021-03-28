{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OpenXJsonSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.OpenXJsonSerDe
  ( OpenXJsonSerDe (..)
  -- * Smart constructor
  , mkOpenXJsonSerDe
  -- * Lenses
  , oxjsdCaseInsensitive
  , oxjsdColumnToJsonKeyMappings
  , oxjsdConvertDotsInJsonKeysToUnderscores
  ) where

import qualified Network.AWS.Firehose.Types.NonEmptyString as Types
import qualified Network.AWS.Firehose.Types.NonEmptyStringWithoutWhitespace as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
--
-- /See:/ 'mkOpenXJsonSerDe' smart constructor.
data OpenXJsonSerDe = OpenXJsonSerDe'
  { caseInsensitive :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
  , columnToJsonKeyMappings :: Core.Maybe (Core.HashMap Types.NonEmptyStringWithoutWhitespace Types.NonEmptyString)
    -- ^ Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
  , convertDotsInJsonKeysToUnderscores :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option.
--
-- The default is @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpenXJsonSerDe' value with any optional fields omitted.
mkOpenXJsonSerDe
    :: OpenXJsonSerDe
mkOpenXJsonSerDe
  = OpenXJsonSerDe'{caseInsensitive = Core.Nothing,
                    columnToJsonKeyMappings = Core.Nothing,
                    convertDotsInJsonKeysToUnderscores = Core.Nothing}

-- | When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
--
-- /Note:/ Consider using 'caseInsensitive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oxjsdCaseInsensitive :: Lens.Lens' OpenXJsonSerDe (Core.Maybe Core.Bool)
oxjsdCaseInsensitive = Lens.field @"caseInsensitive"
{-# INLINEABLE oxjsdCaseInsensitive #-}
{-# DEPRECATED caseInsensitive "Use generic-lens or generic-optics with 'caseInsensitive' instead"  #-}

-- | Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
--
-- /Note:/ Consider using 'columnToJsonKeyMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oxjsdColumnToJsonKeyMappings :: Lens.Lens' OpenXJsonSerDe (Core.Maybe (Core.HashMap Types.NonEmptyStringWithoutWhitespace Types.NonEmptyString))
oxjsdColumnToJsonKeyMappings = Lens.field @"columnToJsonKeyMappings"
{-# INLINEABLE oxjsdColumnToJsonKeyMappings #-}
{-# DEPRECATED columnToJsonKeyMappings "Use generic-lens or generic-optics with 'columnToJsonKeyMappings' instead"  #-}

-- | When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option.
--
-- The default is @false@ .
--
-- /Note:/ Consider using 'convertDotsInJsonKeysToUnderscores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oxjsdConvertDotsInJsonKeysToUnderscores :: Lens.Lens' OpenXJsonSerDe (Core.Maybe Core.Bool)
oxjsdConvertDotsInJsonKeysToUnderscores = Lens.field @"convertDotsInJsonKeysToUnderscores"
{-# INLINEABLE oxjsdConvertDotsInJsonKeysToUnderscores #-}
{-# DEPRECATED convertDotsInJsonKeysToUnderscores "Use generic-lens or generic-optics with 'convertDotsInJsonKeysToUnderscores' instead"  #-}

instance Core.FromJSON OpenXJsonSerDe where
        toJSON OpenXJsonSerDe{..}
          = Core.object
              (Core.catMaybes
                 [("CaseInsensitive" Core..=) Core.<$> caseInsensitive,
                  ("ColumnToJsonKeyMappings" Core..=) Core.<$>
                    columnToJsonKeyMappings,
                  ("ConvertDotsInJsonKeysToUnderscores" Core..=) Core.<$>
                    convertDotsInJsonKeysToUnderscores])

instance Core.FromJSON OpenXJsonSerDe where
        parseJSON
          = Core.withObject "OpenXJsonSerDe" Core.$
              \ x ->
                OpenXJsonSerDe' Core.<$>
                  (x Core..:? "CaseInsensitive") Core.<*>
                    x Core..:? "ColumnToJsonKeyMappings"
                    Core.<*> x Core..:? "ConvertDotsInJsonKeysToUnderscores"
