{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HiveJsonSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HiveJsonSerDe
  ( HiveJsonSerDe (..)
  -- * Smart constructor
  , mkHiveJsonSerDe
  -- * Lenses
  , hjsdTimestampFormats
  ) where

import qualified Network.AWS.Firehose.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
--
-- /See:/ 'mkHiveJsonSerDe' smart constructor.
newtype HiveJsonSerDe = HiveJsonSerDe'
  { timestampFormats :: Core.Maybe [Types.NonEmptyString]
    -- ^ Indicates how you want Kinesis Data Firehose to parse the date and timestamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse timestamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HiveJsonSerDe' value with any optional fields omitted.
mkHiveJsonSerDe
    :: HiveJsonSerDe
mkHiveJsonSerDe = HiveJsonSerDe'{timestampFormats = Core.Nothing}

-- | Indicates how you want Kinesis Data Firehose to parse the date and timestamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse timestamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
--
-- /Note:/ Consider using 'timestampFormats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjsdTimestampFormats :: Lens.Lens' HiveJsonSerDe (Core.Maybe [Types.NonEmptyString])
hjsdTimestampFormats = Lens.field @"timestampFormats"
{-# INLINEABLE hjsdTimestampFormats #-}
{-# DEPRECATED timestampFormats "Use generic-lens or generic-optics with 'timestampFormats' instead"  #-}

instance Core.FromJSON HiveJsonSerDe where
        toJSON HiveJsonSerDe{..}
          = Core.object
              (Core.catMaybes
                 [("TimestampFormats" Core..=) Core.<$> timestampFormats])

instance Core.FromJSON HiveJsonSerDe where
        parseJSON
          = Core.withObject "HiveJsonSerDe" Core.$
              \ x -> HiveJsonSerDe' Core.<$> (x Core..:? "TimestampFormats")
