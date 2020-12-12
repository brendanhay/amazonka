{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HiveJSONSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HiveJSONSerDe
  ( HiveJSONSerDe (..),

    -- * Smart constructor
    mkHiveJSONSerDe,

    -- * Lenses
    hjsdTimestampFormats,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
--
-- /See:/ 'mkHiveJSONSerDe' smart constructor.
newtype HiveJSONSerDe = HiveJSONSerDe'
  { timestampFormats ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HiveJSONSerDe' with the minimum fields required to make a request.
--
-- * 'timestampFormats' - Indicates how you want Kinesis Data Firehose to parse the date and timestamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse timestamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
mkHiveJSONSerDe ::
  HiveJSONSerDe
mkHiveJSONSerDe = HiveJSONSerDe' {timestampFormats = Lude.Nothing}

-- | Indicates how you want Kinesis Data Firehose to parse the date and timestamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse timestamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
--
-- /Note:/ Consider using 'timestampFormats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjsdTimestampFormats :: Lens.Lens' HiveJSONSerDe (Lude.Maybe [Lude.Text])
hjsdTimestampFormats = Lens.lens (timestampFormats :: HiveJSONSerDe -> Lude.Maybe [Lude.Text]) (\s a -> s {timestampFormats = a} :: HiveJSONSerDe)
{-# DEPRECATED hjsdTimestampFormats "Use generic-lens or generic-optics with 'timestampFormats' instead." #-}

instance Lude.FromJSON HiveJSONSerDe where
  parseJSON =
    Lude.withObject
      "HiveJSONSerDe"
      ( \x ->
          HiveJSONSerDe'
            Lude.<$> (x Lude..:? "TimestampFormats" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON HiveJSONSerDe where
  toJSON HiveJSONSerDe' {..} =
    Lude.object
      ( Lude.catMaybes
          [("TimestampFormats" Lude..=) Lude.<$> timestampFormats]
      )
