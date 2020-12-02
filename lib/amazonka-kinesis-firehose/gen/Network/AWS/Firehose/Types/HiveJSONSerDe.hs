{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HiveJSONSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HiveJSONSerDe where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
--
--
--
-- /See:/ 'hiveJSONSerDe' smart constructor.
newtype HiveJSONSerDe = HiveJSONSerDe'
  { _hjsdTimestampFormats ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HiveJSONSerDe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hjsdTimestampFormats' - Indicates how you want Kinesis Data Firehose to parse the date and timestamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse timestamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
hiveJSONSerDe ::
  HiveJSONSerDe
hiveJSONSerDe = HiveJSONSerDe' {_hjsdTimestampFormats = Nothing}

-- | Indicates how you want Kinesis Data Firehose to parse the date and timestamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse timestamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
hjsdTimestampFormats :: Lens' HiveJSONSerDe [Text]
hjsdTimestampFormats = lens _hjsdTimestampFormats (\s a -> s {_hjsdTimestampFormats = a}) . _Default . _Coerce

instance FromJSON HiveJSONSerDe where
  parseJSON =
    withObject
      "HiveJSONSerDe"
      (\x -> HiveJSONSerDe' <$> (x .:? "TimestampFormats" .!= mempty))

instance Hashable HiveJSONSerDe

instance NFData HiveJSONSerDe

instance ToJSON HiveJSONSerDe where
  toJSON HiveJSONSerDe' {..} =
    object
      (catMaybes [("TimestampFormats" .=) <$> _hjsdTimestampFormats])
