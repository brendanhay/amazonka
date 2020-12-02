{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamAction where

import Network.AWS.IoT.Types.TimestreamDimension
import Network.AWS.IoT.Types.TimestreamTimestamp
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
--
--
--
-- /See:/ 'timestreamAction' smart constructor.
data TimestreamAction = TimestreamAction'
  { _taTimestamp ::
      !(Maybe TimestreamTimestamp),
    _taRoleARN :: !Text,
    _taDatabaseName :: !Text,
    _taTableName :: !Text,
    _taDimensions :: !(List1 TimestreamDimension)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimestreamAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taTimestamp' - Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column. You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template. If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule.
--
-- * 'taRoleARN' - The ARN of the role that grants permission to write to the Amazon Timestream database table.
--
-- * 'taDatabaseName' - The name of an Amazon Timestream database.
--
-- * 'taTableName' - The name of the database table into which to write the measure records.
--
-- * 'taDimensions' - Metadata attributes of the time series that are written in each measure record.
timestreamAction ::
  -- | 'taRoleARN'
  Text ->
  -- | 'taDatabaseName'
  Text ->
  -- | 'taTableName'
  Text ->
  -- | 'taDimensions'
  NonEmpty TimestreamDimension ->
  TimestreamAction
timestreamAction pRoleARN_ pDatabaseName_ pTableName_ pDimensions_ =
  TimestreamAction'
    { _taTimestamp = Nothing,
      _taRoleARN = pRoleARN_,
      _taDatabaseName = pDatabaseName_,
      _taTableName = pTableName_,
      _taDimensions = _List1 # pDimensions_
    }

-- | Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column. You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template. If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule.
taTimestamp :: Lens' TimestreamAction (Maybe TimestreamTimestamp)
taTimestamp = lens _taTimestamp (\s a -> s {_taTimestamp = a})

-- | The ARN of the role that grants permission to write to the Amazon Timestream database table.
taRoleARN :: Lens' TimestreamAction Text
taRoleARN = lens _taRoleARN (\s a -> s {_taRoleARN = a})

-- | The name of an Amazon Timestream database.
taDatabaseName :: Lens' TimestreamAction Text
taDatabaseName = lens _taDatabaseName (\s a -> s {_taDatabaseName = a})

-- | The name of the database table into which to write the measure records.
taTableName :: Lens' TimestreamAction Text
taTableName = lens _taTableName (\s a -> s {_taTableName = a})

-- | Metadata attributes of the time series that are written in each measure record.
taDimensions :: Lens' TimestreamAction (NonEmpty TimestreamDimension)
taDimensions = lens _taDimensions (\s a -> s {_taDimensions = a}) . _List1

instance FromJSON TimestreamAction where
  parseJSON =
    withObject
      "TimestreamAction"
      ( \x ->
          TimestreamAction'
            <$> (x .:? "timestamp")
            <*> (x .: "roleArn")
            <*> (x .: "databaseName")
            <*> (x .: "tableName")
            <*> (x .: "dimensions")
      )

instance Hashable TimestreamAction

instance NFData TimestreamAction

instance ToJSON TimestreamAction where
  toJSON TimestreamAction' {..} =
    object
      ( catMaybes
          [ ("timestamp" .=) <$> _taTimestamp,
            Just ("roleArn" .= _taRoleARN),
            Just ("databaseName" .= _taDatabaseName),
            Just ("tableName" .= _taTableName),
            Just ("dimensions" .= _taDimensions)
          ]
      )
