{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FirehoseAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FirehoseAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action that writes data to an Amazon Kinesis Firehose stream.
--
--
--
-- /See:/ 'firehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { _faBatchMode ::
      !(Maybe Bool),
    _faSeparator :: !(Maybe Text),
    _faRoleARN :: !Text,
    _faDeliveryStreamName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FirehoseAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faBatchMode' - Whether to deliver the Kinesis Data Firehose stream as a batch by using <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > . The default value is @false@ . When @batchMode@ is @true@ and the rule's SQL statement evaluates to an Array, each Array element forms one record in the <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > request. The resulting array can't have more than 500 records.
--
-- * 'faSeparator' - A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
--
-- * 'faRoleARN' - The IAM role that grants access to the Amazon Kinesis Firehose stream.
--
-- * 'faDeliveryStreamName' - The delivery stream name.
firehoseAction ::
  -- | 'faRoleARN'
  Text ->
  -- | 'faDeliveryStreamName'
  Text ->
  FirehoseAction
firehoseAction pRoleARN_ pDeliveryStreamName_ =
  FirehoseAction'
    { _faBatchMode = Nothing,
      _faSeparator = Nothing,
      _faRoleARN = pRoleARN_,
      _faDeliveryStreamName = pDeliveryStreamName_
    }

-- | Whether to deliver the Kinesis Data Firehose stream as a batch by using <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > . The default value is @false@ . When @batchMode@ is @true@ and the rule's SQL statement evaluates to an Array, each Array element forms one record in the <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > request. The resulting array can't have more than 500 records.
faBatchMode :: Lens' FirehoseAction (Maybe Bool)
faBatchMode = lens _faBatchMode (\s a -> s {_faBatchMode = a})

-- | A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
faSeparator :: Lens' FirehoseAction (Maybe Text)
faSeparator = lens _faSeparator (\s a -> s {_faSeparator = a})

-- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
faRoleARN :: Lens' FirehoseAction Text
faRoleARN = lens _faRoleARN (\s a -> s {_faRoleARN = a})

-- | The delivery stream name.
faDeliveryStreamName :: Lens' FirehoseAction Text
faDeliveryStreamName = lens _faDeliveryStreamName (\s a -> s {_faDeliveryStreamName = a})

instance FromJSON FirehoseAction where
  parseJSON =
    withObject
      "FirehoseAction"
      ( \x ->
          FirehoseAction'
            <$> (x .:? "batchMode")
            <*> (x .:? "separator")
            <*> (x .: "roleArn")
            <*> (x .: "deliveryStreamName")
      )

instance Hashable FirehoseAction

instance NFData FirehoseAction

instance ToJSON FirehoseAction where
  toJSON FirehoseAction' {..} =
    object
      ( catMaybes
          [ ("batchMode" .=) <$> _faBatchMode,
            ("separator" .=) <$> _faSeparator,
            Just ("roleArn" .= _faRoleARN),
            Just ("deliveryStreamName" .= _faDeliveryStreamName)
          ]
      )
