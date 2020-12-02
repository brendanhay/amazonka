{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputUpdate where

import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes updates to a specific input configuration (identified by the @InputId@ of an application).
--
--
--
-- /See:/ 'inputUpdate' smart constructor.
data InputUpdate = InputUpdate'
  { _iuInputProcessingConfigurationUpdate ::
      !(Maybe InputProcessingConfigurationUpdate),
    _iuKinesisStreamsInputUpdate :: !(Maybe KinesisStreamsInputUpdate),
    _iuInputParallelismUpdate :: !(Maybe InputParallelismUpdate),
    _iuNamePrefixUpdate :: !(Maybe Text),
    _iuInputSchemaUpdate :: !(Maybe InputSchemaUpdate),
    _iuKinesisFirehoseInputUpdate ::
      !(Maybe KinesisFirehoseInputUpdate),
    _iuInputId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iuInputProcessingConfigurationUpdate' - Describes updates for an input processing configuration.
--
-- * 'iuKinesisStreamsInputUpdate' - If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
--
-- * 'iuInputParallelismUpdate' - Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
--
-- * 'iuNamePrefixUpdate' - Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
--
-- * 'iuInputSchemaUpdate' - Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
--
-- * 'iuKinesisFirehoseInputUpdate' - If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
--
-- * 'iuInputId' - Input ID of the application input to be updated.
inputUpdate ::
  -- | 'iuInputId'
  Text ->
  InputUpdate
inputUpdate pInputId_ =
  InputUpdate'
    { _iuInputProcessingConfigurationUpdate = Nothing,
      _iuKinesisStreamsInputUpdate = Nothing,
      _iuInputParallelismUpdate = Nothing,
      _iuNamePrefixUpdate = Nothing,
      _iuInputSchemaUpdate = Nothing,
      _iuKinesisFirehoseInputUpdate = Nothing,
      _iuInputId = pInputId_
    }

-- | Describes updates for an input processing configuration.
iuInputProcessingConfigurationUpdate :: Lens' InputUpdate (Maybe InputProcessingConfigurationUpdate)
iuInputProcessingConfigurationUpdate = lens _iuInputProcessingConfigurationUpdate (\s a -> s {_iuInputProcessingConfigurationUpdate = a})

-- | If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
iuKinesisStreamsInputUpdate :: Lens' InputUpdate (Maybe KinesisStreamsInputUpdate)
iuKinesisStreamsInputUpdate = lens _iuKinesisStreamsInputUpdate (\s a -> s {_iuKinesisStreamsInputUpdate = a})

-- | Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
iuInputParallelismUpdate :: Lens' InputUpdate (Maybe InputParallelismUpdate)
iuInputParallelismUpdate = lens _iuInputParallelismUpdate (\s a -> s {_iuInputParallelismUpdate = a})

-- | Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
iuNamePrefixUpdate :: Lens' InputUpdate (Maybe Text)
iuNamePrefixUpdate = lens _iuNamePrefixUpdate (\s a -> s {_iuNamePrefixUpdate = a})

-- | Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
iuInputSchemaUpdate :: Lens' InputUpdate (Maybe InputSchemaUpdate)
iuInputSchemaUpdate = lens _iuInputSchemaUpdate (\s a -> s {_iuInputSchemaUpdate = a})

-- | If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
iuKinesisFirehoseInputUpdate :: Lens' InputUpdate (Maybe KinesisFirehoseInputUpdate)
iuKinesisFirehoseInputUpdate = lens _iuKinesisFirehoseInputUpdate (\s a -> s {_iuKinesisFirehoseInputUpdate = a})

-- | Input ID of the application input to be updated.
iuInputId :: Lens' InputUpdate Text
iuInputId = lens _iuInputId (\s a -> s {_iuInputId = a})

instance Hashable InputUpdate

instance NFData InputUpdate

instance ToJSON InputUpdate where
  toJSON InputUpdate' {..} =
    object
      ( catMaybes
          [ ("InputProcessingConfigurationUpdate" .=)
              <$> _iuInputProcessingConfigurationUpdate,
            ("KinesisStreamsInputUpdate" .=) <$> _iuKinesisStreamsInputUpdate,
            ("InputParallelismUpdate" .=) <$> _iuInputParallelismUpdate,
            ("NamePrefixUpdate" .=) <$> _iuNamePrefixUpdate,
            ("InputSchemaUpdate" .=) <$> _iuInputSchemaUpdate,
            ("KinesisFirehoseInputUpdate" .=)
              <$> _iuKinesisFirehoseInputUpdate,
            Just ("InputId" .= _iuInputId)
          ]
      )
