{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.QueueConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.QueueConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | Specifies the configuration for publishing messages to an Amazon Simple Queue Service (Amazon SQS) queue when Amazon S3 detects specified events.
--
--
--
-- /See:/ 'queueConfiguration' smart constructor.
data QueueConfiguration = QueueConfiguration'
  { _qcId ::
      !(Maybe Text),
    _qcFilter :: !(Maybe NotificationConfigurationFilter),
    _qcQueueARN :: !Text,
    _qcEvents :: ![Event]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueueConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qcId' - Undocumented member.
--
-- * 'qcFilter' - Undocumented member.
--
-- * 'qcQueueARN' - The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon S3 publishes a message when it detects events of the specified type.
--
-- * 'qcEvents' - A collection of bucket events for which to send notifications
queueConfiguration ::
  -- | 'qcQueueARN'
  Text ->
  QueueConfiguration
queueConfiguration pQueueARN_ =
  QueueConfiguration'
    { _qcId = Nothing,
      _qcFilter = Nothing,
      _qcQueueARN = pQueueARN_,
      _qcEvents = mempty
    }

-- | Undocumented member.
qcId :: Lens' QueueConfiguration (Maybe Text)
qcId = lens _qcId (\s a -> s {_qcId = a})

-- | Undocumented member.
qcFilter :: Lens' QueueConfiguration (Maybe NotificationConfigurationFilter)
qcFilter = lens _qcFilter (\s a -> s {_qcFilter = a})

-- | The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon S3 publishes a message when it detects events of the specified type.
qcQueueARN :: Lens' QueueConfiguration Text
qcQueueARN = lens _qcQueueARN (\s a -> s {_qcQueueARN = a})

-- | A collection of bucket events for which to send notifications
qcEvents :: Lens' QueueConfiguration [Event]
qcEvents = lens _qcEvents (\s a -> s {_qcEvents = a}) . _Coerce

instance FromXML QueueConfiguration where
  parseXML x =
    QueueConfiguration'
      <$> (x .@? "Id")
      <*> (x .@? "Filter")
      <*> (x .@ "Queue")
      <*> (parseXMLList "Event" x)

instance Hashable QueueConfiguration

instance NFData QueueConfiguration

instance ToXML QueueConfiguration where
  toXML QueueConfiguration' {..} =
    mconcat
      [ "Id" @= _qcId,
        "Filter" @= _qcFilter,
        "Queue" @= _qcQueueARN,
        toXMLList "Event" _qcEvents
      ]
