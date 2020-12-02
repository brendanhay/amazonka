{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IotEventsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IotEventsAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Sends an input to an AWS IoT Events detector.
--
--
--
-- /See:/ 'iotEventsAction' smart constructor.
data IotEventsAction = IotEventsAction'
  { _ieaBatchMode ::
      !(Maybe Bool),
    _ieaMessageId :: !(Maybe Text),
    _ieaInputName :: !Text,
    _ieaRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IotEventsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieaBatchMode' - Whether to process the event actions as a batch. The default value is @false@ . When @batchMode@ is @true@ , you can't specify a @messageId@ .  When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is treated as a separate message when it's sent to AWS IoT Events by calling <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html @BatchPutMessage@ > . The resulting array can't have more than 10 messages.
--
-- * 'ieaMessageId' - The ID of the message. The default @messageId@ is a new UUID value. When @batchMode@ is @true@ , you can't specify a @messageId@ --a new UUID value will be assigned. Assign a value to this property to ensure that only one input (message) with a given @messageId@ will be processed by an AWS IoT Events detector.
--
-- * 'ieaInputName' - The name of the AWS IoT Events input.
--
-- * 'ieaRoleARN' - The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
iotEventsAction ::
  -- | 'ieaInputName'
  Text ->
  -- | 'ieaRoleARN'
  Text ->
  IotEventsAction
iotEventsAction pInputName_ pRoleARN_ =
  IotEventsAction'
    { _ieaBatchMode = Nothing,
      _ieaMessageId = Nothing,
      _ieaInputName = pInputName_,
      _ieaRoleARN = pRoleARN_
    }

-- | Whether to process the event actions as a batch. The default value is @false@ . When @batchMode@ is @true@ , you can't specify a @messageId@ .  When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is treated as a separate message when it's sent to AWS IoT Events by calling <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html @BatchPutMessage@ > . The resulting array can't have more than 10 messages.
ieaBatchMode :: Lens' IotEventsAction (Maybe Bool)
ieaBatchMode = lens _ieaBatchMode (\s a -> s {_ieaBatchMode = a})

-- | The ID of the message. The default @messageId@ is a new UUID value. When @batchMode@ is @true@ , you can't specify a @messageId@ --a new UUID value will be assigned. Assign a value to this property to ensure that only one input (message) with a given @messageId@ will be processed by an AWS IoT Events detector.
ieaMessageId :: Lens' IotEventsAction (Maybe Text)
ieaMessageId = lens _ieaMessageId (\s a -> s {_ieaMessageId = a})

-- | The name of the AWS IoT Events input.
ieaInputName :: Lens' IotEventsAction Text
ieaInputName = lens _ieaInputName (\s a -> s {_ieaInputName = a})

-- | The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
ieaRoleARN :: Lens' IotEventsAction Text
ieaRoleARN = lens _ieaRoleARN (\s a -> s {_ieaRoleARN = a})

instance FromJSON IotEventsAction where
  parseJSON =
    withObject
      "IotEventsAction"
      ( \x ->
          IotEventsAction'
            <$> (x .:? "batchMode")
            <*> (x .:? "messageId")
            <*> (x .: "inputName")
            <*> (x .: "roleArn")
      )

instance Hashable IotEventsAction

instance NFData IotEventsAction

instance ToJSON IotEventsAction where
  toJSON IotEventsAction' {..} =
    object
      ( catMaybes
          [ ("batchMode" .=) <$> _ieaBatchMode,
            ("messageId" .=) <$> _ieaMessageId,
            Just ("inputName" .= _ieaInputName),
            Just ("roleArn" .= _ieaRoleARN)
          ]
      )
