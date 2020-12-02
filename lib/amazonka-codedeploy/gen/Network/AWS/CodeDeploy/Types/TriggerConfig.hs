{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TriggerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TriggerConfig where

import Network.AWS.CodeDeploy.Types.TriggerEventType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about notification triggers for the deployment group.
--
--
--
-- /See:/ 'triggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { _tcTriggerName ::
      !(Maybe Text),
    _tcTriggerEvents :: !(Maybe [TriggerEventType]),
    _tcTriggerTargetARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TriggerConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcTriggerName' - The name of the notification trigger.
--
-- * 'tcTriggerEvents' - The event type or types for which notifications are triggered.
--
-- * 'tcTriggerTargetARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
triggerConfig ::
  TriggerConfig
triggerConfig =
  TriggerConfig'
    { _tcTriggerName = Nothing,
      _tcTriggerEvents = Nothing,
      _tcTriggerTargetARN = Nothing
    }

-- | The name of the notification trigger.
tcTriggerName :: Lens' TriggerConfig (Maybe Text)
tcTriggerName = lens _tcTriggerName (\s a -> s {_tcTriggerName = a})

-- | The event type or types for which notifications are triggered.
tcTriggerEvents :: Lens' TriggerConfig [TriggerEventType]
tcTriggerEvents = lens _tcTriggerEvents (\s a -> s {_tcTriggerEvents = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
tcTriggerTargetARN :: Lens' TriggerConfig (Maybe Text)
tcTriggerTargetARN = lens _tcTriggerTargetARN (\s a -> s {_tcTriggerTargetARN = a})

instance FromJSON TriggerConfig where
  parseJSON =
    withObject
      "TriggerConfig"
      ( \x ->
          TriggerConfig'
            <$> (x .:? "triggerName")
            <*> (x .:? "triggerEvents" .!= mempty)
            <*> (x .:? "triggerTargetArn")
      )

instance Hashable TriggerConfig

instance NFData TriggerConfig

instance ToJSON TriggerConfig where
  toJSON TriggerConfig' {..} =
    object
      ( catMaybes
          [ ("triggerName" .=) <$> _tcTriggerName,
            ("triggerEvents" .=) <$> _tcTriggerEvents,
            ("triggerTargetArn" .=) <$> _tcTriggerTargetARN
          ]
      )
