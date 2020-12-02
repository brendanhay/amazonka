{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.VaultNotificationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultNotificationConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a vault's notification configuration.
--
--
--
-- /See:/ 'vaultNotificationConfig' smart constructor.
data VaultNotificationConfig = VaultNotificationConfig'
  { _vncSNSTopic ::
      !(Maybe Text),
    _vncEvents :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VaultNotificationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vncSNSTopic' - The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource Name (ARN).
--
-- * 'vncEvents' - A list of one or more events for which Amazon S3 Glacier will send a notification to the specified Amazon SNS topic.
vaultNotificationConfig ::
  VaultNotificationConfig
vaultNotificationConfig =
  VaultNotificationConfig'
    { _vncSNSTopic = Nothing,
      _vncEvents = Nothing
    }

-- | The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource Name (ARN).
vncSNSTopic :: Lens' VaultNotificationConfig (Maybe Text)
vncSNSTopic = lens _vncSNSTopic (\s a -> s {_vncSNSTopic = a})

-- | A list of one or more events for which Amazon S3 Glacier will send a notification to the specified Amazon SNS topic.
vncEvents :: Lens' VaultNotificationConfig [Text]
vncEvents = lens _vncEvents (\s a -> s {_vncEvents = a}) . _Default . _Coerce

instance FromJSON VaultNotificationConfig where
  parseJSON =
    withObject
      "VaultNotificationConfig"
      ( \x ->
          VaultNotificationConfig'
            <$> (x .:? "SNSTopic") <*> (x .:? "Events" .!= mempty)
      )

instance Hashable VaultNotificationConfig

instance NFData VaultNotificationConfig

instance ToJSON VaultNotificationConfig where
  toJSON VaultNotificationConfig' {..} =
    object
      ( catMaybes
          [("SNSTopic" .=) <$> _vncSNSTopic, ("Events" .=) <$> _vncEvents]
      )
