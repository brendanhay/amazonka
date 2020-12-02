{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration where

import Network.AWS.CodeDeploy.Types.AutoRollbackEvent
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment is not completed successfully.
--
--
--
-- /See:/ 'autoRollbackConfiguration' smart constructor.
data AutoRollbackConfiguration = AutoRollbackConfiguration'
  { _arcEnabled ::
      !(Maybe Bool),
    _arcEvents ::
      !(Maybe [AutoRollbackEvent])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoRollbackConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arcEnabled' - Indicates whether a defined automatic rollback configuration is currently enabled.
--
-- * 'arcEvents' - The event type or types that trigger a rollback.
autoRollbackConfiguration ::
  AutoRollbackConfiguration
autoRollbackConfiguration =
  AutoRollbackConfiguration'
    { _arcEnabled = Nothing,
      _arcEvents = Nothing
    }

-- | Indicates whether a defined automatic rollback configuration is currently enabled.
arcEnabled :: Lens' AutoRollbackConfiguration (Maybe Bool)
arcEnabled = lens _arcEnabled (\s a -> s {_arcEnabled = a})

-- | The event type or types that trigger a rollback.
arcEvents :: Lens' AutoRollbackConfiguration [AutoRollbackEvent]
arcEvents = lens _arcEvents (\s a -> s {_arcEvents = a}) . _Default . _Coerce

instance FromJSON AutoRollbackConfiguration where
  parseJSON =
    withObject
      "AutoRollbackConfiguration"
      ( \x ->
          AutoRollbackConfiguration'
            <$> (x .:? "enabled") <*> (x .:? "events" .!= mempty)
      )

instance Hashable AutoRollbackConfiguration

instance NFData AutoRollbackConfiguration

instance ToJSON AutoRollbackConfiguration where
  toJSON AutoRollbackConfiguration' {..} =
    object
      ( catMaybes
          [("enabled" .=) <$> _arcEnabled, ("events" .=) <$> _arcEvents]
      )
