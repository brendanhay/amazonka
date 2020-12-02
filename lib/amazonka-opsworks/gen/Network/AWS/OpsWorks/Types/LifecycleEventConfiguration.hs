{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LifecycleEventConfiguration where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import Network.AWS.Prelude

-- | Specifies the lifecycle event configuration
--
--
--
-- /See:/ 'lifecycleEventConfiguration' smart constructor.
newtype LifecycleEventConfiguration = LifecycleEventConfiguration'
  { _lecShutdown ::
      Maybe ShutdownEventConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecycleEventConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lecShutdown' - A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
lifecycleEventConfiguration ::
  LifecycleEventConfiguration
lifecycleEventConfiguration =
  LifecycleEventConfiguration' {_lecShutdown = Nothing}

-- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
lecShutdown :: Lens' LifecycleEventConfiguration (Maybe ShutdownEventConfiguration)
lecShutdown = lens _lecShutdown (\s a -> s {_lecShutdown = a})

instance FromJSON LifecycleEventConfiguration where
  parseJSON =
    withObject
      "LifecycleEventConfiguration"
      (\x -> LifecycleEventConfiguration' <$> (x .:? "Shutdown"))

instance Hashable LifecycleEventConfiguration

instance NFData LifecycleEventConfiguration

instance ToJSON LifecycleEventConfiguration where
  toJSON LifecycleEventConfiguration' {..} =
    object (catMaybes [("Shutdown" .=) <$> _lecShutdown])
