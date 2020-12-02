{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailConfiguration where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AvailSettings
import Network.AWS.Prelude

-- | Avail Configuration
--
-- /See:/ 'availConfiguration' smart constructor.
newtype AvailConfiguration = AvailConfiguration'
  { _acAvailSettings ::
      Maybe AvailSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acAvailSettings' - Ad avail settings.
availConfiguration ::
  AvailConfiguration
availConfiguration =
  AvailConfiguration' {_acAvailSettings = Nothing}

-- | Ad avail settings.
acAvailSettings :: Lens' AvailConfiguration (Maybe AvailSettings)
acAvailSettings = lens _acAvailSettings (\s a -> s {_acAvailSettings = a})

instance FromJSON AvailConfiguration where
  parseJSON =
    withObject
      "AvailConfiguration"
      (\x -> AvailConfiguration' <$> (x .:? "availSettings"))

instance Hashable AvailConfiguration

instance NFData AvailConfiguration

instance ToJSON AvailConfiguration where
  toJSON AvailConfiguration' {..} =
    object (catMaybes [("availSettings" .=) <$> _acAvailSettings])
