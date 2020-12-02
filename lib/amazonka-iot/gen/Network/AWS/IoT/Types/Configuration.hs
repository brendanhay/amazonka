{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Configuration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration.
--
--
--
-- /See:/ 'configuration' smart constructor.
newtype Configuration = Configuration' {_cEnabled :: Maybe Bool}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cEnabled' - True to enable the configuration.
configuration ::
  Configuration
configuration = Configuration' {_cEnabled = Nothing}

-- | True to enable the configuration.
cEnabled :: Lens' Configuration (Maybe Bool)
cEnabled = lens _cEnabled (\s a -> s {_cEnabled = a})

instance FromJSON Configuration where
  parseJSON =
    withObject
      "Configuration"
      (\x -> Configuration' <$> (x .:? "Enabled"))

instance Hashable Configuration

instance NFData Configuration

instance ToJSON Configuration where
  toJSON Configuration' {..} =
    object (catMaybes [("Enabled" .=) <$> _cEnabled])
