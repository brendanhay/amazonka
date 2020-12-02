{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ClientProperties where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.ReconnectEnum

-- | Describes an Amazon WorkSpaces client.
--
--
--
-- /See:/ 'clientProperties' smart constructor.
newtype ClientProperties = ClientProperties'
  { _cpReconnectEnabled ::
      Maybe ReconnectEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpReconnectEnabled' - Specifies whether users can cache their credentials on the Amazon WorkSpaces client. When enabled, users can choose to reconnect to their WorkSpaces without re-entering their credentials.
clientProperties ::
  ClientProperties
clientProperties = ClientProperties' {_cpReconnectEnabled = Nothing}

-- | Specifies whether users can cache their credentials on the Amazon WorkSpaces client. When enabled, users can choose to reconnect to their WorkSpaces without re-entering their credentials.
cpReconnectEnabled :: Lens' ClientProperties (Maybe ReconnectEnum)
cpReconnectEnabled = lens _cpReconnectEnabled (\s a -> s {_cpReconnectEnabled = a})

instance FromJSON ClientProperties where
  parseJSON =
    withObject
      "ClientProperties"
      (\x -> ClientProperties' <$> (x .:? "ReconnectEnabled"))

instance Hashable ClientProperties

instance NFData ClientProperties

instance ToJSON ClientProperties where
  toJSON ClientProperties' {..} =
    object
      (catMaybes [("ReconnectEnabled" .=) <$> _cpReconnectEnabled])
