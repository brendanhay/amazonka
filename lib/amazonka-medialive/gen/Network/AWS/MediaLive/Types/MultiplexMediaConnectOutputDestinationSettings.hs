{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Multiplex MediaConnect output destination settings.
--
-- /See:/ 'multiplexMediaConnectOutputDestinationSettings' smart constructor.
newtype MultiplexMediaConnectOutputDestinationSettings = MultiplexMediaConnectOutputDestinationSettings'
  { _mmcodsEntitlementARN ::
      Maybe
        Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'MultiplexMediaConnectOutputDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mmcodsEntitlementARN' - The MediaConnect entitlement ARN available as a Flow source.
multiplexMediaConnectOutputDestinationSettings ::
  MultiplexMediaConnectOutputDestinationSettings
multiplexMediaConnectOutputDestinationSettings =
  MultiplexMediaConnectOutputDestinationSettings'
    { _mmcodsEntitlementARN =
        Nothing
    }

-- | The MediaConnect entitlement ARN available as a Flow source.
mmcodsEntitlementARN :: Lens' MultiplexMediaConnectOutputDestinationSettings (Maybe Text)
mmcodsEntitlementARN = lens _mmcodsEntitlementARN (\s a -> s {_mmcodsEntitlementARN = a})

instance FromJSON MultiplexMediaConnectOutputDestinationSettings where
  parseJSON =
    withObject
      "MultiplexMediaConnectOutputDestinationSettings"
      ( \x ->
          MultiplexMediaConnectOutputDestinationSettings'
            <$> (x .:? "entitlementArn")
      )

instance Hashable MultiplexMediaConnectOutputDestinationSettings

instance NFData MultiplexMediaConnectOutputDestinationSettings
