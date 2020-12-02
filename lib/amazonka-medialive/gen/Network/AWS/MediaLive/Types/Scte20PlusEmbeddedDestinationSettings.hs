{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Scte20 Plus Embedded Destination Settings
--
-- /See:/ 'scte20PlusEmbeddedDestinationSettings' smart constructor.
data Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte20PlusEmbeddedDestinationSettings' with the minimum fields required to make a request.
scte20PlusEmbeddedDestinationSettings ::
  Scte20PlusEmbeddedDestinationSettings
scte20PlusEmbeddedDestinationSettings =
  Scte20PlusEmbeddedDestinationSettings'

instance FromJSON Scte20PlusEmbeddedDestinationSettings where
  parseJSON =
    withObject
      "Scte20PlusEmbeddedDestinationSettings"
      (\x -> pure Scte20PlusEmbeddedDestinationSettings')

instance Hashable Scte20PlusEmbeddedDestinationSettings

instance NFData Scte20PlusEmbeddedDestinationSettings

instance ToJSON Scte20PlusEmbeddedDestinationSettings where
  toJSON = const (Object mempty)
