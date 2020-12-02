{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte27DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte27DestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Scte27 Destination Settings
--
-- /See:/ 'scte27DestinationSettings' smart constructor.
data Scte27DestinationSettings = Scte27DestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte27DestinationSettings' with the minimum fields required to make a request.
scte27DestinationSettings ::
  Scte27DestinationSettings
scte27DestinationSettings = Scte27DestinationSettings'

instance FromJSON Scte27DestinationSettings where
  parseJSON =
    withObject
      "Scte27DestinationSettings"
      (\x -> pure Scte27DestinationSettings')

instance Hashable Scte27DestinationSettings

instance NFData Scte27DestinationSettings

instance ToJSON Scte27DestinationSettings where
  toJSON = const (Object mempty)
