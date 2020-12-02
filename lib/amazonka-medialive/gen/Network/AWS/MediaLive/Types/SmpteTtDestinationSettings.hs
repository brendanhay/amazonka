{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmpteTtDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Smpte Tt Destination Settings
--
-- /See:/ 'smpteTtDestinationSettings' smart constructor.
data SmpteTtDestinationSettings = SmpteTtDestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SmpteTtDestinationSettings' with the minimum fields required to make a request.
smpteTtDestinationSettings ::
  SmpteTtDestinationSettings
smpteTtDestinationSettings = SmpteTtDestinationSettings'

instance FromJSON SmpteTtDestinationSettings where
  parseJSON =
    withObject
      "SmpteTtDestinationSettings"
      (\x -> pure SmpteTtDestinationSettings')

instance Hashable SmpteTtDestinationSettings

instance NFData SmpteTtDestinationSettings

instance ToJSON SmpteTtDestinationSettings where
  toJSON = const (Object mempty)
