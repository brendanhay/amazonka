{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WebvttDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WebvttDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Webvtt Destination Settings
--
-- /See:/ 'webvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebvttDestinationSettings' with the minimum fields required to make a request.
webvttDestinationSettings ::
  WebvttDestinationSettings
webvttDestinationSettings = WebvttDestinationSettings'

instance FromJSON WebvttDestinationSettings where
  parseJSON =
    withObject
      "WebvttDestinationSettings"
      (\x -> pure WebvttDestinationSettings')

instance Hashable WebvttDestinationSettings

instance NFData WebvttDestinationSettings

instance ToJSON WebvttDestinationSettings where
  toJSON = const (Object mempty)
