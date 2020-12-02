{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TeletextDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TeletextDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Teletext Destination Settings
--
-- /See:/ 'teletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TeletextDestinationSettings' with the minimum fields required to make a request.
teletextDestinationSettings ::
  TeletextDestinationSettings
teletextDestinationSettings = TeletextDestinationSettings'

instance FromJSON TeletextDestinationSettings where
  parseJSON =
    withObject
      "TeletextDestinationSettings"
      (\x -> pure TeletextDestinationSettings')

instance Hashable TeletextDestinationSettings

instance NFData TeletextDestinationSettings

instance ToJSON TeletextDestinationSettings where
  toJSON = const (Object mempty)
