{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Embedded Plus Scte20 Destination Settings
--
-- /See:/ 'embeddedPlusScte20DestinationSettings' smart constructor.
data EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmbeddedPlusScte20DestinationSettings' with the minimum fields required to make a request.
embeddedPlusScte20DestinationSettings ::
  EmbeddedPlusScte20DestinationSettings
embeddedPlusScte20DestinationSettings =
  EmbeddedPlusScte20DestinationSettings'

instance FromJSON EmbeddedPlusScte20DestinationSettings where
  parseJSON =
    withObject
      "EmbeddedPlusScte20DestinationSettings"
      (\x -> pure EmbeddedPlusScte20DestinationSettings')

instance Hashable EmbeddedPlusScte20DestinationSettings

instance NFData EmbeddedPlusScte20DestinationSettings

instance ToJSON EmbeddedPlusScte20DestinationSettings where
  toJSON = const (Object mempty)
