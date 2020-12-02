{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Embedded Destination Settings
--
-- /See:/ 'embeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmbeddedDestinationSettings' with the minimum fields required to make a request.
embeddedDestinationSettings ::
  EmbeddedDestinationSettings
embeddedDestinationSettings = EmbeddedDestinationSettings'

instance FromJSON EmbeddedDestinationSettings where
  parseJSON =
    withObject
      "EmbeddedDestinationSettings"
      (\x -> pure EmbeddedDestinationSettings')

instance Hashable EmbeddedDestinationSettings

instance NFData EmbeddedDestinationSettings

instance ToJSON EmbeddedDestinationSettings where
  toJSON = const (Object mempty)
