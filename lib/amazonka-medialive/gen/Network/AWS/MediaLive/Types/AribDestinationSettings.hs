{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AribDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AribDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Arib Destination Settings
--
-- /See:/ 'aribDestinationSettings' smart constructor.
data AribDestinationSettings = AribDestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AribDestinationSettings' with the minimum fields required to make a request.
aribDestinationSettings ::
  AribDestinationSettings
aribDestinationSettings = AribDestinationSettings'

instance FromJSON AribDestinationSettings where
  parseJSON =
    withObject
      "AribDestinationSettings"
      (\x -> pure AribDestinationSettings')

instance Hashable AribDestinationSettings

instance NFData AribDestinationSettings

instance ToJSON AribDestinationSettings where
  toJSON = const (Object mempty)
