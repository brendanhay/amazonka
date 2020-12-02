{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Passthrough applies no color space conversion to the output
--
-- /See:/ 'colorSpacePassthroughSettings' smart constructor.
data ColorSpacePassthroughSettings = ColorSpacePassthroughSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ColorSpacePassthroughSettings' with the minimum fields required to make a request.
colorSpacePassthroughSettings ::
  ColorSpacePassthroughSettings
colorSpacePassthroughSettings = ColorSpacePassthroughSettings'

instance FromJSON ColorSpacePassthroughSettings where
  parseJSON =
    withObject
      "ColorSpacePassthroughSettings"
      (\x -> pure ColorSpacePassthroughSettings')

instance Hashable ColorSpacePassthroughSettings

instance NFData ColorSpacePassthroughSettings

instance ToJSON ColorSpacePassthroughSettings where
  toJSON = const (Object mempty)
