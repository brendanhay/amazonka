{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Media Package Output Settings
--
-- /See:/ 'mediaPackageOutputSettings' smart constructor.
data MediaPackageOutputSettings = MediaPackageOutputSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MediaPackageOutputSettings' with the minimum fields required to make a request.
mediaPackageOutputSettings ::
  MediaPackageOutputSettings
mediaPackageOutputSettings = MediaPackageOutputSettings'

instance FromJSON MediaPackageOutputSettings where
  parseJSON =
    withObject
      "MediaPackageOutputSettings"
      (\x -> pure MediaPackageOutputSettings')

instance Hashable MediaPackageOutputSettings

instance NFData MediaPackageOutputSettings

instance ToJSON MediaPackageOutputSettings where
  toJSON = const (Object mempty)
