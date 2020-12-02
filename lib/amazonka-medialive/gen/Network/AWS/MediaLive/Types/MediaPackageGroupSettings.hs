{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.Prelude

-- | Media Package Group Settings
--
-- /See:/ 'mediaPackageGroupSettings' smart constructor.
newtype MediaPackageGroupSettings = MediaPackageGroupSettings'
  { _mpgsDestination ::
      OutputLocationRef
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MediaPackageGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpgsDestination' - MediaPackage channel destination.
mediaPackageGroupSettings ::
  -- | 'mpgsDestination'
  OutputLocationRef ->
  MediaPackageGroupSettings
mediaPackageGroupSettings pDestination_ =
  MediaPackageGroupSettings' {_mpgsDestination = pDestination_}

-- | MediaPackage channel destination.
mpgsDestination :: Lens' MediaPackageGroupSettings OutputLocationRef
mpgsDestination = lens _mpgsDestination (\s a -> s {_mpgsDestination = a})

instance FromJSON MediaPackageGroupSettings where
  parseJSON =
    withObject
      "MediaPackageGroupSettings"
      (\x -> MediaPackageGroupSettings' <$> (x .: "destination"))

instance Hashable MediaPackageGroupSettings

instance NFData MediaPackageGroupSettings

instance ToJSON MediaPackageGroupSettings where
  toJSON MediaPackageGroupSettings' {..} =
    object (catMaybes [Just ("destination" .= _mpgsDestination)])
