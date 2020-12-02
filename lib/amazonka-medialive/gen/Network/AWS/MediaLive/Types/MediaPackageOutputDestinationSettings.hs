{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | MediaPackage Output Destination Settings
--
-- /See:/ 'mediaPackageOutputDestinationSettings' smart constructor.
newtype MediaPackageOutputDestinationSettings = MediaPackageOutputDestinationSettings'
  { _mpodsChannelId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MediaPackageOutputDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpodsChannelId' - ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
mediaPackageOutputDestinationSettings ::
  MediaPackageOutputDestinationSettings
mediaPackageOutputDestinationSettings =
  MediaPackageOutputDestinationSettings' {_mpodsChannelId = Nothing}

-- | ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
mpodsChannelId :: Lens' MediaPackageOutputDestinationSettings (Maybe Text)
mpodsChannelId = lens _mpodsChannelId (\s a -> s {_mpodsChannelId = a})

instance FromJSON MediaPackageOutputDestinationSettings where
  parseJSON =
    withObject
      "MediaPackageOutputDestinationSettings"
      ( \x ->
          MediaPackageOutputDestinationSettings' <$> (x .:? "channelId")
      )

instance Hashable MediaPackageOutputDestinationSettings

instance NFData MediaPackageOutputDestinationSettings

instance ToJSON MediaPackageOutputDestinationSettings where
  toJSON MediaPackageOutputDestinationSettings' {..} =
    object (catMaybes [("channelId" .=) <$> _mpodsChannelId])
