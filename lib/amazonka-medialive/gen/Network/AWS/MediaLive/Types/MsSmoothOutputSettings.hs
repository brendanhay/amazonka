{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothOutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
import Network.AWS.Prelude

-- | Ms Smooth Output Settings
--
-- /See:/ 'msSmoothOutputSettings' smart constructor.
data MsSmoothOutputSettings = MsSmoothOutputSettings'
  { _msosH265PackagingType ::
      !(Maybe MsSmoothH265PackagingType),
    _msosNameModifier :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MsSmoothOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msosH265PackagingType' - Only applicable when this output is referencing an H.265 video description. Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
--
-- * 'msosNameModifier' - String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
msSmoothOutputSettings ::
  MsSmoothOutputSettings
msSmoothOutputSettings =
  MsSmoothOutputSettings'
    { _msosH265PackagingType = Nothing,
      _msosNameModifier = Nothing
    }

-- | Only applicable when this output is referencing an H.265 video description. Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
msosH265PackagingType :: Lens' MsSmoothOutputSettings (Maybe MsSmoothH265PackagingType)
msosH265PackagingType = lens _msosH265PackagingType (\s a -> s {_msosH265PackagingType = a})

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
msosNameModifier :: Lens' MsSmoothOutputSettings (Maybe Text)
msosNameModifier = lens _msosNameModifier (\s a -> s {_msosNameModifier = a})

instance FromJSON MsSmoothOutputSettings where
  parseJSON =
    withObject
      "MsSmoothOutputSettings"
      ( \x ->
          MsSmoothOutputSettings'
            <$> (x .:? "h265PackagingType") <*> (x .:? "nameModifier")
      )

instance Hashable MsSmoothOutputSettings

instance NFData MsSmoothOutputSettings

instance ToJSON MsSmoothOutputSettings where
  toJSON MsSmoothOutputSettings' {..} =
    object
      ( catMaybes
          [ ("h265PackagingType" .=) <$> _msosH265PackagingType,
            ("nameModifier" .=) <$> _msosNameModifier
          ]
      )
