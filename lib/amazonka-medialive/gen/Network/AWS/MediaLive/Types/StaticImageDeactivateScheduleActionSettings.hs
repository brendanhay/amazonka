{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the action to deactivate the image in a specific layer.
--
-- /See:/ 'staticImageDeactivateScheduleActionSettings' smart constructor.
data StaticImageDeactivateScheduleActionSettings = StaticImageDeactivateScheduleActionSettings'
  { _sidsasFadeOut ::
      !( Maybe
           Nat
       ),
    _sidsasLayer ::
      !( Maybe
           Nat
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StaticImageDeactivateScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sidsasFadeOut' - The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
--
-- * 'sidsasLayer' - The image overlay layer to deactivate, 0 to 7. Default is 0.
staticImageDeactivateScheduleActionSettings ::
  StaticImageDeactivateScheduleActionSettings
staticImageDeactivateScheduleActionSettings =
  StaticImageDeactivateScheduleActionSettings'
    { _sidsasFadeOut =
        Nothing,
      _sidsasLayer = Nothing
    }

-- | The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
sidsasFadeOut :: Lens' StaticImageDeactivateScheduleActionSettings (Maybe Natural)
sidsasFadeOut = lens _sidsasFadeOut (\s a -> s {_sidsasFadeOut = a}) . mapping _Nat

-- | The image overlay layer to deactivate, 0 to 7. Default is 0.
sidsasLayer :: Lens' StaticImageDeactivateScheduleActionSettings (Maybe Natural)
sidsasLayer = lens _sidsasLayer (\s a -> s {_sidsasLayer = a}) . mapping _Nat

instance FromJSON StaticImageDeactivateScheduleActionSettings where
  parseJSON =
    withObject
      "StaticImageDeactivateScheduleActionSettings"
      ( \x ->
          StaticImageDeactivateScheduleActionSettings'
            <$> (x .:? "fadeOut") <*> (x .:? "layer")
      )

instance Hashable StaticImageDeactivateScheduleActionSettings

instance NFData StaticImageDeactivateScheduleActionSettings

instance ToJSON StaticImageDeactivateScheduleActionSettings where
  toJSON StaticImageDeactivateScheduleActionSettings' {..} =
    object
      ( catMaybes
          [("fadeOut" .=) <$> _sidsasFadeOut, ("layer" .=) <$> _sidsasLayer]
      )
