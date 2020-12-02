{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Hdr10Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Hdr10Settings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Hdr10 Settings
--
-- /See:/ 'hdr10Settings' smart constructor.
data Hdr10Settings = Hdr10Settings'
  { _hsMaxFall :: !(Maybe Nat),
    _hsMaxCll :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Hdr10Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsMaxFall' - Maximum Frame Average Light Level An integer metadata value defining the maximum average light level, in nits, for any single frame within an encoded HDR video stream or file.
--
-- * 'hsMaxCll' - Maximum Content Light Level An integer metadata value defining the maximum light level, in nits, of any single pixel within an encoded HDR video stream or file.
hdr10Settings ::
  Hdr10Settings
hdr10Settings =
  Hdr10Settings' {_hsMaxFall = Nothing, _hsMaxCll = Nothing}

-- | Maximum Frame Average Light Level An integer metadata value defining the maximum average light level, in nits, for any single frame within an encoded HDR video stream or file.
hsMaxFall :: Lens' Hdr10Settings (Maybe Natural)
hsMaxFall = lens _hsMaxFall (\s a -> s {_hsMaxFall = a}) . mapping _Nat

-- | Maximum Content Light Level An integer metadata value defining the maximum light level, in nits, of any single pixel within an encoded HDR video stream or file.
hsMaxCll :: Lens' Hdr10Settings (Maybe Natural)
hsMaxCll = lens _hsMaxCll (\s a -> s {_hsMaxCll = a}) . mapping _Nat

instance FromJSON Hdr10Settings where
  parseJSON =
    withObject
      "Hdr10Settings"
      (\x -> Hdr10Settings' <$> (x .:? "maxFall") <*> (x .:? "maxCll"))

instance Hashable Hdr10Settings

instance NFData Hdr10Settings

instance ToJSON Hdr10Settings where
  toJSON Hdr10Settings' {..} =
    object
      ( catMaybes
          [("maxFall" .=) <$> _hsMaxFall, ("maxCll" .=) <$> _hsMaxCll]
      )
