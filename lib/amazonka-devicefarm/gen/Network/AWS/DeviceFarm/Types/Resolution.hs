{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Resolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Resolution where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the screen resolution of a device in height and width, expressed in pixels.
--
--
--
-- /See:/ 'resolution' smart constructor.
data Resolution = Resolution'
  { _rHeight :: !(Maybe Int),
    _rWidth :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resolution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHeight' - The screen resolution's height, expressed in pixels.
--
-- * 'rWidth' - The screen resolution's width, expressed in pixels.
resolution ::
  Resolution
resolution = Resolution' {_rHeight = Nothing, _rWidth = Nothing}

-- | The screen resolution's height, expressed in pixels.
rHeight :: Lens' Resolution (Maybe Int)
rHeight = lens _rHeight (\s a -> s {_rHeight = a})

-- | The screen resolution's width, expressed in pixels.
rWidth :: Lens' Resolution (Maybe Int)
rWidth = lens _rWidth (\s a -> s {_rWidth = a})

instance FromJSON Resolution where
  parseJSON =
    withObject
      "Resolution"
      (\x -> Resolution' <$> (x .:? "height") <*> (x .:? "width"))

instance Hashable Resolution

instance NFData Resolution
