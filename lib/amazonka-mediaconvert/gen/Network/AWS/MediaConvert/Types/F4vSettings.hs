{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.F4vSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.F4vSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.F4vMoovPlacement
import Network.AWS.Prelude

-- | Settings for F4v container
--
-- /See:/ 'f4vSettings' smart constructor.
newtype F4vSettings = F4vSettings'
  { _fsMoovPlacement ::
      Maybe F4vMoovPlacement
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'F4vSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsMoovPlacement' - If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
f4vSettings ::
  F4vSettings
f4vSettings = F4vSettings' {_fsMoovPlacement = Nothing}

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
fsMoovPlacement :: Lens' F4vSettings (Maybe F4vMoovPlacement)
fsMoovPlacement = lens _fsMoovPlacement (\s a -> s {_fsMoovPlacement = a})

instance FromJSON F4vSettings where
  parseJSON =
    withObject
      "F4vSettings"
      (\x -> F4vSettings' <$> (x .:? "moovPlacement"))

instance Hashable F4vSettings

instance NFData F4vSettings

instance ToJSON F4vSettings where
  toJSON F4vSettings' {..} =
    object (catMaybes [("moovPlacement" .=) <$> _fsMoovPlacement])
