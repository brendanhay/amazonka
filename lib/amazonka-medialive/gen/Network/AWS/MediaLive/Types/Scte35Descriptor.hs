{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35Descriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35Descriptor where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35DescriptorSettings
import Network.AWS.Prelude

-- | Holds one set of SCTE-35 Descriptor Settings.
--
-- /See:/ 'scte35Descriptor' smart constructor.
newtype Scte35Descriptor = Scte35Descriptor'
  { _sdScte35DescriptorSettings ::
      Scte35DescriptorSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35Descriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdScte35DescriptorSettings' - SCTE-35 Descriptor Settings.
scte35Descriptor ::
  -- | 'sdScte35DescriptorSettings'
  Scte35DescriptorSettings ->
  Scte35Descriptor
scte35Descriptor pScte35DescriptorSettings_ =
  Scte35Descriptor'
    { _sdScte35DescriptorSettings =
        pScte35DescriptorSettings_
    }

-- | SCTE-35 Descriptor Settings.
sdScte35DescriptorSettings :: Lens' Scte35Descriptor Scte35DescriptorSettings
sdScte35DescriptorSettings = lens _sdScte35DescriptorSettings (\s a -> s {_sdScte35DescriptorSettings = a})

instance FromJSON Scte35Descriptor where
  parseJSON =
    withObject
      "Scte35Descriptor"
      (\x -> Scte35Descriptor' <$> (x .: "scte35DescriptorSettings"))

instance Hashable Scte35Descriptor

instance NFData Scte35Descriptor

instance ToJSON Scte35Descriptor where
  toJSON Scte35Descriptor' {..} =
    object
      ( catMaybes
          [Just ("scte35DescriptorSettings" .= _sdScte35DescriptorSettings)]
      )
