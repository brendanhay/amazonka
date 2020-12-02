{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputChannelMapping where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | OutputChannel mapping settings.
--
-- /See:/ 'outputChannelMapping' smart constructor.
newtype OutputChannelMapping = OutputChannelMapping'
  { _ocmInputChannels ::
      Maybe [Int]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputChannelMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocmInputChannels' - List of input channels
outputChannelMapping ::
  OutputChannelMapping
outputChannelMapping =
  OutputChannelMapping' {_ocmInputChannels = Nothing}

-- | List of input channels
ocmInputChannels :: Lens' OutputChannelMapping [Int]
ocmInputChannels = lens _ocmInputChannels (\s a -> s {_ocmInputChannels = a}) . _Default . _Coerce

instance FromJSON OutputChannelMapping where
  parseJSON =
    withObject
      "OutputChannelMapping"
      ( \x ->
          OutputChannelMapping' <$> (x .:? "inputChannels" .!= mempty)
      )

instance Hashable OutputChannelMapping

instance NFData OutputChannelMapping

instance ToJSON OutputChannelMapping where
  toJSON OutputChannelMapping' {..} =
    object (catMaybes [("inputChannels" .=) <$> _ocmInputChannels])
