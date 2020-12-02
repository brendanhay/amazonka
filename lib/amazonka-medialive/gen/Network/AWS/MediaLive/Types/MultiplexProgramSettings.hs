{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
import Network.AWS.MediaLive.Types.MultiplexVideoSettings
import Network.AWS.MediaLive.Types.PreferredChannelPipeline
import Network.AWS.Prelude

-- | Multiplex Program settings configuration.
--
-- /See:/ 'multiplexProgramSettings' smart constructor.
data MultiplexProgramSettings = MultiplexProgramSettings'
  { _mpsPreferredChannelPipeline ::
      !(Maybe PreferredChannelPipeline),
    _mpsVideoSettings ::
      !(Maybe MultiplexVideoSettings),
    _mpsServiceDescriptor ::
      !( Maybe
           MultiplexProgramServiceDescriptor
       ),
    _mpsProgramNumber :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexProgramSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsPreferredChannelPipeline' - Indicates which pipeline is preferred by the multiplex for program ingest.
--
-- * 'mpsVideoSettings' - Program video settings configuration.
--
-- * 'mpsServiceDescriptor' - Transport stream service descriptor configuration for the Multiplex program.
--
-- * 'mpsProgramNumber' - Unique program number.
multiplexProgramSettings ::
  -- | 'mpsProgramNumber'
  Natural ->
  MultiplexProgramSettings
multiplexProgramSettings pProgramNumber_ =
  MultiplexProgramSettings'
    { _mpsPreferredChannelPipeline = Nothing,
      _mpsVideoSettings = Nothing,
      _mpsServiceDescriptor = Nothing,
      _mpsProgramNumber = _Nat # pProgramNumber_
    }

-- | Indicates which pipeline is preferred by the multiplex for program ingest.
mpsPreferredChannelPipeline :: Lens' MultiplexProgramSettings (Maybe PreferredChannelPipeline)
mpsPreferredChannelPipeline = lens _mpsPreferredChannelPipeline (\s a -> s {_mpsPreferredChannelPipeline = a})

-- | Program video settings configuration.
mpsVideoSettings :: Lens' MultiplexProgramSettings (Maybe MultiplexVideoSettings)
mpsVideoSettings = lens _mpsVideoSettings (\s a -> s {_mpsVideoSettings = a})

-- | Transport stream service descriptor configuration for the Multiplex program.
mpsServiceDescriptor :: Lens' MultiplexProgramSettings (Maybe MultiplexProgramServiceDescriptor)
mpsServiceDescriptor = lens _mpsServiceDescriptor (\s a -> s {_mpsServiceDescriptor = a})

-- | Unique program number.
mpsProgramNumber :: Lens' MultiplexProgramSettings Natural
mpsProgramNumber = lens _mpsProgramNumber (\s a -> s {_mpsProgramNumber = a}) . _Nat

instance FromJSON MultiplexProgramSettings where
  parseJSON =
    withObject
      "MultiplexProgramSettings"
      ( \x ->
          MultiplexProgramSettings'
            <$> (x .:? "preferredChannelPipeline")
            <*> (x .:? "videoSettings")
            <*> (x .:? "serviceDescriptor")
            <*> (x .: "programNumber")
      )

instance Hashable MultiplexProgramSettings

instance NFData MultiplexProgramSettings

instance ToJSON MultiplexProgramSettings where
  toJSON MultiplexProgramSettings' {..} =
    object
      ( catMaybes
          [ ("preferredChannelPipeline" .=) <$> _mpsPreferredChannelPipeline,
            ("videoSettings" .=) <$> _mpsVideoSettings,
            ("serviceDescriptor" .=) <$> _mpsServiceDescriptor,
            Just ("programNumber" .= _mpsProgramNumber)
          ]
      )
