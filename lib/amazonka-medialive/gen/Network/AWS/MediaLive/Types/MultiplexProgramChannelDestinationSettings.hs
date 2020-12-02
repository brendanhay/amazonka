{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Multiplex Program Input Destination Settings for outputting a Channel to a Multiplex
--
-- /See:/ 'multiplexProgramChannelDestinationSettings' smart constructor.
data MultiplexProgramChannelDestinationSettings = MultiplexProgramChannelDestinationSettings'
  { _mpcdsMultiplexId ::
      !( Maybe
           Text
       ),
    _mpcdsProgramName ::
      !( Maybe
           Text
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

-- | Creates a value of 'MultiplexProgramChannelDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpcdsMultiplexId' - The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances. The Multiplex must be in the same region as the Channel.
--
-- * 'mpcdsProgramName' - The program name of the Multiplex program that the encoder is providing output to.
multiplexProgramChannelDestinationSettings ::
  MultiplexProgramChannelDestinationSettings
multiplexProgramChannelDestinationSettings =
  MultiplexProgramChannelDestinationSettings'
    { _mpcdsMultiplexId =
        Nothing,
      _mpcdsProgramName = Nothing
    }

-- | The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances. The Multiplex must be in the same region as the Channel.
mpcdsMultiplexId :: Lens' MultiplexProgramChannelDestinationSettings (Maybe Text)
mpcdsMultiplexId = lens _mpcdsMultiplexId (\s a -> s {_mpcdsMultiplexId = a})

-- | The program name of the Multiplex program that the encoder is providing output to.
mpcdsProgramName :: Lens' MultiplexProgramChannelDestinationSettings (Maybe Text)
mpcdsProgramName = lens _mpcdsProgramName (\s a -> s {_mpcdsProgramName = a})

instance FromJSON MultiplexProgramChannelDestinationSettings where
  parseJSON =
    withObject
      "MultiplexProgramChannelDestinationSettings"
      ( \x ->
          MultiplexProgramChannelDestinationSettings'
            <$> (x .:? "multiplexId") <*> (x .:? "programName")
      )

instance Hashable MultiplexProgramChannelDestinationSettings

instance NFData MultiplexProgramChannelDestinationSettings

instance ToJSON MultiplexProgramChannelDestinationSettings where
  toJSON MultiplexProgramChannelDestinationSettings' {..} =
    object
      ( catMaybes
          [ ("multiplexId" .=) <$> _mpcdsMultiplexId,
            ("programName" .=) <$> _mpcdsProgramName
          ]
      )
