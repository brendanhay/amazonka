{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Transport stream service descriptor configuration for the Multiplex program.
--
-- /See:/ 'multiplexProgramServiceDescriptor' smart constructor.
data MultiplexProgramServiceDescriptor = MultiplexProgramServiceDescriptor'
  { _mpsdProviderName ::
      !Text,
    _mpsdServiceName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexProgramServiceDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsdProviderName' - Name of the provider.
--
-- * 'mpsdServiceName' - Name of the service.
multiplexProgramServiceDescriptor ::
  -- | 'mpsdProviderName'
  Text ->
  -- | 'mpsdServiceName'
  Text ->
  MultiplexProgramServiceDescriptor
multiplexProgramServiceDescriptor pProviderName_ pServiceName_ =
  MultiplexProgramServiceDescriptor'
    { _mpsdProviderName =
        pProviderName_,
      _mpsdServiceName = pServiceName_
    }

-- | Name of the provider.
mpsdProviderName :: Lens' MultiplexProgramServiceDescriptor Text
mpsdProviderName = lens _mpsdProviderName (\s a -> s {_mpsdProviderName = a})

-- | Name of the service.
mpsdServiceName :: Lens' MultiplexProgramServiceDescriptor Text
mpsdServiceName = lens _mpsdServiceName (\s a -> s {_mpsdServiceName = a})

instance FromJSON MultiplexProgramServiceDescriptor where
  parseJSON =
    withObject
      "MultiplexProgramServiceDescriptor"
      ( \x ->
          MultiplexProgramServiceDescriptor'
            <$> (x .: "providerName") <*> (x .: "serviceName")
      )

instance Hashable MultiplexProgramServiceDescriptor

instance NFData MultiplexProgramServiceDescriptor

instance ToJSON MultiplexProgramServiceDescriptor where
  toJSON MultiplexProgramServiceDescriptor' {..} =
    object
      ( catMaybes
          [ Just ("providerName" .= _mpsdProviderName),
            Just ("serviceName" .= _mpsdServiceName)
          ]
      )
