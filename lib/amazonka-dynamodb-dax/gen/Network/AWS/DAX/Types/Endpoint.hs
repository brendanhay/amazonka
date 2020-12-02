{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Endpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the information required for client programs to connect to the configuration endpoint for a DAX cluster, or to an individual node within the cluster.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eAddress :: !(Maybe Text),
    _ePort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAddress' - The DNS hostname of the endpoint.
--
-- * 'ePort' - The port number that applications should use to connect to the endpoint.
endpoint ::
  Endpoint
endpoint = Endpoint' {_eAddress = Nothing, _ePort = Nothing}

-- | The DNS hostname of the endpoint.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\s a -> s {_eAddress = a})

-- | The port number that applications should use to connect to the endpoint.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\s a -> s {_ePort = a})

instance FromJSON Endpoint where
  parseJSON =
    withObject
      "Endpoint"
      (\x -> Endpoint' <$> (x .:? "Address") <*> (x .:? "Port"))

instance Hashable Endpoint

instance NFData Endpoint
