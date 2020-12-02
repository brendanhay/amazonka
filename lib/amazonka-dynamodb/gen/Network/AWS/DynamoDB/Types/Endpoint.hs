{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Endpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An endpoint information details.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eAddress :: !Text,
    _eCachePeriodInMinutes :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAddress' - IP address of the endpoint.
--
-- * 'eCachePeriodInMinutes' - Endpoint cache time to live (TTL) value.
endpoint ::
  -- | 'eAddress'
  Text ->
  -- | 'eCachePeriodInMinutes'
  Integer ->
  Endpoint
endpoint pAddress_ pCachePeriodInMinutes_ =
  Endpoint'
    { _eAddress = pAddress_,
      _eCachePeriodInMinutes = pCachePeriodInMinutes_
    }

-- | IP address of the endpoint.
eAddress :: Lens' Endpoint Text
eAddress = lens _eAddress (\s a -> s {_eAddress = a})

-- | Endpoint cache time to live (TTL) value.
eCachePeriodInMinutes :: Lens' Endpoint Integer
eCachePeriodInMinutes = lens _eCachePeriodInMinutes (\s a -> s {_eCachePeriodInMinutes = a})

instance FromJSON Endpoint where
  parseJSON =
    withObject
      "Endpoint"
      ( \x ->
          Endpoint' <$> (x .: "Address") <*> (x .: "CachePeriodInMinutes")
      )

instance Hashable Endpoint

instance NFData Endpoint
