{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.CustomerConnectorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.CustomerConnectorInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Inventory data for installed discovery connectors.
--
--
--
-- /See:/ 'customerConnectorInfo' smart constructor.
data CustomerConnectorInfo = CustomerConnectorInfo'
  { _cciActiveConnectors ::
      !Int,
    _cciHealthyConnectors :: !Int,
    _cciBlackListedConnectors :: !Int,
    _cciShutdownConnectors :: !Int,
    _cciUnhealthyConnectors :: !Int,
    _cciTotalConnectors :: !Int,
    _cciUnknownConnectors :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerConnectorInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cciActiveConnectors' - Number of active discovery connectors.
--
-- * 'cciHealthyConnectors' - Number of healthy discovery connectors.
--
-- * 'cciBlackListedConnectors' - Number of blacklisted discovery connectors.
--
-- * 'cciShutdownConnectors' - Number of discovery connectors with status SHUTDOWN,
--
-- * 'cciUnhealthyConnectors' - Number of unhealthy discovery connectors.
--
-- * 'cciTotalConnectors' - Total number of discovery connectors.
--
-- * 'cciUnknownConnectors' - Number of unknown discovery connectors.
customerConnectorInfo ::
  -- | 'cciActiveConnectors'
  Int ->
  -- | 'cciHealthyConnectors'
  Int ->
  -- | 'cciBlackListedConnectors'
  Int ->
  -- | 'cciShutdownConnectors'
  Int ->
  -- | 'cciUnhealthyConnectors'
  Int ->
  -- | 'cciTotalConnectors'
  Int ->
  -- | 'cciUnknownConnectors'
  Int ->
  CustomerConnectorInfo
customerConnectorInfo
  pActiveConnectors_
  pHealthyConnectors_
  pBlackListedConnectors_
  pShutdownConnectors_
  pUnhealthyConnectors_
  pTotalConnectors_
  pUnknownConnectors_ =
    CustomerConnectorInfo'
      { _cciActiveConnectors = pActiveConnectors_,
        _cciHealthyConnectors = pHealthyConnectors_,
        _cciBlackListedConnectors = pBlackListedConnectors_,
        _cciShutdownConnectors = pShutdownConnectors_,
        _cciUnhealthyConnectors = pUnhealthyConnectors_,
        _cciTotalConnectors = pTotalConnectors_,
        _cciUnknownConnectors = pUnknownConnectors_
      }

-- | Number of active discovery connectors.
cciActiveConnectors :: Lens' CustomerConnectorInfo Int
cciActiveConnectors = lens _cciActiveConnectors (\s a -> s {_cciActiveConnectors = a})

-- | Number of healthy discovery connectors.
cciHealthyConnectors :: Lens' CustomerConnectorInfo Int
cciHealthyConnectors = lens _cciHealthyConnectors (\s a -> s {_cciHealthyConnectors = a})

-- | Number of blacklisted discovery connectors.
cciBlackListedConnectors :: Lens' CustomerConnectorInfo Int
cciBlackListedConnectors = lens _cciBlackListedConnectors (\s a -> s {_cciBlackListedConnectors = a})

-- | Number of discovery connectors with status SHUTDOWN,
cciShutdownConnectors :: Lens' CustomerConnectorInfo Int
cciShutdownConnectors = lens _cciShutdownConnectors (\s a -> s {_cciShutdownConnectors = a})

-- | Number of unhealthy discovery connectors.
cciUnhealthyConnectors :: Lens' CustomerConnectorInfo Int
cciUnhealthyConnectors = lens _cciUnhealthyConnectors (\s a -> s {_cciUnhealthyConnectors = a})

-- | Total number of discovery connectors.
cciTotalConnectors :: Lens' CustomerConnectorInfo Int
cciTotalConnectors = lens _cciTotalConnectors (\s a -> s {_cciTotalConnectors = a})

-- | Number of unknown discovery connectors.
cciUnknownConnectors :: Lens' CustomerConnectorInfo Int
cciUnknownConnectors = lens _cciUnknownConnectors (\s a -> s {_cciUnknownConnectors = a})

instance FromJSON CustomerConnectorInfo where
  parseJSON =
    withObject
      "CustomerConnectorInfo"
      ( \x ->
          CustomerConnectorInfo'
            <$> (x .: "activeConnectors")
            <*> (x .: "healthyConnectors")
            <*> (x .: "blackListedConnectors")
            <*> (x .: "shutdownConnectors")
            <*> (x .: "unhealthyConnectors")
            <*> (x .: "totalConnectors")
            <*> (x .: "unknownConnectors")
      )

instance Hashable CustomerConnectorInfo

instance NFData CustomerConnectorInfo
