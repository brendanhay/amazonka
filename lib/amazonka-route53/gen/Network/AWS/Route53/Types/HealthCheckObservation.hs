{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckObservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckObservation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HealthCheckRegion
import Network.AWS.Route53.Types.StatusReport

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker.
--
--
--
-- /See:/ 'healthCheckObservation' smart constructor.
data HealthCheckObservation = HealthCheckObservation'
  { _hcoIPAddress ::
      !(Maybe Text),
    _hcoStatusReport :: !(Maybe StatusReport),
    _hcoRegion :: !(Maybe HealthCheckRegion)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HealthCheckObservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcoIPAddress' - The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
--
-- * 'hcoStatusReport' - A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
--
-- * 'hcoRegion' - The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
healthCheckObservation ::
  HealthCheckObservation
healthCheckObservation =
  HealthCheckObservation'
    { _hcoIPAddress = Nothing,
      _hcoStatusReport = Nothing,
      _hcoRegion = Nothing
    }

-- | The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
hcoIPAddress :: Lens' HealthCheckObservation (Maybe Text)
hcoIPAddress = lens _hcoIPAddress (\s a -> s {_hcoIPAddress = a})

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
hcoStatusReport :: Lens' HealthCheckObservation (Maybe StatusReport)
hcoStatusReport = lens _hcoStatusReport (\s a -> s {_hcoStatusReport = a})

-- | The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
hcoRegion :: Lens' HealthCheckObservation (Maybe HealthCheckRegion)
hcoRegion = lens _hcoRegion (\s a -> s {_hcoRegion = a})

instance FromXML HealthCheckObservation where
  parseXML x =
    HealthCheckObservation'
      <$> (x .@? "IPAddress") <*> (x .@? "StatusReport") <*> (x .@? "Region")

instance Hashable HealthCheckObservation

instance NFData HealthCheckObservation
