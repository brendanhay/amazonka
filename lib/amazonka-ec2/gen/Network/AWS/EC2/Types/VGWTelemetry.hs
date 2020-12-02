{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VGWTelemetry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VGWTelemetry where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TelemetryStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes telemetry for a VPN tunnel.
--
--
--
-- /See:/ 'vgwTelemetry' smart constructor.
data VGWTelemetry = VGWTelemetry'
  { _vtStatus ::
      !(Maybe TelemetryStatus),
    _vtOutsideIPAddress :: !(Maybe Text),
    _vtCertificateARN :: !(Maybe Text),
    _vtLastStatusChange :: !(Maybe ISO8601),
    _vtAcceptedRouteCount :: !(Maybe Int),
    _vtStatusMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VGWTelemetry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtStatus' - The status of the VPN tunnel.
--
-- * 'vtOutsideIPAddress' - The Internet-routable IP address of the virtual private gateway's outside interface.
--
-- * 'vtCertificateARN' - The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
--
-- * 'vtLastStatusChange' - The date and time of the last change in status.
--
-- * 'vtAcceptedRouteCount' - The number of accepted routes.
--
-- * 'vtStatusMessage' - If an error occurs, a description of the error.
vgwTelemetry ::
  VGWTelemetry
vgwTelemetry =
  VGWTelemetry'
    { _vtStatus = Nothing,
      _vtOutsideIPAddress = Nothing,
      _vtCertificateARN = Nothing,
      _vtLastStatusChange = Nothing,
      _vtAcceptedRouteCount = Nothing,
      _vtStatusMessage = Nothing
    }

-- | The status of the VPN tunnel.
vtStatus :: Lens' VGWTelemetry (Maybe TelemetryStatus)
vtStatus = lens _vtStatus (\s a -> s {_vtStatus = a})

-- | The Internet-routable IP address of the virtual private gateway's outside interface.
vtOutsideIPAddress :: Lens' VGWTelemetry (Maybe Text)
vtOutsideIPAddress = lens _vtOutsideIPAddress (\s a -> s {_vtOutsideIPAddress = a})

-- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
vtCertificateARN :: Lens' VGWTelemetry (Maybe Text)
vtCertificateARN = lens _vtCertificateARN (\s a -> s {_vtCertificateARN = a})

-- | The date and time of the last change in status.
vtLastStatusChange :: Lens' VGWTelemetry (Maybe UTCTime)
vtLastStatusChange = lens _vtLastStatusChange (\s a -> s {_vtLastStatusChange = a}) . mapping _Time

-- | The number of accepted routes.
vtAcceptedRouteCount :: Lens' VGWTelemetry (Maybe Int)
vtAcceptedRouteCount = lens _vtAcceptedRouteCount (\s a -> s {_vtAcceptedRouteCount = a})

-- | If an error occurs, a description of the error.
vtStatusMessage :: Lens' VGWTelemetry (Maybe Text)
vtStatusMessage = lens _vtStatusMessage (\s a -> s {_vtStatusMessage = a})

instance FromXML VGWTelemetry where
  parseXML x =
    VGWTelemetry'
      <$> (x .@? "status")
      <*> (x .@? "outsideIpAddress")
      <*> (x .@? "certificateArn")
      <*> (x .@? "lastStatusChange")
      <*> (x .@? "acceptedRouteCount")
      <*> (x .@? "statusMessage")

instance Hashable VGWTelemetry

instance NFData VGWTelemetry
