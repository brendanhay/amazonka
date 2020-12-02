{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewaySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewaySummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary of a gateway.
--
--
--
-- /See:/ 'gatewaySummary' smart constructor.
data GatewaySummary = GatewaySummary'
  { _gsARN :: !(Maybe Text),
    _gsName :: !(Maybe Text),
    _gsGatewayGroupARN :: !(Maybe Text),
    _gsSoftwareVersion :: !(Maybe Text),
    _gsDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GatewaySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsARN' - The ARN of the gateway.
--
-- * 'gsName' - The name of the gateway.
--
-- * 'gsGatewayGroupARN' - The ARN of the gateway group that the gateway is associated to.
--
-- * 'gsSoftwareVersion' - The software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- * 'gsDescription' - The description of the gateway.
gatewaySummary ::
  GatewaySummary
gatewaySummary =
  GatewaySummary'
    { _gsARN = Nothing,
      _gsName = Nothing,
      _gsGatewayGroupARN = Nothing,
      _gsSoftwareVersion = Nothing,
      _gsDescription = Nothing
    }

-- | The ARN of the gateway.
gsARN :: Lens' GatewaySummary (Maybe Text)
gsARN = lens _gsARN (\s a -> s {_gsARN = a})

-- | The name of the gateway.
gsName :: Lens' GatewaySummary (Maybe Text)
gsName = lens _gsName (\s a -> s {_gsName = a})

-- | The ARN of the gateway group that the gateway is associated to.
gsGatewayGroupARN :: Lens' GatewaySummary (Maybe Text)
gsGatewayGroupARN = lens _gsGatewayGroupARN (\s a -> s {_gsGatewayGroupARN = a})

-- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
gsSoftwareVersion :: Lens' GatewaySummary (Maybe Text)
gsSoftwareVersion = lens _gsSoftwareVersion (\s a -> s {_gsSoftwareVersion = a})

-- | The description of the gateway.
gsDescription :: Lens' GatewaySummary (Maybe Text)
gsDescription = lens _gsDescription (\s a -> s {_gsDescription = a})

instance FromJSON GatewaySummary where
  parseJSON =
    withObject
      "GatewaySummary"
      ( \x ->
          GatewaySummary'
            <$> (x .:? "Arn")
            <*> (x .:? "Name")
            <*> (x .:? "GatewayGroupArn")
            <*> (x .:? "SoftwareVersion")
            <*> (x .:? "Description")
      )

instance Hashable GatewaySummary

instance NFData GatewaySummary
