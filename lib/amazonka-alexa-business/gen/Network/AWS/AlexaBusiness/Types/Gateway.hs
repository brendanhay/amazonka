{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Gateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Gateway where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the gateway.
--
--
--
-- /See:/ 'gateway' smart constructor.
data Gateway = Gateway'
  { _gARN :: !(Maybe Text),
    _gName :: !(Maybe Text),
    _gGatewayGroupARN :: !(Maybe Text),
    _gSoftwareVersion :: !(Maybe Text),
    _gDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Gateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gARN' - The ARN of the gateway.
--
-- * 'gName' - The name of the gateway.
--
-- * 'gGatewayGroupARN' - The ARN of the gateway group that the gateway is associated to.
--
-- * 'gSoftwareVersion' - The software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- * 'gDescription' - The description of the gateway.
gateway ::
  Gateway
gateway =
  Gateway'
    { _gARN = Nothing,
      _gName = Nothing,
      _gGatewayGroupARN = Nothing,
      _gSoftwareVersion = Nothing,
      _gDescription = Nothing
    }

-- | The ARN of the gateway.
gARN :: Lens' Gateway (Maybe Text)
gARN = lens _gARN (\s a -> s {_gARN = a})

-- | The name of the gateway.
gName :: Lens' Gateway (Maybe Text)
gName = lens _gName (\s a -> s {_gName = a})

-- | The ARN of the gateway group that the gateway is associated to.
gGatewayGroupARN :: Lens' Gateway (Maybe Text)
gGatewayGroupARN = lens _gGatewayGroupARN (\s a -> s {_gGatewayGroupARN = a})

-- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
gSoftwareVersion :: Lens' Gateway (Maybe Text)
gSoftwareVersion = lens _gSoftwareVersion (\s a -> s {_gSoftwareVersion = a})

-- | The description of the gateway.
gDescription :: Lens' Gateway (Maybe Text)
gDescription = lens _gDescription (\s a -> s {_gDescription = a})

instance FromJSON Gateway where
  parseJSON =
    withObject
      "Gateway"
      ( \x ->
          Gateway'
            <$> (x .:? "Arn")
            <*> (x .:? "Name")
            <*> (x .:? "GatewayGroupArn")
            <*> (x .:? "SoftwareVersion")
            <*> (x .:? "Description")
      )

instance Hashable Gateway

instance NFData Gateway
