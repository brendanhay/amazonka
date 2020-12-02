{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.AccessEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AccessEndpoint where

import Network.AWS.AppStream.Types.AccessEndpointType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an interface VPC endpoint (interface endpoint) that lets you create a private connection between the virtual private cloud (VPC) that you specify and AppStream 2.0. When you specify an interface endpoint for a stack, users of the stack can connect to AppStream 2.0 only through that endpoint. When you specify an interface endpoint for an image builder, administrators can connect to the image builder only through that endpoint.
--
--
--
-- /See:/ 'accessEndpoint' smart constructor.
data AccessEndpoint = AccessEndpoint'
  { _aeVPCeId :: !(Maybe Text),
    _aeEndpointType :: !AccessEndpointType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeVPCeId' - The identifier (ID) of the VPC in which the interface endpoint is used.
--
-- * 'aeEndpointType' - The type of interface endpoint.
accessEndpoint ::
  -- | 'aeEndpointType'
  AccessEndpointType ->
  AccessEndpoint
accessEndpoint pEndpointType_ =
  AccessEndpoint'
    { _aeVPCeId = Nothing,
      _aeEndpointType = pEndpointType_
    }

-- | The identifier (ID) of the VPC in which the interface endpoint is used.
aeVPCeId :: Lens' AccessEndpoint (Maybe Text)
aeVPCeId = lens _aeVPCeId (\s a -> s {_aeVPCeId = a})

-- | The type of interface endpoint.
aeEndpointType :: Lens' AccessEndpoint AccessEndpointType
aeEndpointType = lens _aeEndpointType (\s a -> s {_aeEndpointType = a})

instance FromJSON AccessEndpoint where
  parseJSON =
    withObject
      "AccessEndpoint"
      ( \x ->
          AccessEndpoint' <$> (x .:? "VpceId") <*> (x .: "EndpointType")
      )

instance Hashable AccessEndpoint

instance NFData AccessEndpoint

instance ToJSON AccessEndpoint where
  toJSON AccessEndpoint' {..} =
    object
      ( catMaybes
          [ ("VpceId" .=) <$> _aeVPCeId,
            Just ("EndpointType" .= _aeEndpointType)
          ]
      )
