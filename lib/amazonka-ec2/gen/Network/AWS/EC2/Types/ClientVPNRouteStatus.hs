{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNRouteStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNRouteStatus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVPNRouteStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the state of a Client VPN endpoint route.
--
--
--
-- /See:/ 'clientVPNRouteStatus' smart constructor.
data ClientVPNRouteStatus = ClientVPNRouteStatus'
  { _cvrsCode ::
      !(Maybe ClientVPNRouteStatusCode),
    _cvrsMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNRouteStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvrsCode' - The state of the Client VPN endpoint route.
--
-- * 'cvrsMessage' - A message about the status of the Client VPN endpoint route, if applicable.
clientVPNRouteStatus ::
  ClientVPNRouteStatus
clientVPNRouteStatus =
  ClientVPNRouteStatus'
    { _cvrsCode = Nothing,
      _cvrsMessage = Nothing
    }

-- | The state of the Client VPN endpoint route.
cvrsCode :: Lens' ClientVPNRouteStatus (Maybe ClientVPNRouteStatusCode)
cvrsCode = lens _cvrsCode (\s a -> s {_cvrsCode = a})

-- | A message about the status of the Client VPN endpoint route, if applicable.
cvrsMessage :: Lens' ClientVPNRouteStatus (Maybe Text)
cvrsMessage = lens _cvrsMessage (\s a -> s {_cvrsMessage = a})

instance FromXML ClientVPNRouteStatus where
  parseXML x =
    ClientVPNRouteStatus' <$> (x .@? "code") <*> (x .@? "message")

instance Hashable ClientVPNRouteStatus

instance NFData ClientVPNRouteStatus
