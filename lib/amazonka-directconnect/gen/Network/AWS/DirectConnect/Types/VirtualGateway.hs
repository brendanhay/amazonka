{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualGateway where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a virtual private gateway for a private virtual interface.
--
--
--
-- /See:/ 'virtualGateway' smart constructor.
data VirtualGateway = VirtualGateway'
  { _vgVirtualGatewayId ::
      !(Maybe Text),
    _vgVirtualGatewayState :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VirtualGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vgVirtualGatewayId' - The ID of the virtual private gateway.
--
-- * 'vgVirtualGatewayState' - The state of the virtual private gateway. The following are the possible values:     * @pending@ : Initial state after creating the virtual private gateway.     * @available@ : Ready for use by a private virtual interface.     * @deleting@ : Initial state after deleting the virtual private gateway.     * @deleted@ : The virtual private gateway is deleted. The private virtual interface is unable to send traffic over this gateway.
virtualGateway ::
  VirtualGateway
virtualGateway =
  VirtualGateway'
    { _vgVirtualGatewayId = Nothing,
      _vgVirtualGatewayState = Nothing
    }

-- | The ID of the virtual private gateway.
vgVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayId = lens _vgVirtualGatewayId (\s a -> s {_vgVirtualGatewayId = a})

-- | The state of the virtual private gateway. The following are the possible values:     * @pending@ : Initial state after creating the virtual private gateway.     * @available@ : Ready for use by a private virtual interface.     * @deleting@ : Initial state after deleting the virtual private gateway.     * @deleted@ : The virtual private gateway is deleted. The private virtual interface is unable to send traffic over this gateway.
vgVirtualGatewayState :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayState = lens _vgVirtualGatewayState (\s a -> s {_vgVirtualGatewayState = a})

instance FromJSON VirtualGateway where
  parseJSON =
    withObject
      "VirtualGateway"
      ( \x ->
          VirtualGateway'
            <$> (x .:? "virtualGatewayId") <*> (x .:? "virtualGatewayState")
      )

instance Hashable VirtualGateway

instance NFData VirtualGateway
