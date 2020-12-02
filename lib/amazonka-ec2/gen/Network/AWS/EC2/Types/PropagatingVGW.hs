{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PropagatingVGW
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PropagatingVGW where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a virtual private gateway propagating route.
--
--
--
-- /See:/ 'propagatingVGW' smart constructor.
newtype PropagatingVGW = PropagatingVGW'
  { _pvGatewayId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PropagatingVGW' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvGatewayId' - The ID of the virtual private gateway.
propagatingVGW ::
  PropagatingVGW
propagatingVGW = PropagatingVGW' {_pvGatewayId = Nothing}

-- | The ID of the virtual private gateway.
pvGatewayId :: Lens' PropagatingVGW (Maybe Text)
pvGatewayId = lens _pvGatewayId (\s a -> s {_pvGatewayId = a})

instance FromXML PropagatingVGW where
  parseXML x = PropagatingVGW' <$> (x .@? "gatewayId")

instance Hashable PropagatingVGW

instance NFData PropagatingVGW
