{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringTgwInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringTgwInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the transit gateway in the peering attachment.
--
--
--
-- /See:/ 'peeringTgwInfo' smart constructor.
data PeeringTgwInfo = PeeringTgwInfo'
  { _ptiOwnerId :: !(Maybe Text),
    _ptiTransitGatewayId :: !(Maybe Text),
    _ptiRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PeeringTgwInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptiOwnerId' - The AWS account ID of the owner of the transit gateway.
--
-- * 'ptiTransitGatewayId' - The ID of the transit gateway.
--
-- * 'ptiRegion' - The Region of the transit gateway.
peeringTgwInfo ::
  PeeringTgwInfo
peeringTgwInfo =
  PeeringTgwInfo'
    { _ptiOwnerId = Nothing,
      _ptiTransitGatewayId = Nothing,
      _ptiRegion = Nothing
    }

-- | The AWS account ID of the owner of the transit gateway.
ptiOwnerId :: Lens' PeeringTgwInfo (Maybe Text)
ptiOwnerId = lens _ptiOwnerId (\s a -> s {_ptiOwnerId = a})

-- | The ID of the transit gateway.
ptiTransitGatewayId :: Lens' PeeringTgwInfo (Maybe Text)
ptiTransitGatewayId = lens _ptiTransitGatewayId (\s a -> s {_ptiTransitGatewayId = a})

-- | The Region of the transit gateway.
ptiRegion :: Lens' PeeringTgwInfo (Maybe Text)
ptiRegion = lens _ptiRegion (\s a -> s {_ptiRegion = a})

instance FromXML PeeringTgwInfo where
  parseXML x =
    PeeringTgwInfo'
      <$> (x .@? "ownerId")
      <*> (x .@? "transitGatewayId")
      <*> (x .@? "region")

instance Hashable PeeringTgwInfo

instance NFData PeeringTgwInfo
