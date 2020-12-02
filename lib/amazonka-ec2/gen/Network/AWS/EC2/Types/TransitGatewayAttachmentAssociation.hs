{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAssociationState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association.
--
--
--
-- /See:/ 'transitGatewayAttachmentAssociation' smart constructor.
data TransitGatewayAttachmentAssociation = TransitGatewayAttachmentAssociation'
  { _tgaaState ::
      !( Maybe
           TransitGatewayAssociationState
       ),
    _tgaaTransitGatewayRouteTableId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayAttachmentAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgaaState' - The state of the association.
--
-- * 'tgaaTransitGatewayRouteTableId' - The ID of the route table for the transit gateway.
transitGatewayAttachmentAssociation ::
  TransitGatewayAttachmentAssociation
transitGatewayAttachmentAssociation =
  TransitGatewayAttachmentAssociation'
    { _tgaaState = Nothing,
      _tgaaTransitGatewayRouteTableId = Nothing
    }

-- | The state of the association.
tgaaState :: Lens' TransitGatewayAttachmentAssociation (Maybe TransitGatewayAssociationState)
tgaaState = lens _tgaaState (\s a -> s {_tgaaState = a})

-- | The ID of the route table for the transit gateway.
tgaaTransitGatewayRouteTableId :: Lens' TransitGatewayAttachmentAssociation (Maybe Text)
tgaaTransitGatewayRouteTableId = lens _tgaaTransitGatewayRouteTableId (\s a -> s {_tgaaTransitGatewayRouteTableId = a})

instance FromXML TransitGatewayAttachmentAssociation where
  parseXML x =
    TransitGatewayAttachmentAssociation'
      <$> (x .@? "state") <*> (x .@? "transitGatewayRouteTableId")

instance Hashable TransitGatewayAttachmentAssociation

instance NFData TransitGatewayAttachmentAssociation
