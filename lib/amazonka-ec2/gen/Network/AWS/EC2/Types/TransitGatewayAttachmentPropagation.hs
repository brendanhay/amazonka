{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a propagation route table.
--
--
--
-- /See:/ 'transitGatewayAttachmentPropagation' smart constructor.
data TransitGatewayAttachmentPropagation = TransitGatewayAttachmentPropagation'
  { _tgapState ::
      !( Maybe
           TransitGatewayPropagationState
       ),
    _tgapTransitGatewayRouteTableId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayAttachmentPropagation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgapState' - The state of the propagation route table.
--
-- * 'tgapTransitGatewayRouteTableId' - The ID of the propagation route table.
transitGatewayAttachmentPropagation ::
  TransitGatewayAttachmentPropagation
transitGatewayAttachmentPropagation =
  TransitGatewayAttachmentPropagation'
    { _tgapState = Nothing,
      _tgapTransitGatewayRouteTableId = Nothing
    }

-- | The state of the propagation route table.
tgapState :: Lens' TransitGatewayAttachmentPropagation (Maybe TransitGatewayPropagationState)
tgapState = lens _tgapState (\s a -> s {_tgapState = a})

-- | The ID of the propagation route table.
tgapTransitGatewayRouteTableId :: Lens' TransitGatewayAttachmentPropagation (Maybe Text)
tgapTransitGatewayRouteTableId = lens _tgapTransitGatewayRouteTableId (\s a -> s {_tgapTransitGatewayRouteTableId = a})

instance FromXML TransitGatewayAttachmentPropagation where
  parseXML x =
    TransitGatewayAttachmentPropagation'
      <$> (x .@? "state") <*> (x .@? "transitGatewayRouteTableId")

instance Hashable TransitGatewayAttachmentPropagation

instance NFData TransitGatewayAttachmentPropagation
