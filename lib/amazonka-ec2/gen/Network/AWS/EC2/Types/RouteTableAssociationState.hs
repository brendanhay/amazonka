{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociationState where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RouteTableAssociationStateCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the state of an association between a route table and a subnet or gateway.
--
--
--
-- /See:/ 'routeTableAssociationState' smart constructor.
data RouteTableAssociationState = RouteTableAssociationState'
  { _rtasState ::
      !( Maybe
           RouteTableAssociationStateCode
       ),
    _rtasStatusMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RouteTableAssociationState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtasState' - The state of the association.
--
-- * 'rtasStatusMessage' - The status message, if applicable.
routeTableAssociationState ::
  RouteTableAssociationState
routeTableAssociationState =
  RouteTableAssociationState'
    { _rtasState = Nothing,
      _rtasStatusMessage = Nothing
    }

-- | The state of the association.
rtasState :: Lens' RouteTableAssociationState (Maybe RouteTableAssociationStateCode)
rtasState = lens _rtasState (\s a -> s {_rtasState = a})

-- | The status message, if applicable.
rtasStatusMessage :: Lens' RouteTableAssociationState (Maybe Text)
rtasStatusMessage = lens _rtasStatusMessage (\s a -> s {_rtasStatusMessage = a})

instance FromXML RouteTableAssociationState where
  parseXML x =
    RouteTableAssociationState'
      <$> (x .@? "state") <*> (x .@? "statusMessage")

instance Hashable RouteTableAssociationState

instance NFData RouteTableAssociationState
