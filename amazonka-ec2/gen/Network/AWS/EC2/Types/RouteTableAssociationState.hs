{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociationState where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RouteTableAssociationStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the state of an association between a route table and a subnet
-- or gateway.
--
-- /See:/ 'newRouteTableAssociationState' smart constructor.
data RouteTableAssociationState = RouteTableAssociationState'
  { -- | The status message, if applicable.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    state :: Prelude.Maybe RouteTableAssociationStateCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RouteTableAssociationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'routeTableAssociationState_statusMessage' - The status message, if applicable.
--
-- 'state', 'routeTableAssociationState_state' - The state of the association.
newRouteTableAssociationState ::
  RouteTableAssociationState
newRouteTableAssociationState =
  RouteTableAssociationState'
    { statusMessage =
        Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The status message, if applicable.
routeTableAssociationState_statusMessage :: Lens.Lens' RouteTableAssociationState (Prelude.Maybe Prelude.Text)
routeTableAssociationState_statusMessage = Lens.lens (\RouteTableAssociationState' {statusMessage} -> statusMessage) (\s@RouteTableAssociationState' {} a -> s {statusMessage = a} :: RouteTableAssociationState)

-- | The state of the association.
routeTableAssociationState_state :: Lens.Lens' RouteTableAssociationState (Prelude.Maybe RouteTableAssociationStateCode)
routeTableAssociationState_state = Lens.lens (\RouteTableAssociationState' {state} -> state) (\s@RouteTableAssociationState' {} a -> s {state = a} :: RouteTableAssociationState)

instance Prelude.FromXML RouteTableAssociationState where
  parseXML x =
    RouteTableAssociationState'
      Prelude.<$> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "state")

instance Prelude.Hashable RouteTableAssociationState

instance Prelude.NFData RouteTableAssociationState
