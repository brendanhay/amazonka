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
-- Module      : Amazonka.EC2.Types.RouteTableAssociationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RouteTableAssociationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.RouteTableAssociationStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of an association between a route table and a subnet
-- or gateway.
--
-- /See:/ 'newRouteTableAssociationState' smart constructor.
data RouteTableAssociationState = RouteTableAssociationState'
  { -- | The state of the association.
    state :: Prelude.Maybe RouteTableAssociationStateCode,
    -- | The status message, if applicable.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteTableAssociationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'routeTableAssociationState_state' - The state of the association.
--
-- 'statusMessage', 'routeTableAssociationState_statusMessage' - The status message, if applicable.
newRouteTableAssociationState ::
  RouteTableAssociationState
newRouteTableAssociationState =
  RouteTableAssociationState'
    { state =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The state of the association.
routeTableAssociationState_state :: Lens.Lens' RouteTableAssociationState (Prelude.Maybe RouteTableAssociationStateCode)
routeTableAssociationState_state = Lens.lens (\RouteTableAssociationState' {state} -> state) (\s@RouteTableAssociationState' {} a -> s {state = a} :: RouteTableAssociationState)

-- | The status message, if applicable.
routeTableAssociationState_statusMessage :: Lens.Lens' RouteTableAssociationState (Prelude.Maybe Prelude.Text)
routeTableAssociationState_statusMessage = Lens.lens (\RouteTableAssociationState' {statusMessage} -> statusMessage) (\s@RouteTableAssociationState' {} a -> s {statusMessage = a} :: RouteTableAssociationState)

instance Data.FromXML RouteTableAssociationState where
  parseXML x =
    RouteTableAssociationState'
      Prelude.<$> (x Data..@? "state")
      Prelude.<*> (x Data..@? "statusMessage")

instance Prelude.Hashable RouteTableAssociationState where
  hashWithSalt _salt RouteTableAssociationState' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData RouteTableAssociationState where
  rnf RouteTableAssociationState' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
