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
-- Module      : Network.AWS.DirectConnect.Types.AssociatedGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.AssociatedGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.GatewayType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the associated gateway.
--
-- /See:/ 'newAssociatedGateway' smart constructor.
data AssociatedGateway = AssociatedGateway'
  { -- | The ID of the associated gateway.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account that owns the associated virtual private gateway
    -- or transit gateway.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Region where the associated gateway is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The type of associated gateway.
    type' :: Prelude.Maybe GatewayType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associatedGateway_id' - The ID of the associated gateway.
--
-- 'ownerAccount', 'associatedGateway_ownerAccount' - The ID of the account that owns the associated virtual private gateway
-- or transit gateway.
--
-- 'region', 'associatedGateway_region' - The Region where the associated gateway is located.
--
-- 'type'', 'associatedGateway_type' - The type of associated gateway.
newAssociatedGateway ::
  AssociatedGateway
newAssociatedGateway =
  AssociatedGateway'
    { id = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      region = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ID of the associated gateway.
associatedGateway_id :: Lens.Lens' AssociatedGateway (Prelude.Maybe Prelude.Text)
associatedGateway_id = Lens.lens (\AssociatedGateway' {id} -> id) (\s@AssociatedGateway' {} a -> s {id = a} :: AssociatedGateway)

-- | The ID of the account that owns the associated virtual private gateway
-- or transit gateway.
associatedGateway_ownerAccount :: Lens.Lens' AssociatedGateway (Prelude.Maybe Prelude.Text)
associatedGateway_ownerAccount = Lens.lens (\AssociatedGateway' {ownerAccount} -> ownerAccount) (\s@AssociatedGateway' {} a -> s {ownerAccount = a} :: AssociatedGateway)

-- | The Region where the associated gateway is located.
associatedGateway_region :: Lens.Lens' AssociatedGateway (Prelude.Maybe Prelude.Text)
associatedGateway_region = Lens.lens (\AssociatedGateway' {region} -> region) (\s@AssociatedGateway' {} a -> s {region = a} :: AssociatedGateway)

-- | The type of associated gateway.
associatedGateway_type :: Lens.Lens' AssociatedGateway (Prelude.Maybe GatewayType)
associatedGateway_type = Lens.lens (\AssociatedGateway' {type'} -> type') (\s@AssociatedGateway' {} a -> s {type' = a} :: AssociatedGateway)

instance Core.FromJSON AssociatedGateway where
  parseJSON =
    Core.withObject
      "AssociatedGateway"
      ( \x ->
          AssociatedGateway'
            Prelude.<$> (x Core..:? "id")
            Prelude.<*> (x Core..:? "ownerAccount")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable AssociatedGateway

instance Prelude.NFData AssociatedGateway
