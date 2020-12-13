{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.AssociatedGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.AssociatedGateway
  ( AssociatedGateway (..),

    -- * Smart constructor
    mkAssociatedGateway,

    -- * Lenses
    agId,
    agOwnerAccount,
    agRegion,
    agType,
  )
where

import Network.AWS.DirectConnect.Types.GatewayType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the associated gateway.
--
-- /See:/ 'mkAssociatedGateway' smart constructor.
data AssociatedGateway = AssociatedGateway'
  { -- | The ID of the associated gateway.
    id :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS account that owns the associated virtual private gateway or transit gateway.
    ownerAccount :: Lude.Maybe Lude.Text,
    -- | The Region where the associated gateway is located.
    region :: Lude.Maybe Lude.Text,
    -- | The type of associated gateway.
    type' :: Lude.Maybe GatewayType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociatedGateway' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the associated gateway.
-- * 'ownerAccount' - The ID of the AWS account that owns the associated virtual private gateway or transit gateway.
-- * 'region' - The Region where the associated gateway is located.
-- * 'type'' - The type of associated gateway.
mkAssociatedGateway ::
  AssociatedGateway
mkAssociatedGateway =
  AssociatedGateway'
    { id = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      region = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agId :: Lens.Lens' AssociatedGateway (Lude.Maybe Lude.Text)
agId = Lens.lens (id :: AssociatedGateway -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: AssociatedGateway)
{-# DEPRECATED agId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the AWS account that owns the associated virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agOwnerAccount :: Lens.Lens' AssociatedGateway (Lude.Maybe Lude.Text)
agOwnerAccount = Lens.lens (ownerAccount :: AssociatedGateway -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: AssociatedGateway)
{-# DEPRECATED agOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The Region where the associated gateway is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agRegion :: Lens.Lens' AssociatedGateway (Lude.Maybe Lude.Text)
agRegion = Lens.lens (region :: AssociatedGateway -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: AssociatedGateway)
{-# DEPRECATED agRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The type of associated gateway.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agType :: Lens.Lens' AssociatedGateway (Lude.Maybe GatewayType)
agType = Lens.lens (type' :: AssociatedGateway -> Lude.Maybe GatewayType) (\s a -> s {type' = a} :: AssociatedGateway)
{-# DEPRECATED agType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON AssociatedGateway where
  parseJSON =
    Lude.withObject
      "AssociatedGateway"
      ( \x ->
          AssociatedGateway'
            Lude.<$> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "ownerAccount")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "type")
      )
