{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetAssociation
  ( SubnetAssociation (..),

    -- * Smart constructor
    mkSubnetAssociation,

    -- * Lenses
    saState,
    saSubnetId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the subnet association with the transit gateway multicast domain.
--
-- /See:/ 'mkSubnetAssociation' smart constructor.
data SubnetAssociation = SubnetAssociation'
  { -- | The state of the subnet association.
    state :: Lude.Maybe TransitGatewayMulitcastDomainAssociationState,
    -- | The ID of the subnet.
    subnetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubnetAssociation' with the minimum fields required to make a request.
--
-- * 'state' - The state of the subnet association.
-- * 'subnetId' - The ID of the subnet.
mkSubnetAssociation ::
  SubnetAssociation
mkSubnetAssociation =
  SubnetAssociation' {state = Lude.Nothing, subnetId = Lude.Nothing}

-- | The state of the subnet association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saState :: Lens.Lens' SubnetAssociation (Lude.Maybe TransitGatewayMulitcastDomainAssociationState)
saState = Lens.lens (state :: SubnetAssociation -> Lude.Maybe TransitGatewayMulitcastDomainAssociationState) (\s a -> s {state = a} :: SubnetAssociation)
{-# DEPRECATED saState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saSubnetId :: Lens.Lens' SubnetAssociation (Lude.Maybe Lude.Text)
saSubnetId = Lens.lens (subnetId :: SubnetAssociation -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: SubnetAssociation)
{-# DEPRECATED saSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Lude.FromXML SubnetAssociation where
  parseXML x =
    SubnetAssociation'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "subnetId")
