{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGateway
  ( TransitGateway (..),

    -- * Smart constructor
    mkTransitGateway,

    -- * Lenses
    tgCreationTime,
    tgState,
    tgOwnerId,
    tgTransitGatewayARN,
    tgTransitGatewayId,
    tgOptions,
    tgDescription,
    tgTags,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayOptions
import Network.AWS.EC2.Types.TransitGatewayState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a transit gateway.
--
-- /See:/ 'mkTransitGateway' smart constructor.
data TransitGateway = TransitGateway'
  { -- | The creation time.
    creationTime :: Lude.Maybe Lude.DateTime,
    -- | The state of the transit gateway.
    state :: Lude.Maybe TransitGatewayState,
    -- | The ID of the AWS account ID that owns the transit gateway.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the transit gateway.
    transitGatewayARN :: Lude.Maybe Lude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Lude.Maybe Lude.Text,
    -- | The transit gateway options.
    options :: Lude.Maybe TransitGatewayOptions,
    -- | The description of the transit gateway.
    description :: Lude.Maybe Lude.Text,
    -- | The tags for the transit gateway.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGateway' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time.
-- * 'state' - The state of the transit gateway.
-- * 'ownerId' - The ID of the AWS account ID that owns the transit gateway.
-- * 'transitGatewayARN' - The Amazon Resource Name (ARN) of the transit gateway.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'options' - The transit gateway options.
-- * 'description' - The description of the transit gateway.
-- * 'tags' - The tags for the transit gateway.
mkTransitGateway ::
  TransitGateway
mkTransitGateway =
  TransitGateway'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      ownerId = Lude.Nothing,
      transitGatewayARN = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      options = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgCreationTime :: Lens.Lens' TransitGateway (Lude.Maybe Lude.DateTime)
tgCreationTime = Lens.lens (creationTime :: TransitGateway -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTime = a} :: TransitGateway)
{-# DEPRECATED tgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the transit gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgState :: Lens.Lens' TransitGateway (Lude.Maybe TransitGatewayState)
tgState = Lens.lens (state :: TransitGateway -> Lude.Maybe TransitGatewayState) (\s a -> s {state = a} :: TransitGateway)
{-# DEPRECATED tgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the AWS account ID that owns the transit gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgOwnerId :: Lens.Lens' TransitGateway (Lude.Maybe Lude.Text)
tgOwnerId = Lens.lens (ownerId :: TransitGateway -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: TransitGateway)
{-# DEPRECATED tgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The Amazon Resource Name (ARN) of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTransitGatewayARN :: Lens.Lens' TransitGateway (Lude.Maybe Lude.Text)
tgTransitGatewayARN = Lens.lens (transitGatewayARN :: TransitGateway -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayARN = a} :: TransitGateway)
{-# DEPRECATED tgTransitGatewayARN "Use generic-lens or generic-optics with 'transitGatewayARN' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTransitGatewayId :: Lens.Lens' TransitGateway (Lude.Maybe Lude.Text)
tgTransitGatewayId = Lens.lens (transitGatewayId :: TransitGateway -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: TransitGateway)
{-# DEPRECATED tgTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The transit gateway options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgOptions :: Lens.Lens' TransitGateway (Lude.Maybe TransitGatewayOptions)
tgOptions = Lens.lens (options :: TransitGateway -> Lude.Maybe TransitGatewayOptions) (\s a -> s {options = a} :: TransitGateway)
{-# DEPRECATED tgOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The description of the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgDescription :: Lens.Lens' TransitGateway (Lude.Maybe Lude.Text)
tgDescription = Lens.lens (description :: TransitGateway -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TransitGateway)
{-# DEPRECATED tgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the transit gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTags :: Lens.Lens' TransitGateway (Lude.Maybe [Tag])
tgTags = Lens.lens (tags :: TransitGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransitGateway)
{-# DEPRECATED tgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TransitGateway where
  parseXML x =
    TransitGateway'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "transitGatewayArn")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> (x Lude..@? "options")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
