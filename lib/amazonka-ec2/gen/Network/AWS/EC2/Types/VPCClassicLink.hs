{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCClassicLink
  ( VPCClassicLink (..),

    -- * Smart constructor
    mkVPCClassicLink,

    -- * Lenses
    vclVPCId,
    vclTags,
    vclClassicLinkEnabled,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes whether a VPC is enabled for ClassicLink.
--
-- /See:/ 'mkVPCClassicLink' smart constructor.
data VPCClassicLink = VPCClassicLink'
  { -- | The ID of the VPC.
    vpcId :: Lude.Maybe Lude.Text,
    -- | Any tags assigned to the VPC.
    tags :: Lude.Maybe [Tag],
    -- | Indicates whether the VPC is enabled for ClassicLink.
    classicLinkEnabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCClassicLink' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
-- * 'tags' - Any tags assigned to the VPC.
-- * 'classicLinkEnabled' - Indicates whether the VPC is enabled for ClassicLink.
mkVPCClassicLink ::
  VPCClassicLink
mkVPCClassicLink =
  VPCClassicLink'
    { vpcId = Lude.Nothing,
      tags = Lude.Nothing,
      classicLinkEnabled = Lude.Nothing
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vclVPCId :: Lens.Lens' VPCClassicLink (Lude.Maybe Lude.Text)
vclVPCId = Lens.lens (vpcId :: VPCClassicLink -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCClassicLink)
{-# DEPRECATED vclVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Any tags assigned to the VPC.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vclTags :: Lens.Lens' VPCClassicLink (Lude.Maybe [Tag])
vclTags = Lens.lens (tags :: VPCClassicLink -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPCClassicLink)
{-# DEPRECATED vclTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether the VPC is enabled for ClassicLink.
--
-- /Note:/ Consider using 'classicLinkEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vclClassicLinkEnabled :: Lens.Lens' VPCClassicLink (Lude.Maybe Lude.Bool)
vclClassicLinkEnabled = Lens.lens (classicLinkEnabled :: VPCClassicLink -> Lude.Maybe Lude.Bool) (\s a -> s {classicLinkEnabled = a} :: VPCClassicLink)
{-# DEPRECATED vclClassicLinkEnabled "Use generic-lens or generic-optics with 'classicLinkEnabled' instead." #-}

instance Lude.FromXML VPCClassicLink where
  parseXML x =
    VPCClassicLink'
      Lude.<$> (x Lude..@? "vpcId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "classicLinkEnabled")
