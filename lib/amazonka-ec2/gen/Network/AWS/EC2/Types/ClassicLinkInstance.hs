{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLinkInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLinkInstance
  ( ClassicLinkInstance (..),

    -- * Smart constructor
    mkClassicLinkInstance,

    -- * Lenses
    cliInstanceId,
    cliGroups,
    cliVPCId,
    cliTags,
  )
where

import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a linked EC2-Classic instance.
--
-- /See:/ 'mkClassicLinkInstance' smart constructor.
data ClassicLinkInstance = ClassicLinkInstance'
  { instanceId ::
      Lude.Maybe Lude.Text,
    groups :: Lude.Maybe [GroupIdentifier],
    vpcId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClassicLinkInstance' with the minimum fields required to make a request.
--
-- * 'groups' - A list of security groups.
-- * 'instanceId' - The ID of the instance.
-- * 'tags' - Any tags assigned to the instance.
-- * 'vpcId' - The ID of the VPC.
mkClassicLinkInstance ::
  ClassicLinkInstance
mkClassicLinkInstance =
  ClassicLinkInstance'
    { instanceId = Lude.Nothing,
      groups = Lude.Nothing,
      vpcId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cliInstanceId :: Lens.Lens' ClassicLinkInstance (Lude.Maybe Lude.Text)
cliInstanceId = Lens.lens (instanceId :: ClassicLinkInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ClassicLinkInstance)
{-# DEPRECATED cliInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A list of security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cliGroups :: Lens.Lens' ClassicLinkInstance (Lude.Maybe [GroupIdentifier])
cliGroups = Lens.lens (groups :: ClassicLinkInstance -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groups = a} :: ClassicLinkInstance)
{-# DEPRECATED cliGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cliVPCId :: Lens.Lens' ClassicLinkInstance (Lude.Maybe Lude.Text)
cliVPCId = Lens.lens (vpcId :: ClassicLinkInstance -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: ClassicLinkInstance)
{-# DEPRECATED cliVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Any tags assigned to the instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cliTags :: Lens.Lens' ClassicLinkInstance (Lude.Maybe [Tag])
cliTags = Lens.lens (tags :: ClassicLinkInstance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ClassicLinkInstance)
{-# DEPRECATED cliTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ClassicLinkInstance where
  parseXML x =
    ClassicLinkInstance'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
