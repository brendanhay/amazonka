{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EC2SecurityGroup
  ( EC2SecurityGroup (..),

    -- * Smart constructor
    mkEC2SecurityGroup,

    -- * Lenses
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,
    esgTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes an Amazon EC2 security group.
--
-- /See:/ 'mkEC2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { status ::
      Lude.Maybe Lude.Text,
    ec2SecurityGroupOwnerId :: Lude.Maybe Lude.Text,
    ec2SecurityGroupName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'EC2SecurityGroup' with the minimum fields required to make a request.
--
-- * 'ec2SecurityGroupName' - The name of the EC2 Security Group.
-- * 'ec2SecurityGroupOwnerId' - The AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
-- * 'status' - The status of the EC2 security group.
-- * 'tags' - The list of tags for the EC2 security group.
mkEC2SecurityGroup ::
  EC2SecurityGroup
mkEC2SecurityGroup =
  EC2SecurityGroup'
    { status = Lude.Nothing,
      ec2SecurityGroupOwnerId = Lude.Nothing,
      ec2SecurityGroupName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The status of the EC2 security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esgStatus :: Lens.Lens' EC2SecurityGroup (Lude.Maybe Lude.Text)
esgStatus = Lens.lens (status :: EC2SecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: EC2SecurityGroup)
{-# DEPRECATED esgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
--
-- /Note:/ Consider using 'ec2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esgEC2SecurityGroupOwnerId :: Lens.Lens' EC2SecurityGroup (Lude.Maybe Lude.Text)
esgEC2SecurityGroupOwnerId = Lens.lens (ec2SecurityGroupOwnerId :: EC2SecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupOwnerId = a} :: EC2SecurityGroup)
{-# DEPRECATED esgEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'ec2SecurityGroupOwnerId' instead." #-}

-- | The name of the EC2 Security Group.
--
-- /Note:/ Consider using 'ec2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esgEC2SecurityGroupName :: Lens.Lens' EC2SecurityGroup (Lude.Maybe Lude.Text)
esgEC2SecurityGroupName = Lens.lens (ec2SecurityGroupName :: EC2SecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupName = a} :: EC2SecurityGroup)
{-# DEPRECATED esgEC2SecurityGroupName "Use generic-lens or generic-optics with 'ec2SecurityGroupName' instead." #-}

-- | The list of tags for the EC2 security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esgTags :: Lens.Lens' EC2SecurityGroup (Lude.Maybe [Tag])
esgTags = Lens.lens (tags :: EC2SecurityGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: EC2SecurityGroup)
{-# DEPRECATED esgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML EC2SecurityGroup where
  parseXML x =
    EC2SecurityGroup'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "EC2SecurityGroupOwnerId")
      Lude.<*> (x Lude..@? "EC2SecurityGroupName")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
