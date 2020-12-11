-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroup
  ( SecurityGroup (..),

    -- * Smart constructor
    mkSecurityGroup,

    -- * Lenses
    sgVPCId,
    sgIPPermissions,
    sgIPPermissionsEgress,
    sgTags,
    sgOwnerId,
    sgGroupId,
    sgGroupName,
    sgDescription,
  )
where

import Network.AWS.EC2.Types.IPPermission
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a security group
--
-- /See:/ 'mkSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { vpcId :: Lude.Maybe Lude.Text,
    ipPermissions :: Lude.Maybe [IPPermission],
    ipPermissionsEgress :: Lude.Maybe [IPPermission],
    tags :: Lude.Maybe [Tag],
    ownerId :: Lude.Text,
    groupId :: Lude.Text,
    groupName :: Lude.Text,
    description :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityGroup' with the minimum fields required to make a request.
--
-- * 'description' - A description of the security group.
-- * 'groupId' - The ID of the security group.
-- * 'groupName' - The name of the security group.
-- * 'ipPermissions' - The inbound rules associated with the security group.
-- * 'ipPermissionsEgress' - [VPC only] The outbound rules associated with the security group.
-- * 'ownerId' - The AWS account ID of the owner of the security group.
-- * 'tags' - Any tags assigned to the security group.
-- * 'vpcId' - [VPC only] The ID of the VPC for the security group.
mkSecurityGroup ::
  -- | 'ownerId'
  Lude.Text ->
  -- | 'groupId'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  SecurityGroup
mkSecurityGroup pOwnerId_ pGroupId_ pGroupName_ pDescription_ =
  SecurityGroup'
    { vpcId = Lude.Nothing,
      ipPermissions = Lude.Nothing,
      ipPermissionsEgress = Lude.Nothing,
      tags = Lude.Nothing,
      ownerId = pOwnerId_,
      groupId = pGroupId_,
      groupName = pGroupName_,
      description = pDescription_
    }

-- | [VPC only] The ID of the VPC for the security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgVPCId :: Lens.Lens' SecurityGroup (Lude.Maybe Lude.Text)
sgVPCId = Lens.lens (vpcId :: SecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: SecurityGroup)
{-# DEPRECATED sgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The inbound rules associated with the security group.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgIPPermissions :: Lens.Lens' SecurityGroup (Lude.Maybe [IPPermission])
sgIPPermissions = Lens.lens (ipPermissions :: SecurityGroup -> Lude.Maybe [IPPermission]) (\s a -> s {ipPermissions = a} :: SecurityGroup)
{-# DEPRECATED sgIPPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | [VPC only] The outbound rules associated with the security group.
--
-- /Note:/ Consider using 'ipPermissionsEgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgIPPermissionsEgress :: Lens.Lens' SecurityGroup (Lude.Maybe [IPPermission])
sgIPPermissionsEgress = Lens.lens (ipPermissionsEgress :: SecurityGroup -> Lude.Maybe [IPPermission]) (\s a -> s {ipPermissionsEgress = a} :: SecurityGroup)
{-# DEPRECATED sgIPPermissionsEgress "Use generic-lens or generic-optics with 'ipPermissionsEgress' instead." #-}

-- | Any tags assigned to the security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgTags :: Lens.Lens' SecurityGroup (Lude.Maybe [Tag])
sgTags = Lens.lens (tags :: SecurityGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SecurityGroup)
{-# DEPRECATED sgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The AWS account ID of the owner of the security group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgOwnerId :: Lens.Lens' SecurityGroup Lude.Text
sgOwnerId = Lens.lens (ownerId :: SecurityGroup -> Lude.Text) (\s a -> s {ownerId = a} :: SecurityGroup)
{-# DEPRECATED sgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupId :: Lens.Lens' SecurityGroup Lude.Text
sgGroupId = Lens.lens (groupId :: SecurityGroup -> Lude.Text) (\s a -> s {groupId = a} :: SecurityGroup)
{-# DEPRECATED sgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupName :: Lens.Lens' SecurityGroup Lude.Text
sgGroupName = Lens.lens (groupName :: SecurityGroup -> Lude.Text) (\s a -> s {groupName = a} :: SecurityGroup)
{-# DEPRECATED sgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A description of the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SecurityGroup Lude.Text
sgDescription = Lens.lens (description :: SecurityGroup -> Lude.Text) (\s a -> s {description = a} :: SecurityGroup)
{-# DEPRECATED sgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML SecurityGroup where
  parseXML x =
    SecurityGroup'
      Lude.<$> (x Lude..@? "vpcId")
      Lude.<*> ( x Lude..@? "ipPermissions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "ipPermissionsEgress" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "ownerId")
      Lude.<*> (x Lude..@ "groupId")
      Lude.<*> (x Lude..@ "groupName")
      Lude.<*> (x Lude..@ "groupDescription")
