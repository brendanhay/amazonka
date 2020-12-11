-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserIdGroupPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserIdGroupPair
  ( UserIdGroupPair (..),

    -- * Smart constructor
    mkUserIdGroupPair,

    -- * Lenses
    uigpVPCPeeringConnectionId,
    uigpVPCId,
    uigpUserId,
    uigpGroupId,
    uigpGroupName,
    uigpDescription,
    uigpPeeringStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a security group and AWS account ID pair.
--
-- /See:/ 'mkUserIdGroupPair' smart constructor.
data UserIdGroupPair = UserIdGroupPair'
  { vpcPeeringConnectionId ::
      Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    userId :: Lude.Maybe Lude.Text,
    groupId :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    peeringStatus :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserIdGroupPair' with the minimum fields required to make a request.
--
-- * 'description' - A description for the security group rule that references this user ID group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
-- * 'groupId' - The ID of the security group.
-- * 'groupName' - The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID.
--
-- For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
-- * 'peeringStatus' - The status of a VPC peering connection, if applicable.
-- * 'userId' - The ID of an AWS account.
--
-- For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned.
-- [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
-- * 'vpcId' - The ID of the VPC for the referenced security group, if applicable.
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection, if applicable.
mkUserIdGroupPair ::
  UserIdGroupPair
mkUserIdGroupPair =
  UserIdGroupPair'
    { vpcPeeringConnectionId = Lude.Nothing,
      vpcId = Lude.Nothing,
      userId = Lude.Nothing,
      groupId = Lude.Nothing,
      groupName = Lude.Nothing,
      description = Lude.Nothing,
      peeringStatus = Lude.Nothing
    }

-- | The ID of the VPC peering connection, if applicable.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpVPCPeeringConnectionId :: Lens.Lens' UserIdGroupPair (Lude.Maybe Lude.Text)
uigpVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: UserIdGroupPair -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: UserIdGroupPair)
{-# DEPRECATED uigpVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The ID of the VPC for the referenced security group, if applicable.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpVPCId :: Lens.Lens' UserIdGroupPair (Lude.Maybe Lude.Text)
uigpVPCId = Lens.lens (vpcId :: UserIdGroupPair -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: UserIdGroupPair)
{-# DEPRECATED uigpVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of an AWS account.
--
-- For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned.
-- [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpUserId :: Lens.Lens' UserIdGroupPair (Lude.Maybe Lude.Text)
uigpUserId = Lens.lens (userId :: UserIdGroupPair -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: UserIdGroupPair)
{-# DEPRECATED uigpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpGroupId :: Lens.Lens' UserIdGroupPair (Lude.Maybe Lude.Text)
uigpGroupId = Lens.lens (groupId :: UserIdGroupPair -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: UserIdGroupPair)
{-# DEPRECATED uigpGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID.
--
-- For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpGroupName :: Lens.Lens' UserIdGroupPair (Lude.Maybe Lude.Text)
uigpGroupName = Lens.lens (groupName :: UserIdGroupPair -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: UserIdGroupPair)
{-# DEPRECATED uigpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A description for the security group rule that references this user ID group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpDescription :: Lens.Lens' UserIdGroupPair (Lude.Maybe Lude.Text)
uigpDescription = Lens.lens (description :: UserIdGroupPair -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UserIdGroupPair)
{-# DEPRECATED uigpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The status of a VPC peering connection, if applicable.
--
-- /Note:/ Consider using 'peeringStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpPeeringStatus :: Lens.Lens' UserIdGroupPair (Lude.Maybe Lude.Text)
uigpPeeringStatus = Lens.lens (peeringStatus :: UserIdGroupPair -> Lude.Maybe Lude.Text) (\s a -> s {peeringStatus = a} :: UserIdGroupPair)
{-# DEPRECATED uigpPeeringStatus "Use generic-lens or generic-optics with 'peeringStatus' instead." #-}

instance Lude.FromXML UserIdGroupPair where
  parseXML x =
    UserIdGroupPair'
      Lude.<$> (x Lude..@? "vpcPeeringConnectionId")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "userId")
      Lude.<*> (x Lude..@? "groupId")
      Lude.<*> (x Lude..@? "groupName")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "peeringStatus")

instance Lude.ToQuery UserIdGroupPair where
  toQuery UserIdGroupPair' {..} =
    Lude.mconcat
      [ "VpcPeeringConnectionId" Lude.=: vpcPeeringConnectionId,
        "VpcId" Lude.=: vpcId,
        "UserId" Lude.=: userId,
        "GroupId" Lude.=: groupId,
        "GroupName" Lude.=: groupName,
        "Description" Lude.=: description,
        "PeeringStatus" Lude.=: peeringStatus
      ]
