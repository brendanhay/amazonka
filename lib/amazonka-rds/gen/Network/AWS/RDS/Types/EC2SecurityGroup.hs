{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EC2SecurityGroup
  ( EC2SecurityGroup (..),

    -- * Smart constructor
    mkEC2SecurityGroup,

    -- * Lenses
    ecsgEC2SecurityGroupId,
    ecsgEC2SecurityGroupName,
    ecsgEC2SecurityGroupOwnerId,
    ecsgStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.EC2SecurityGroupId as Types
import qualified Network.AWS.RDS.Types.EC2SecurityGroupName as Types
import qualified Network.AWS.RDS.Types.EC2SecurityGroupOwnerId as Types
import qualified Network.AWS.RDS.Types.Status as Types

-- | This data type is used as a response element in the following actions:
--
--
--     * @AuthorizeDBSecurityGroupIngress@
--
--
--     * @DescribeDBSecurityGroups@
--
--
--     * @RevokeDBSecurityGroupIngress@
--
--
--
-- /See:/ 'mkEC2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { -- | Specifies the id of the EC2 security group.
    eC2SecurityGroupId :: Core.Maybe Types.EC2SecurityGroupId,
    -- | Specifies the name of the EC2 security group.
    eC2SecurityGroupName :: Core.Maybe Types.EC2SecurityGroupName,
    -- | Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
    eC2SecurityGroupOwnerId :: Core.Maybe Types.EC2SecurityGroupOwnerId,
    -- | Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
    status :: Core.Maybe Types.Status
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2SecurityGroup' value with any optional fields omitted.
mkEC2SecurityGroup ::
  EC2SecurityGroup
mkEC2SecurityGroup =
  EC2SecurityGroup'
    { eC2SecurityGroupId = Core.Nothing,
      eC2SecurityGroupName = Core.Nothing,
      eC2SecurityGroupOwnerId = Core.Nothing,
      status = Core.Nothing
    }

-- | Specifies the id of the EC2 security group.
--
-- /Note:/ Consider using 'eC2SecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupId :: Lens.Lens' EC2SecurityGroup (Core.Maybe Types.EC2SecurityGroupId)
ecsgEC2SecurityGroupId = Lens.field @"eC2SecurityGroupId"
{-# DEPRECATED ecsgEC2SecurityGroupId "Use generic-lens or generic-optics with 'eC2SecurityGroupId' instead." #-}

-- | Specifies the name of the EC2 security group.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupName :: Lens.Lens' EC2SecurityGroup (Core.Maybe Types.EC2SecurityGroupName)
ecsgEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# DEPRECATED ecsgEC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead." #-}

-- | Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupOwnerId :: Lens.Lens' EC2SecurityGroup (Core.Maybe Types.EC2SecurityGroupOwnerId)
ecsgEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# DEPRECATED ecsgEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead." #-}

-- | Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgStatus :: Lens.Lens' EC2SecurityGroup (Core.Maybe Types.Status)
ecsgStatus = Lens.field @"status"
{-# DEPRECATED ecsgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML EC2SecurityGroup where
  parseXML x =
    EC2SecurityGroup'
      Core.<$> (x Core..@? "EC2SecurityGroupId")
      Core.<*> (x Core..@? "EC2SecurityGroupName")
      Core.<*> (x Core..@? "EC2SecurityGroupOwnerId")
      Core.<*> (x Core..@? "Status")
