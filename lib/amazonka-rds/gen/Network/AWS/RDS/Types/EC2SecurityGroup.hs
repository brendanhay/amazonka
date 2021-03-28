{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.EC2SecurityGroup
  ( EC2SecurityGroup (..)
  -- * Smart constructor
  , mkEC2SecurityGroup
  -- * Lenses
  , ecsgEC2SecurityGroupId
  , ecsgEC2SecurityGroupName
  , ecsgEC2SecurityGroupOwnerId
  , ecsgStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { eC2SecurityGroupId :: Core.Maybe Core.Text
    -- ^ Specifies the id of the EC2 security group.
  , eC2SecurityGroupName :: Core.Maybe Core.Text
    -- ^ Specifies the name of the EC2 security group.
  , eC2SecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field. 
  , status :: Core.Maybe Core.Text
    -- ^ Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2SecurityGroup' value with any optional fields omitted.
mkEC2SecurityGroup
    :: EC2SecurityGroup
mkEC2SecurityGroup
  = EC2SecurityGroup'{eC2SecurityGroupId = Core.Nothing,
                      eC2SecurityGroupName = Core.Nothing,
                      eC2SecurityGroupOwnerId = Core.Nothing, status = Core.Nothing}

-- | Specifies the id of the EC2 security group.
--
-- /Note:/ Consider using 'eC2SecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupId :: Lens.Lens' EC2SecurityGroup (Core.Maybe Core.Text)
ecsgEC2SecurityGroupId = Lens.field @"eC2SecurityGroupId"
{-# INLINEABLE ecsgEC2SecurityGroupId #-}
{-# DEPRECATED eC2SecurityGroupId "Use generic-lens or generic-optics with 'eC2SecurityGroupId' instead"  #-}

-- | Specifies the name of the EC2 security group.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupName :: Lens.Lens' EC2SecurityGroup (Core.Maybe Core.Text)
ecsgEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# INLINEABLE ecsgEC2SecurityGroupName #-}
{-# DEPRECATED eC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead"  #-}

-- | Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field. 
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupOwnerId :: Lens.Lens' EC2SecurityGroup (Core.Maybe Core.Text)
ecsgEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# INLINEABLE ecsgEC2SecurityGroupOwnerId #-}
{-# DEPRECATED eC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead"  #-}

-- | Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgStatus :: Lens.Lens' EC2SecurityGroup (Core.Maybe Core.Text)
ecsgStatus = Lens.field @"status"
{-# INLINEABLE ecsgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' Core.<$>
              (x Core..@? "EC2SecurityGroupId") Core.<*>
                x Core..@? "EC2SecurityGroupName"
                Core.<*> x Core..@? "EC2SecurityGroupOwnerId"
                Core.<*> x Core..@? "Status"
