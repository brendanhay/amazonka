{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.EC2SecurityGroup
  ( EC2SecurityGroup (..)
  -- * Smart constructor
  , mkEC2SecurityGroup
  -- * Lenses
  , ecsgEC2SecurityGroupName
  , ecsgEC2SecurityGroupOwnerId
  , ecsgStatus
  , ecsgTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes an Amazon EC2 security group.
--
-- /See:/ 'mkEC2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { eC2SecurityGroupName :: Core.Maybe Core.Text
    -- ^ The name of the EC2 Security Group.
  , eC2SecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ The AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field. 
  , status :: Core.Maybe Core.Text
    -- ^ The status of the EC2 security group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the EC2 security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2SecurityGroup' value with any optional fields omitted.
mkEC2SecurityGroup
    :: EC2SecurityGroup
mkEC2SecurityGroup
  = EC2SecurityGroup'{eC2SecurityGroupName = Core.Nothing,
                      eC2SecurityGroupOwnerId = Core.Nothing, status = Core.Nothing,
                      tags = Core.Nothing}

-- | The name of the EC2 Security Group.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupName :: Lens.Lens' EC2SecurityGroup (Core.Maybe Core.Text)
ecsgEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# INLINEABLE ecsgEC2SecurityGroupName #-}
{-# DEPRECATED eC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead"  #-}

-- | The AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field. 
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgEC2SecurityGroupOwnerId :: Lens.Lens' EC2SecurityGroup (Core.Maybe Core.Text)
ecsgEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# INLINEABLE ecsgEC2SecurityGroupOwnerId #-}
{-# DEPRECATED eC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead"  #-}

-- | The status of the EC2 security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgStatus :: Lens.Lens' EC2SecurityGroup (Core.Maybe Core.Text)
ecsgStatus = Lens.field @"status"
{-# INLINEABLE ecsgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The list of tags for the EC2 security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsgTags :: Lens.Lens' EC2SecurityGroup (Core.Maybe [Types.Tag])
ecsgTags = Lens.field @"tags"
{-# INLINEABLE ecsgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' Core.<$>
              (x Core..@? "EC2SecurityGroupName") Core.<*>
                x Core..@? "EC2SecurityGroupOwnerId"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
