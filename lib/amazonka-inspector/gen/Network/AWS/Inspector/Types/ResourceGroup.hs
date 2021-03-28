{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ResourceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.ResourceGroup
  ( ResourceGroup (..)
  -- * Smart constructor
  , mkResourceGroup
  -- * Lenses
  , rgArn
  , rgTags
  , rgCreatedAt
  ) where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.ResourceGroupTag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a resource group. The resource group defines a set of tags that, when queried, identify the AWS resources that make up the assessment target. This data type is used as the response element in the 'DescribeResourceGroups' action.
--
-- /See:/ 'mkResourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
  { arn :: Types.Arn
    -- ^ The ARN of the resource group.
  , tags :: Core.NonEmpty Types.ResourceGroupTag
    -- ^ The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
  , createdAt :: Core.NominalDiffTime
    -- ^ The time at which resource group is created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResourceGroup' value with any optional fields omitted.
mkResourceGroup
    :: Types.Arn -- ^ 'arn'
    -> Core.NonEmpty Types.ResourceGroupTag -- ^ 'tags'
    -> Core.NominalDiffTime -- ^ 'createdAt'
    -> ResourceGroup
mkResourceGroup arn tags createdAt
  = ResourceGroup'{arn, tags, createdAt}

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgArn :: Lens.Lens' ResourceGroup Types.Arn
rgArn = Lens.field @"arn"
{-# INLINEABLE rgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTags :: Lens.Lens' ResourceGroup (Core.NonEmpty Types.ResourceGroupTag)
rgTags = Lens.field @"tags"
{-# INLINEABLE rgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The time at which resource group is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCreatedAt :: Lens.Lens' ResourceGroup Core.NominalDiffTime
rgCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE rgCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

instance Core.FromJSON ResourceGroup where
        parseJSON
          = Core.withObject "ResourceGroup" Core.$
              \ x ->
                ResourceGroup' Core.<$>
                  (x Core..: "arn") Core.<*> x Core..: "tags" Core.<*>
                    x Core..: "createdAt"
