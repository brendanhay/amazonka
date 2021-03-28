{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.TagInfoForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.TagInfoForResource
  ( TagInfoForResource (..)
  -- * Smart constructor
  , mkTagInfoForResource
  -- * Lenses
  , tifrResourceARN
  , tifrTagList
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ResourceARN as Types
import qualified Network.AWS.WAFRegional.Types.Tag as Types

-- | Information for a tag associated with an AWS resource. Tags are key:value pairs that you can use to categorize and manage your resources, for purposes like billing. For example, you might set the tag key to "customer" and the value to the customer name or ID. You can specify one or more tags to add to each AWS resource, up to 50 tags for a resource.
--
-- Tagging is only available through the API, SDKs, and CLI. You can't manage or view tags through the AWS WAF Classic console. You can tag the AWS resources that you manage through AWS WAF Classic: web ACLs, rule groups, and rules. 
--
-- /See:/ 'mkTagInfoForResource' smart constructor.
data TagInfoForResource = TagInfoForResource'
  { resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ 
  , tagList :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagInfoForResource' value with any optional fields omitted.
mkTagInfoForResource
    :: TagInfoForResource
mkTagInfoForResource
  = TagInfoForResource'{resourceARN = Core.Nothing,
                        tagList = Core.Nothing}

-- | 
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifrResourceARN :: Lens.Lens' TagInfoForResource (Core.Maybe Types.ResourceARN)
tifrResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE tifrResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifrTagList :: Lens.Lens' TagInfoForResource (Core.Maybe (Core.NonEmpty Types.Tag))
tifrTagList = Lens.field @"tagList"
{-# INLINEABLE tifrTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

instance Core.FromJSON TagInfoForResource where
        parseJSON
          = Core.withObject "TagInfoForResource" Core.$
              \ x ->
                TagInfoForResource' Core.<$>
                  (x Core..:? "ResourceARN") Core.<*> x Core..:? "TagList"
