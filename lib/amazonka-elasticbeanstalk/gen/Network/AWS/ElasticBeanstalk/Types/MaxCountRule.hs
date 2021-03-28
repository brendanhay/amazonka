{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxCountRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.MaxCountRule
  ( MaxCountRule (..)
  -- * Smart constructor
  , mkMaxCountRule
  -- * Lenses
  , mcrEnabled
  , mcrDeleteSourceFromS3
  , mcrMaxCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A lifecycle rule that deletes the oldest application version when the maximum count is exceeded.
--
-- /See:/ 'mkMaxCountRule' smart constructor.
data MaxCountRule = MaxCountRule'
  { enabled :: Core.Bool
    -- ^ Specify @true@ to apply the rule, or @false@ to disable it.
  , deleteSourceFromS3 :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
  , maxCount :: Core.Maybe Core.Int
    -- ^ Specify the maximum number of application versions to retain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaxCountRule' value with any optional fields omitted.
mkMaxCountRule
    :: Core.Bool -- ^ 'enabled'
    -> MaxCountRule
mkMaxCountRule enabled
  = MaxCountRule'{enabled, deleteSourceFromS3 = Core.Nothing,
                  maxCount = Core.Nothing}

-- | Specify @true@ to apply the rule, or @false@ to disable it.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrEnabled :: Lens.Lens' MaxCountRule Core.Bool
mcrEnabled = Lens.field @"enabled"
{-# INLINEABLE mcrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- /Note:/ Consider using 'deleteSourceFromS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrDeleteSourceFromS3 :: Lens.Lens' MaxCountRule (Core.Maybe Core.Bool)
mcrDeleteSourceFromS3 = Lens.field @"deleteSourceFromS3"
{-# INLINEABLE mcrDeleteSourceFromS3 #-}
{-# DEPRECATED deleteSourceFromS3 "Use generic-lens or generic-optics with 'deleteSourceFromS3' instead"  #-}

-- | Specify the maximum number of application versions to retain.
--
-- /Note:/ Consider using 'maxCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrMaxCount :: Lens.Lens' MaxCountRule (Core.Maybe Core.Int)
mcrMaxCount = Lens.field @"maxCount"
{-# INLINEABLE mcrMaxCount #-}
{-# DEPRECATED maxCount "Use generic-lens or generic-optics with 'maxCount' instead"  #-}

instance Core.ToQuery MaxCountRule where
        toQuery MaxCountRule{..}
          = Core.toQueryPair "Enabled" enabled Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeleteSourceFromS3")
                deleteSourceFromS3
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxCount") maxCount

instance Core.FromXML MaxCountRule where
        parseXML x
          = MaxCountRule' Core.<$>
              (x Core..@ "Enabled") Core.<*> x Core..@? "DeleteSourceFromS3"
                Core.<*> x Core..@? "MaxCount"
