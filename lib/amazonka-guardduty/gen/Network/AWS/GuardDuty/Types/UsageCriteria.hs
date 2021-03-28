{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.UsageCriteria
  ( UsageCriteria (..)
  -- * Smart constructor
  , mkUsageCriteria
  -- * Lenses
  , ucDataSources
  , ucAccountIds
  , ucResources
  ) where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.DataSource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the criteria used to query usage statistics.
--
-- /See:/ 'mkUsageCriteria' smart constructor.
data UsageCriteria = UsageCriteria'
  { dataSources :: [Types.DataSource]
    -- ^ The data sources to aggregate usage statistics from.
  , accountIds :: Core.Maybe (Core.NonEmpty Types.AccountId)
    -- ^ The account IDs to aggregate usage statistics from.
  , resources :: Core.Maybe [Core.Text]
    -- ^ The resources to aggregate usage statistics from. Only accepts exact resource names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsageCriteria' value with any optional fields omitted.
mkUsageCriteria
    :: UsageCriteria
mkUsageCriteria
  = UsageCriteria'{dataSources = Core.mempty,
                   accountIds = Core.Nothing, resources = Core.Nothing}

-- | The data sources to aggregate usage statistics from.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDataSources :: Lens.Lens' UsageCriteria [Types.DataSource]
ucDataSources = Lens.field @"dataSources"
{-# INLINEABLE ucDataSources #-}
{-# DEPRECATED dataSources "Use generic-lens or generic-optics with 'dataSources' instead"  #-}

-- | The account IDs to aggregate usage statistics from.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucAccountIds :: Lens.Lens' UsageCriteria (Core.Maybe (Core.NonEmpty Types.AccountId))
ucAccountIds = Lens.field @"accountIds"
{-# INLINEABLE ucAccountIds #-}
{-# DEPRECATED accountIds "Use generic-lens or generic-optics with 'accountIds' instead"  #-}

-- | The resources to aggregate usage statistics from. Only accepts exact resource names.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucResources :: Lens.Lens' UsageCriteria (Core.Maybe [Core.Text])
ucResources = Lens.field @"resources"
{-# INLINEABLE ucResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

instance Core.FromJSON UsageCriteria where
        toJSON UsageCriteria{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("dataSources" Core..= dataSources),
                  ("accountIds" Core..=) Core.<$> accountIds,
                  ("resources" Core..=) Core.<$> resources])
