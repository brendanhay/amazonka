{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaseKpiResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.BaseKpiResult
  ( BaseKpiResult (..)
  -- * Smart constructor
  , mkBaseKpiResult
  -- * Lenses
  , bkrRows
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ResultRow as Types
import qualified Network.AWS.Prelude as Core

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
-- /See:/ 'mkBaseKpiResult' smart constructor.
newtype BaseKpiResult = BaseKpiResult'
  { rows :: [Types.ResultRow]
    -- ^ An array of objects that provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BaseKpiResult' value with any optional fields omitted.
mkBaseKpiResult
    :: BaseKpiResult
mkBaseKpiResult = BaseKpiResult'{rows = Core.mempty}

-- | An array of objects that provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
-- /Note:/ Consider using 'rows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bkrRows :: Lens.Lens' BaseKpiResult [Types.ResultRow]
bkrRows = Lens.field @"rows"
{-# INLINEABLE bkrRows #-}
{-# DEPRECATED rows "Use generic-lens or generic-optics with 'rows' instead"  #-}

instance Core.FromJSON BaseKpiResult where
        parseJSON
          = Core.withObject "BaseKpiResult" Core.$
              \ x ->
                BaseKpiResult' Core.<$> (x Core..:? "Rows" Core..!= Core.mempty)
