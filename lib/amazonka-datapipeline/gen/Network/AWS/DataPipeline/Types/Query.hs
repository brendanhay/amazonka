{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Query
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.Query
  ( Query (..)
  -- * Smart constructor
  , mkQuery
  -- * Lenses
  , qSelectors
  ) where

import qualified Network.AWS.DataPipeline.Types.Selector as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines the query to run against an object.
--
-- /See:/ 'mkQuery' smart constructor.
newtype Query = Query'
  { selectors :: Core.Maybe [Types.Selector]
    -- ^ List of selectors that define the query. An object must satisfy all of the selectors to match the query.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Query' value with any optional fields omitted.
mkQuery
    :: Query
mkQuery = Query'{selectors = Core.Nothing}

-- | List of selectors that define the query. An object must satisfy all of the selectors to match the query.
--
-- /Note:/ Consider using 'selectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qSelectors :: Lens.Lens' Query (Core.Maybe [Types.Selector])
qSelectors = Lens.field @"selectors"
{-# INLINEABLE qSelectors #-}
{-# DEPRECATED selectors "Use generic-lens or generic-optics with 'selectors' instead"  #-}

instance Core.FromJSON Query where
        toJSON Query{..}
          = Core.object
              (Core.catMaybes [("selectors" Core..=) Core.<$> selectors])
