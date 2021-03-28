{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ServiceActionDetail
  ( ServiceActionDetail (..)
  -- * Smart constructor
  , mkServiceActionDetail
  -- * Lenses
  , sadDefinition
  , sadServiceActionSummary
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionValue as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionSummary as Types

-- | An object containing detailed information about the self-service action.
--
-- /See:/ 'mkServiceActionDetail' smart constructor.
data ServiceActionDetail = ServiceActionDetail'
  { definition :: Core.Maybe (Core.HashMap Types.ServiceActionDefinitionKey Types.ServiceActionDefinitionValue)
    -- ^ A map that defines the self-service action.
  , serviceActionSummary :: Core.Maybe Types.ServiceActionSummary
    -- ^ Summary information about the self-service action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceActionDetail' value with any optional fields omitted.
mkServiceActionDetail
    :: ServiceActionDetail
mkServiceActionDetail
  = ServiceActionDetail'{definition = Core.Nothing,
                         serviceActionSummary = Core.Nothing}

-- | A map that defines the self-service action.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadDefinition :: Lens.Lens' ServiceActionDetail (Core.Maybe (Core.HashMap Types.ServiceActionDefinitionKey Types.ServiceActionDefinitionValue))
sadDefinition = Lens.field @"definition"
{-# INLINEABLE sadDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | Summary information about the self-service action.
--
-- /Note:/ Consider using 'serviceActionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadServiceActionSummary :: Lens.Lens' ServiceActionDetail (Core.Maybe Types.ServiceActionSummary)
sadServiceActionSummary = Lens.field @"serviceActionSummary"
{-# INLINEABLE sadServiceActionSummary #-}
{-# DEPRECATED serviceActionSummary "Use generic-lens or generic-optics with 'serviceActionSummary' instead"  #-}

instance Core.FromJSON ServiceActionDetail where
        parseJSON
          = Core.withObject "ServiceActionDetail" Core.$
              \ x ->
                ServiceActionDetail' Core.<$>
                  (x Core..:? "Definition") Core.<*>
                    x Core..:? "ServiceActionSummary"
