{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightImpactGraphEdge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightImpactGraphEdge
  ( InsightImpactGraphEdge (..),

    -- * Smart constructor
    mkInsightImpactGraphEdge,

    -- * Lenses
    iigeReferenceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The connection between two service in an insight impact graph.
--
-- /See:/ 'mkInsightImpactGraphEdge' smart constructor.
newtype InsightImpactGraphEdge = InsightImpactGraphEdge'
  { -- | Identifier of the edge. Unique within a service map.
    referenceId :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InsightImpactGraphEdge' value with any optional fields omitted.
mkInsightImpactGraphEdge ::
  InsightImpactGraphEdge
mkInsightImpactGraphEdge =
  InsightImpactGraphEdge' {referenceId = Core.Nothing}

-- | Identifier of the edge. Unique within a service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigeReferenceId :: Lens.Lens' InsightImpactGraphEdge (Core.Maybe Core.Int)
iigeReferenceId = Lens.field @"referenceId"
{-# DEPRECATED iigeReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

instance Core.FromJSON InsightImpactGraphEdge where
  parseJSON =
    Core.withObject "InsightImpactGraphEdge" Core.$
      \x -> InsightImpactGraphEdge' Core.<$> (x Core..:? "ReferenceId")
