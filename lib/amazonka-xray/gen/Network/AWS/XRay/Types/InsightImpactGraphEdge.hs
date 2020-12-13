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
import qualified Network.AWS.Prelude as Lude

-- | The connection between two service in an insight impact graph.
--
-- /See:/ 'mkInsightImpactGraphEdge' smart constructor.
newtype InsightImpactGraphEdge = InsightImpactGraphEdge'
  { -- | Identifier of the edge. Unique within a service map.
    referenceId :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightImpactGraphEdge' with the minimum fields required to make a request.
--
-- * 'referenceId' - Identifier of the edge. Unique within a service map.
mkInsightImpactGraphEdge ::
  InsightImpactGraphEdge
mkInsightImpactGraphEdge =
  InsightImpactGraphEdge' {referenceId = Lude.Nothing}

-- | Identifier of the edge. Unique within a service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigeReferenceId :: Lens.Lens' InsightImpactGraphEdge (Lude.Maybe Lude.Int)
iigeReferenceId = Lens.lens (referenceId :: InsightImpactGraphEdge -> Lude.Maybe Lude.Int) (\s a -> s {referenceId = a} :: InsightImpactGraphEdge)
{-# DEPRECATED iigeReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

instance Lude.FromJSON InsightImpactGraphEdge where
  parseJSON =
    Lude.withObject
      "InsightImpactGraphEdge"
      ( \x ->
          InsightImpactGraphEdge' Lude.<$> (x Lude..:? "ReferenceId")
      )
