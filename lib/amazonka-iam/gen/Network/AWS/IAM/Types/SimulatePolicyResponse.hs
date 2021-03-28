{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SimulatePolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.SimulatePolicyResponse
  ( SimulatePolicyResponse (..)
  -- * Smart constructor
  , mkSimulatePolicyResponse
  -- * Lenses
  , sprEvaluationResults
  , sprIsTruncated
  , sprMarker
  ) where

import qualified Network.AWS.IAM.Types.EvaluationResult as Types
import qualified Network.AWS.IAM.Types.Marker as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the response to a successful 'SimulatePrincipalPolicy' or 'SimulateCustomPolicy' request.
--
-- /See:/ 'mkSimulatePolicyResponse' smart constructor.
data SimulatePolicyResponse = SimulatePolicyResponse'
  { evaluationResults :: Core.Maybe [Types.EvaluationResult]
    -- ^ The results of the simulation.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
  , marker :: Core.Maybe Types.Marker
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SimulatePolicyResponse' value with any optional fields omitted.
mkSimulatePolicyResponse
    :: SimulatePolicyResponse
mkSimulatePolicyResponse
  = SimulatePolicyResponse'{evaluationResults = Core.Nothing,
                            isTruncated = Core.Nothing, marker = Core.Nothing}

-- | The results of the simulation.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprEvaluationResults :: Lens.Lens' SimulatePolicyResponse (Core.Maybe [Types.EvaluationResult])
sprEvaluationResults = Lens.field @"evaluationResults"
{-# INLINEABLE sprEvaluationResults #-}
{-# DEPRECATED evaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead"  #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprIsTruncated :: Lens.Lens' SimulatePolicyResponse (Core.Maybe Core.Bool)
sprIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE sprIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprMarker :: Lens.Lens' SimulatePolicyResponse (Core.Maybe Types.Marker)
sprMarker = Lens.field @"marker"
{-# INLINEABLE sprMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.FromXML SimulatePolicyResponse where
        parseXML x
          = SimulatePolicyResponse' Core.<$>
              (x Core..@? "EvaluationResults" Core..<@>
                 Core.parseXMLList "member")
                Core.<*> x Core..@? "IsTruncated"
                Core.<*> x Core..@? "Marker"
