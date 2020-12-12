{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SimulatePolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SimulatePolicyResponse
  ( SimulatePolicyResponse (..),

    -- * Smart constructor
    mkSimulatePolicyResponse,

    -- * Lenses
    spEvaluationResults,
    spMarker,
    spIsTruncated,
  )
where

import Network.AWS.IAM.Types.EvaluationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the response to a successful 'SimulatePrincipalPolicy' or 'SimulateCustomPolicy' request.
--
-- /See:/ 'mkSimulatePolicyResponse' smart constructor.
data SimulatePolicyResponse = SimulatePolicyResponse'
  { evaluationResults ::
      Lude.Maybe [EvaluationResult],
    marker :: Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SimulatePolicyResponse' with the minimum fields required to make a request.
--
-- * 'evaluationResults' - The results of the simulation.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
mkSimulatePolicyResponse ::
  SimulatePolicyResponse
mkSimulatePolicyResponse =
  SimulatePolicyResponse'
    { evaluationResults = Lude.Nothing,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing
    }

-- | The results of the simulation.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spEvaluationResults :: Lens.Lens' SimulatePolicyResponse (Lude.Maybe [EvaluationResult])
spEvaluationResults = Lens.lens (evaluationResults :: SimulatePolicyResponse -> Lude.Maybe [EvaluationResult]) (\s a -> s {evaluationResults = a} :: SimulatePolicyResponse)
{-# DEPRECATED spEvaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMarker :: Lens.Lens' SimulatePolicyResponse (Lude.Maybe Lude.Text)
spMarker = Lens.lens (marker :: SimulatePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: SimulatePolicyResponse)
{-# DEPRECATED spMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spIsTruncated :: Lens.Lens' SimulatePolicyResponse (Lude.Maybe Lude.Bool)
spIsTruncated = Lens.lens (isTruncated :: SimulatePolicyResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: SimulatePolicyResponse)
{-# DEPRECATED spIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

instance Lude.FromXML SimulatePolicyResponse where
  parseXML x =
    SimulatePolicyResponse'
      Lude.<$> ( x Lude..@? "EvaluationResults" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Marker")
      Lude.<*> (x Lude..@? "IsTruncated")
