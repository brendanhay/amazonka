-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NonCompliantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NonCompliantSummary
  ( NonCompliantSummary (..),

    -- * Smart constructor
    mkNonCompliantSummary,

    -- * Lenses
    ncsNonCompliantCount,
    ncsSeveritySummary,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.SeveritySummary

-- | A summary of resources that are not compliant. The summary is organized according to resource type.
--
-- /See:/ 'mkNonCompliantSummary' smart constructor.
data NonCompliantSummary = NonCompliantSummary'
  { nonCompliantCount ::
      Lude.Maybe Lude.Int,
    severitySummary :: Lude.Maybe SeveritySummary
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NonCompliantSummary' with the minimum fields required to make a request.
--
-- * 'nonCompliantCount' - The total number of compliance items that are not compliant.
-- * 'severitySummary' - A summary of the non-compliance severity by compliance type
mkNonCompliantSummary ::
  NonCompliantSummary
mkNonCompliantSummary =
  NonCompliantSummary'
    { nonCompliantCount = Lude.Nothing,
      severitySummary = Lude.Nothing
    }

-- | The total number of compliance items that are not compliant.
--
-- /Note:/ Consider using 'nonCompliantCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncsNonCompliantCount :: Lens.Lens' NonCompliantSummary (Lude.Maybe Lude.Int)
ncsNonCompliantCount = Lens.lens (nonCompliantCount :: NonCompliantSummary -> Lude.Maybe Lude.Int) (\s a -> s {nonCompliantCount = a} :: NonCompliantSummary)
{-# DEPRECATED ncsNonCompliantCount "Use generic-lens or generic-optics with 'nonCompliantCount' instead." #-}

-- | A summary of the non-compliance severity by compliance type
--
-- /Note:/ Consider using 'severitySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncsSeveritySummary :: Lens.Lens' NonCompliantSummary (Lude.Maybe SeveritySummary)
ncsSeveritySummary = Lens.lens (severitySummary :: NonCompliantSummary -> Lude.Maybe SeveritySummary) (\s a -> s {severitySummary = a} :: NonCompliantSummary)
{-# DEPRECATED ncsSeveritySummary "Use generic-lens or generic-optics with 'severitySummary' instead." #-}

instance Lude.FromJSON NonCompliantSummary where
  parseJSON =
    Lude.withObject
      "NonCompliantSummary"
      ( \x ->
          NonCompliantSummary'
            Lude.<$> (x Lude..:? "NonCompliantCount")
            Lude.<*> (x Lude..:? "SeveritySummary")
      )
