{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CompliantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CompliantSummary
  ( CompliantSummary (..),

    -- * Smart constructor
    mkCompliantSummary,

    -- * Lenses
    csCompliantCount,
    csSeveritySummary,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.SeveritySummary

-- | A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.
--
-- /See:/ 'mkCompliantSummary' smart constructor.
data CompliantSummary = CompliantSummary'
  { -- | The total number of resources that are compliant.
    compliantCount :: Lude.Maybe Lude.Int,
    -- | A summary of the compliance severity by compliance type.
    severitySummary :: Lude.Maybe SeveritySummary
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompliantSummary' with the minimum fields required to make a request.
--
-- * 'compliantCount' - The total number of resources that are compliant.
-- * 'severitySummary' - A summary of the compliance severity by compliance type.
mkCompliantSummary ::
  CompliantSummary
mkCompliantSummary =
  CompliantSummary'
    { compliantCount = Lude.Nothing,
      severitySummary = Lude.Nothing
    }

-- | The total number of resources that are compliant.
--
-- /Note:/ Consider using 'compliantCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCompliantCount :: Lens.Lens' CompliantSummary (Lude.Maybe Lude.Int)
csCompliantCount = Lens.lens (compliantCount :: CompliantSummary -> Lude.Maybe Lude.Int) (\s a -> s {compliantCount = a} :: CompliantSummary)
{-# DEPRECATED csCompliantCount "Use generic-lens or generic-optics with 'compliantCount' instead." #-}

-- | A summary of the compliance severity by compliance type.
--
-- /Note:/ Consider using 'severitySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSeveritySummary :: Lens.Lens' CompliantSummary (Lude.Maybe SeveritySummary)
csSeveritySummary = Lens.lens (severitySummary :: CompliantSummary -> Lude.Maybe SeveritySummary) (\s a -> s {severitySummary = a} :: CompliantSummary)
{-# DEPRECATED csSeveritySummary "Use generic-lens or generic-optics with 'severitySummary' instead." #-}

instance Lude.FromJSON CompliantSummary where
  parseJSON =
    Lude.withObject
      "CompliantSummary"
      ( \x ->
          CompliantSummary'
            Lude.<$> (x Lude..:? "CompliantCount")
            Lude.<*> (x Lude..:? "SeveritySummary")
      )
