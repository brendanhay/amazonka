{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BatchRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BatchRestrictions
  ( BatchRestrictions (..),

    -- * Smart constructor
    mkBatchRestrictions,

    -- * Lenses
    brMaximumBuildsAllowed,
    brComputeTypesAllowed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies restrictions for the batch build.
--
-- /See:/ 'mkBatchRestrictions' smart constructor.
data BatchRestrictions = BatchRestrictions'
  { -- | Specifies the maximum number of builds allowed.
    maximumBuildsAllowed :: Lude.Maybe Lude.Int,
    -- | An array of strings that specify the compute types that are allowed for the batch build. See <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types> in the /AWS CodeBuild User Guide/ for these values.
    computeTypesAllowed :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchRestrictions' with the minimum fields required to make a request.
--
-- * 'maximumBuildsAllowed' - Specifies the maximum number of builds allowed.
-- * 'computeTypesAllowed' - An array of strings that specify the compute types that are allowed for the batch build. See <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types> in the /AWS CodeBuild User Guide/ for these values.
mkBatchRestrictions ::
  BatchRestrictions
mkBatchRestrictions =
  BatchRestrictions'
    { maximumBuildsAllowed = Lude.Nothing,
      computeTypesAllowed = Lude.Nothing
    }

-- | Specifies the maximum number of builds allowed.
--
-- /Note:/ Consider using 'maximumBuildsAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brMaximumBuildsAllowed :: Lens.Lens' BatchRestrictions (Lude.Maybe Lude.Int)
brMaximumBuildsAllowed = Lens.lens (maximumBuildsAllowed :: BatchRestrictions -> Lude.Maybe Lude.Int) (\s a -> s {maximumBuildsAllowed = a} :: BatchRestrictions)
{-# DEPRECATED brMaximumBuildsAllowed "Use generic-lens or generic-optics with 'maximumBuildsAllowed' instead." #-}

-- | An array of strings that specify the compute types that are allowed for the batch build. See <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types> in the /AWS CodeBuild User Guide/ for these values.
--
-- /Note:/ Consider using 'computeTypesAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brComputeTypesAllowed :: Lens.Lens' BatchRestrictions (Lude.Maybe [Lude.Text])
brComputeTypesAllowed = Lens.lens (computeTypesAllowed :: BatchRestrictions -> Lude.Maybe [Lude.Text]) (\s a -> s {computeTypesAllowed = a} :: BatchRestrictions)
{-# DEPRECATED brComputeTypesAllowed "Use generic-lens or generic-optics with 'computeTypesAllowed' instead." #-}

instance Lude.FromJSON BatchRestrictions where
  parseJSON =
    Lude.withObject
      "BatchRestrictions"
      ( \x ->
          BatchRestrictions'
            Lude.<$> (x Lude..:? "maximumBuildsAllowed")
            Lude.<*> (x Lude..:? "computeTypesAllowed" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON BatchRestrictions where
  toJSON BatchRestrictions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maximumBuildsAllowed" Lude..=) Lude.<$> maximumBuildsAllowed,
            ("computeTypesAllowed" Lude..=) Lude.<$> computeTypesAllowed
          ]
      )
