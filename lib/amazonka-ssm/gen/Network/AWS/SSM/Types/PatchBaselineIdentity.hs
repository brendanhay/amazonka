-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchBaselineIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchBaselineIdentity
  ( PatchBaselineIdentity (..),

    -- * Smart constructor
    mkPatchBaselineIdentity,

    -- * Lenses
    pbiBaselineName,
    pbiBaselineDescription,
    pbiOperatingSystem,
    pbiDefaultBaseline,
    pbiBaselineId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OperatingSystem

-- | Defines the basic information about a patch baseline.
--
-- /See:/ 'mkPatchBaselineIdentity' smart constructor.
data PatchBaselineIdentity = PatchBaselineIdentity'
  { baselineName ::
      Lude.Maybe Lude.Text,
    baselineDescription :: Lude.Maybe Lude.Text,
    operatingSystem :: Lude.Maybe OperatingSystem,
    defaultBaseline :: Lude.Maybe Lude.Bool,
    baselineId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchBaselineIdentity' with the minimum fields required to make a request.
--
-- * 'baselineDescription' - The description of the patch baseline.
-- * 'baselineId' - The ID of the patch baseline.
-- * 'baselineName' - The name of the patch baseline.
-- * 'defaultBaseline' - Whether this is the default baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
-- * 'operatingSystem' - Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
mkPatchBaselineIdentity ::
  PatchBaselineIdentity
mkPatchBaselineIdentity =
  PatchBaselineIdentity'
    { baselineName = Lude.Nothing,
      baselineDescription = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      defaultBaseline = Lude.Nothing,
      baselineId = Lude.Nothing
    }

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'baselineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiBaselineName :: Lens.Lens' PatchBaselineIdentity (Lude.Maybe Lude.Text)
pbiBaselineName = Lens.lens (baselineName :: PatchBaselineIdentity -> Lude.Maybe Lude.Text) (\s a -> s {baselineName = a} :: PatchBaselineIdentity)
{-# DEPRECATED pbiBaselineName "Use generic-lens or generic-optics with 'baselineName' instead." #-}

-- | The description of the patch baseline.
--
-- /Note:/ Consider using 'baselineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiBaselineDescription :: Lens.Lens' PatchBaselineIdentity (Lude.Maybe Lude.Text)
pbiBaselineDescription = Lens.lens (baselineDescription :: PatchBaselineIdentity -> Lude.Maybe Lude.Text) (\s a -> s {baselineDescription = a} :: PatchBaselineIdentity)
{-# DEPRECATED pbiBaselineDescription "Use generic-lens or generic-optics with 'baselineDescription' instead." #-}

-- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiOperatingSystem :: Lens.Lens' PatchBaselineIdentity (Lude.Maybe OperatingSystem)
pbiOperatingSystem = Lens.lens (operatingSystem :: PatchBaselineIdentity -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: PatchBaselineIdentity)
{-# DEPRECATED pbiOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | Whether this is the default baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
--
-- /Note:/ Consider using 'defaultBaseline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiDefaultBaseline :: Lens.Lens' PatchBaselineIdentity (Lude.Maybe Lude.Bool)
pbiDefaultBaseline = Lens.lens (defaultBaseline :: PatchBaselineIdentity -> Lude.Maybe Lude.Bool) (\s a -> s {defaultBaseline = a} :: PatchBaselineIdentity)
{-# DEPRECATED pbiDefaultBaseline "Use generic-lens or generic-optics with 'defaultBaseline' instead." #-}

-- | The ID of the patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiBaselineId :: Lens.Lens' PatchBaselineIdentity (Lude.Maybe Lude.Text)
pbiBaselineId = Lens.lens (baselineId :: PatchBaselineIdentity -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: PatchBaselineIdentity)
{-# DEPRECATED pbiBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

instance Lude.FromJSON PatchBaselineIdentity where
  parseJSON =
    Lude.withObject
      "PatchBaselineIdentity"
      ( \x ->
          PatchBaselineIdentity'
            Lude.<$> (x Lude..:? "BaselineName")
            Lude.<*> (x Lude..:? "BaselineDescription")
            Lude.<*> (x Lude..:? "OperatingSystem")
            Lude.<*> (x Lude..:? "DefaultBaseline")
            Lude.<*> (x Lude..:? "BaselineId")
      )
