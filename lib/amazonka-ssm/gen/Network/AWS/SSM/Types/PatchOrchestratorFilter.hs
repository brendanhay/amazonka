{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchOrchestratorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchOrchestratorFilter
  ( PatchOrchestratorFilter (..),

    -- * Smart constructor
    mkPatchOrchestratorFilter,

    -- * Lenses
    pofValues,
    pofKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a filter used in Patch Manager APIs.
--
-- /See:/ 'mkPatchOrchestratorFilter' smart constructor.
data PatchOrchestratorFilter = PatchOrchestratorFilter'
  { -- | The value for the filter.
    values :: Lude.Maybe [Lude.Text],
    -- | The key for the filter.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchOrchestratorFilter' with the minimum fields required to make a request.
--
-- * 'values' - The value for the filter.
-- * 'key' - The key for the filter.
mkPatchOrchestratorFilter ::
  PatchOrchestratorFilter
mkPatchOrchestratorFilter =
  PatchOrchestratorFilter'
    { values = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The value for the filter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pofValues :: Lens.Lens' PatchOrchestratorFilter (Lude.Maybe [Lude.Text])
pofValues = Lens.lens (values :: PatchOrchestratorFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: PatchOrchestratorFilter)
{-# DEPRECATED pofValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The key for the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pofKey :: Lens.Lens' PatchOrchestratorFilter (Lude.Maybe Lude.Text)
pofKey = Lens.lens (key :: PatchOrchestratorFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: PatchOrchestratorFilter)
{-# DEPRECATED pofKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON PatchOrchestratorFilter where
  toJSON PatchOrchestratorFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Values" Lude..=) Lude.<$> values, ("Key" Lude..=) Lude.<$> key]
      )
