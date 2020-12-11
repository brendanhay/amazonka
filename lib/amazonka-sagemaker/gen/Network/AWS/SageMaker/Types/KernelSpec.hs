-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelSpec
  ( KernelSpec (..),

    -- * Smart constructor
    mkKernelSpec,

    -- * Lenses
    ksDisplayName,
    ksName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The specification of a Jupyter kernel.
--
-- /See:/ 'mkKernelSpec' smart constructor.
data KernelSpec = KernelSpec'
  { displayName :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KernelSpec' with the minimum fields required to make a request.
--
-- * 'displayName' - The display name of the kernel.
-- * 'name' - The name of the kernel.
mkKernelSpec ::
  -- | 'name'
  Lude.Text ->
  KernelSpec
mkKernelSpec pName_ =
  KernelSpec' {displayName = Lude.Nothing, name = pName_}

-- | The display name of the kernel.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksDisplayName :: Lens.Lens' KernelSpec (Lude.Maybe Lude.Text)
ksDisplayName = Lens.lens (displayName :: KernelSpec -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: KernelSpec)
{-# DEPRECATED ksDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the kernel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksName :: Lens.Lens' KernelSpec Lude.Text
ksName = Lens.lens (name :: KernelSpec -> Lude.Text) (\s a -> s {name = a} :: KernelSpec)
{-# DEPRECATED ksName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON KernelSpec where
  parseJSON =
    Lude.withObject
      "KernelSpec"
      ( \x ->
          KernelSpec'
            Lude.<$> (x Lude..:? "DisplayName") Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON KernelSpec where
  toJSON KernelSpec' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayName" Lude..=) Lude.<$> displayName,
            Lude.Just ("Name" Lude..= name)
          ]
      )
