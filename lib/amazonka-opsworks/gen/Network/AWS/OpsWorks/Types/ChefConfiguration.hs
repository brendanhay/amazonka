-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ChefConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ChefConfiguration
  ( ChefConfiguration (..),

    -- * Smart constructor
    mkChefConfiguration,

    -- * Lenses
    ccBerkshelfVersion,
    ccManageBerkshelf,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Chef configuration.
--
-- /See:/ 'mkChefConfiguration' smart constructor.
data ChefConfiguration = ChefConfiguration'
  { berkshelfVersion ::
      Lude.Maybe Lude.Text,
    manageBerkshelf :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChefConfiguration' with the minimum fields required to make a request.
--
-- * 'berkshelfVersion' - The Berkshelf version.
-- * 'manageBerkshelf' - Whether to enable Berkshelf.
mkChefConfiguration ::
  ChefConfiguration
mkChefConfiguration =
  ChefConfiguration'
    { berkshelfVersion = Lude.Nothing,
      manageBerkshelf = Lude.Nothing
    }

-- | The Berkshelf version.
--
-- /Note:/ Consider using 'berkshelfVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBerkshelfVersion :: Lens.Lens' ChefConfiguration (Lude.Maybe Lude.Text)
ccBerkshelfVersion = Lens.lens (berkshelfVersion :: ChefConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {berkshelfVersion = a} :: ChefConfiguration)
{-# DEPRECATED ccBerkshelfVersion "Use generic-lens or generic-optics with 'berkshelfVersion' instead." #-}

-- | Whether to enable Berkshelf.
--
-- /Note:/ Consider using 'manageBerkshelf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccManageBerkshelf :: Lens.Lens' ChefConfiguration (Lude.Maybe Lude.Bool)
ccManageBerkshelf = Lens.lens (manageBerkshelf :: ChefConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {manageBerkshelf = a} :: ChefConfiguration)
{-# DEPRECATED ccManageBerkshelf "Use generic-lens or generic-optics with 'manageBerkshelf' instead." #-}

instance Lude.FromJSON ChefConfiguration where
  parseJSON =
    Lude.withObject
      "ChefConfiguration"
      ( \x ->
          ChefConfiguration'
            Lude.<$> (x Lude..:? "BerkshelfVersion")
            Lude.<*> (x Lude..:? "ManageBerkshelf")
      )

instance Lude.ToJSON ChefConfiguration where
  toJSON ChefConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BerkshelfVersion" Lude..=) Lude.<$> berkshelfVersion,
            ("ManageBerkshelf" Lude..=) Lude.<$> manageBerkshelf
          ]
      )
