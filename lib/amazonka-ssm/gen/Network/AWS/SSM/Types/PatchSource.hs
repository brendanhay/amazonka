{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchSource
  ( PatchSource (..),

    -- * Smart constructor
    mkPatchSource,

    -- * Lenses
    psName,
    psProducts,
    psConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.
--
-- /See:/ 'mkPatchSource' smart constructor.
data PatchSource = PatchSource'
  { name :: Lude.Text,
    products :: Lude.NonEmpty Lude.Text,
    configuration :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchSource' with the minimum fields required to make a request.
--
-- * 'configuration' - The value of the yum repo configuration. For example:
--
-- @[main]@
-- @cachedir=/var/cache/yum/$basesearch$releasever@
-- @keepcache=0@
-- @debuglevel=2@
-- * 'name' - The name specified to identify the patch source.
-- * 'products' - The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
mkPatchSource ::
  -- | 'name'
  Lude.Text ->
  -- | 'products'
  Lude.NonEmpty Lude.Text ->
  -- | 'configuration'
  Lude.Sensitive Lude.Text ->
  PatchSource
mkPatchSource pName_ pProducts_ pConfiguration_ =
  PatchSource'
    { name = pName_,
      products = pProducts_,
      configuration = pConfiguration_
    }

-- | The name specified to identify the patch source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PatchSource Lude.Text
psName = Lens.lens (name :: PatchSource -> Lude.Text) (\s a -> s {name = a} :: PatchSource)
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
--
-- /Note:/ Consider using 'products' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psProducts :: Lens.Lens' PatchSource (Lude.NonEmpty Lude.Text)
psProducts = Lens.lens (products :: PatchSource -> Lude.NonEmpty Lude.Text) (\s a -> s {products = a} :: PatchSource)
{-# DEPRECATED psProducts "Use generic-lens or generic-optics with 'products' instead." #-}

-- | The value of the yum repo configuration. For example:
--
-- @[main]@
-- @cachedir=/var/cache/yum/$basesearch$releasever@
-- @keepcache=0@
-- @debuglevel=2@
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psConfiguration :: Lens.Lens' PatchSource (Lude.Sensitive Lude.Text)
psConfiguration = Lens.lens (configuration :: PatchSource -> Lude.Sensitive Lude.Text) (\s a -> s {configuration = a} :: PatchSource)
{-# DEPRECATED psConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

instance Lude.FromJSON PatchSource where
  parseJSON =
    Lude.withObject
      "PatchSource"
      ( \x ->
          PatchSource'
            Lude.<$> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Products")
            Lude.<*> (x Lude..: "Configuration")
      )

instance Lude.ToJSON PatchSource where
  toJSON PatchSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Products" Lude..= products),
            Lude.Just ("Configuration" Lude..= configuration)
          ]
      )
