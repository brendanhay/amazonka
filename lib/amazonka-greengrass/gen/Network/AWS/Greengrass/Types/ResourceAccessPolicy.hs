-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceAccessPolicy
  ( ResourceAccessPolicy (..),

    -- * Smart constructor
    mkResourceAccessPolicy,

    -- * Lenses
    rapPermission,
    rapResourceId,
  )
where

import Network.AWS.Greengrass.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A policy used by the function to access a resource.
--
-- /See:/ 'mkResourceAccessPolicy' smart constructor.
data ResourceAccessPolicy = ResourceAccessPolicy'
  { permission ::
      Lude.Maybe Permission,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceAccessPolicy' with the minimum fields required to make a request.
--
-- * 'permission' - The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
-- * 'resourceId' - The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
mkResourceAccessPolicy ::
  -- | 'resourceId'
  Lude.Text ->
  ResourceAccessPolicy
mkResourceAccessPolicy pResourceId_ =
  ResourceAccessPolicy'
    { permission = Lude.Nothing,
      resourceId = pResourceId_
    }

-- | The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rapPermission :: Lens.Lens' ResourceAccessPolicy (Lude.Maybe Permission)
rapPermission = Lens.lens (permission :: ResourceAccessPolicy -> Lude.Maybe Permission) (\s a -> s {permission = a} :: ResourceAccessPolicy)
{-# DEPRECATED rapPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

-- | The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rapResourceId :: Lens.Lens' ResourceAccessPolicy Lude.Text
rapResourceId = Lens.lens (resourceId :: ResourceAccessPolicy -> Lude.Text) (\s a -> s {resourceId = a} :: ResourceAccessPolicy)
{-# DEPRECATED rapResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.FromJSON ResourceAccessPolicy where
  parseJSON =
    Lude.withObject
      "ResourceAccessPolicy"
      ( \x ->
          ResourceAccessPolicy'
            Lude.<$> (x Lude..:? "Permission") Lude.<*> (x Lude..: "ResourceId")
      )

instance Lude.ToJSON ResourceAccessPolicy where
  toJSON ResourceAccessPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Permission" Lude..=) Lude.<$> permission,
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )
