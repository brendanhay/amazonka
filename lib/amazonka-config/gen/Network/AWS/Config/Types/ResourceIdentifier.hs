-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceIdentifier
  ( ResourceIdentifier (..),

    -- * Smart constructor
    mkResourceIdentifier,

    -- * Lenses
    riResourceId,
    riResourceType,
    riResourceName,
    riResourceDeletionTime,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
--
-- /See:/ 'mkResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    resourceName :: Lude.Maybe Lude.Text,
    resourceDeletionTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- * 'resourceDeletionTime' - The time that the resource was deleted.
-- * 'resourceId' - The ID of the resource (for example, @sg-xxxxxx@ ).
-- * 'resourceName' - The custom name of the resource (if available).
-- * 'resourceType' - The type of resource.
mkResourceIdentifier ::
  ResourceIdentifier
mkResourceIdentifier =
  ResourceIdentifier'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      resourceName = Lude.Nothing,
      resourceDeletionTime = Lude.Nothing
    }

-- | The ID of the resource (for example, @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceId :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riResourceId = Lens.lens (resourceId :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ResourceIdentifier)
{-# DEPRECATED riResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceType :: Lens.Lens' ResourceIdentifier (Lude.Maybe ResourceType)
riResourceType = Lens.lens (resourceType :: ResourceIdentifier -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ResourceIdentifier)
{-# DEPRECATED riResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The custom name of the resource (if available).
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceName :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riResourceName = Lens.lens (resourceName :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: ResourceIdentifier)
{-# DEPRECATED riResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The time that the resource was deleted.
--
-- /Note:/ Consider using 'resourceDeletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceDeletionTime :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Timestamp)
riResourceDeletionTime = Lens.lens (resourceDeletionTime :: ResourceIdentifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {resourceDeletionTime = a} :: ResourceIdentifier)
{-# DEPRECATED riResourceDeletionTime "Use generic-lens or generic-optics with 'resourceDeletionTime' instead." #-}

instance Lude.FromJSON ResourceIdentifier where
  parseJSON =
    Lude.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Lude.<$> (x Lude..:? "resourceId")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "resourceName")
            Lude.<*> (x Lude..:? "resourceDeletionTime")
      )
