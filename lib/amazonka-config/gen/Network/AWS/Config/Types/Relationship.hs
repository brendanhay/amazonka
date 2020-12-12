{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Relationship
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Relationship
  ( Relationship (..),

    -- * Smart constructor
    mkRelationship,

    -- * Lenses
    rResourceId,
    rResourceType,
    rResourceName,
    rRelationshipName,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The relationship of the related resource to the main resource.
--
-- /See:/ 'mkRelationship' smart constructor.
data Relationship = Relationship'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    resourceName :: Lude.Maybe Lude.Text,
    relationshipName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Relationship' with the minimum fields required to make a request.
--
-- * 'relationshipName' - The type of relationship with the related resource.
-- * 'resourceId' - The ID of the related resource (for example, @sg-xxxxxx@ ).
-- * 'resourceName' - The custom name of the related resource, if available.
-- * 'resourceType' - The resource type of the related resource.
mkRelationship ::
  Relationship
mkRelationship =
  Relationship'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      resourceName = Lude.Nothing,
      relationshipName = Lude.Nothing
    }

-- | The ID of the related resource (for example, @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceId :: Lens.Lens' Relationship (Lude.Maybe Lude.Text)
rResourceId = Lens.lens (resourceId :: Relationship -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: Relationship)
{-# DEPRECATED rResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type of the related resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' Relationship (Lude.Maybe ResourceType)
rResourceType = Lens.lens (resourceType :: Relationship -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Relationship)
{-# DEPRECATED rResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The custom name of the related resource, if available.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceName :: Lens.Lens' Relationship (Lude.Maybe Lude.Text)
rResourceName = Lens.lens (resourceName :: Relationship -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: Relationship)
{-# DEPRECATED rResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The type of relationship with the related resource.
--
-- /Note:/ Consider using 'relationshipName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRelationshipName :: Lens.Lens' Relationship (Lude.Maybe Lude.Text)
rRelationshipName = Lens.lens (relationshipName :: Relationship -> Lude.Maybe Lude.Text) (\s a -> s {relationshipName = a} :: Relationship)
{-# DEPRECATED rRelationshipName "Use generic-lens or generic-optics with 'relationshipName' instead." #-}

instance Lude.FromJSON Relationship where
  parseJSON =
    Lude.withObject
      "Relationship"
      ( \x ->
          Relationship'
            Lude.<$> (x Lude..:? "resourceId")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "resourceName")
            Lude.<*> (x Lude..:? "relationshipName")
      )
