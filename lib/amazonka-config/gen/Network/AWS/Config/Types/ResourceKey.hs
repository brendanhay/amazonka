{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceKey
  ( ResourceKey (..),

    -- * Smart constructor
    mkResourceKey,

    -- * Lenses
    rkResourceId,
    rkResourceType,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details that identify a resource within AWS Config, including the resource type and resource ID.
--
-- /See:/ 'mkResourceKey' smart constructor.
data ResourceKey = ResourceKey'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Lude.Text,
    -- | The resource type.
    resourceType :: ResourceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceKey' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource (for example., sg-xxxxxx).
-- * 'resourceType' - The resource type.
mkResourceKey ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  ResourceType ->
  ResourceKey
mkResourceKey pResourceId_ pResourceType_ =
  ResourceKey'
    { resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rkResourceId :: Lens.Lens' ResourceKey Lude.Text
rkResourceId = Lens.lens (resourceId :: ResourceKey -> Lude.Text) (\s a -> s {resourceId = a} :: ResourceKey)
{-# DEPRECATED rkResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rkResourceType :: Lens.Lens' ResourceKey ResourceType
rkResourceType = Lens.lens (resourceType :: ResourceKey -> ResourceType) (\s a -> s {resourceType = a} :: ResourceKey)
{-# DEPRECATED rkResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.FromJSON ResourceKey where
  parseJSON =
    Lude.withObject
      "ResourceKey"
      ( \x ->
          ResourceKey'
            Lude.<$> (x Lude..: "resourceId") Lude.<*> (x Lude..: "resourceType")
      )

instance Lude.ToJSON ResourceKey where
  toJSON ResourceKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceId" Lude..= resourceId),
            Lude.Just ("resourceType" Lude..= resourceType)
          ]
      )
