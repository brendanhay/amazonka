{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDefinitionVersion
  ( ResourceDefinitionVersion (..),

    -- * Smart constructor
    mkResourceDefinitionVersion,

    -- * Lenses
    rdvResources,
  )
where

import Network.AWS.Greengrass.Types.Resource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a resource definition version.
--
-- /See:/ 'mkResourceDefinitionVersion' smart constructor.
newtype ResourceDefinitionVersion = ResourceDefinitionVersion'
  { resources ::
      Lude.Maybe [Resource]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'resources' - A list of resources.
mkResourceDefinitionVersion ::
  ResourceDefinitionVersion
mkResourceDefinitionVersion =
  ResourceDefinitionVersion' {resources = Lude.Nothing}

-- | A list of resources.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdvResources :: Lens.Lens' ResourceDefinitionVersion (Lude.Maybe [Resource])
rdvResources = Lens.lens (resources :: ResourceDefinitionVersion -> Lude.Maybe [Resource]) (\s a -> s {resources = a} :: ResourceDefinitionVersion)
{-# DEPRECATED rdvResources "Use generic-lens or generic-optics with 'resources' instead." #-}

instance Lude.FromJSON ResourceDefinitionVersion where
  parseJSON =
    Lude.withObject
      "ResourceDefinitionVersion"
      ( \x ->
          ResourceDefinitionVersion'
            Lude.<$> (x Lude..:? "Resources" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ResourceDefinitionVersion where
  toJSON ResourceDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Resources" Lude..=) Lude.<$> resources])
