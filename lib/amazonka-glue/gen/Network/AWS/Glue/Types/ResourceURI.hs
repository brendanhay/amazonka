{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ResourceURI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ResourceURI
  ( ResourceURI (..),

    -- * Smart constructor
    mkResourceURI,

    -- * Lenses
    ruResourceType,
    ruURI,
  )
where

import Network.AWS.Glue.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The URIs for function resources.
--
-- /See:/ 'mkResourceURI' smart constructor.
data ResourceURI = ResourceURI'
  { resourceType ::
      Lude.Maybe ResourceType,
    uri :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceURI' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of the resource.
-- * 'uri' - The URI for accessing the resource.
mkResourceURI ::
  ResourceURI
mkResourceURI =
  ResourceURI' {resourceType = Lude.Nothing, uri = Lude.Nothing}

-- | The type of the resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruResourceType :: Lens.Lens' ResourceURI (Lude.Maybe ResourceType)
ruResourceType = Lens.lens (resourceType :: ResourceURI -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ResourceURI)
{-# DEPRECATED ruResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The URI for accessing the resource.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruURI :: Lens.Lens' ResourceURI (Lude.Maybe Lude.Text)
ruURI = Lens.lens (uri :: ResourceURI -> Lude.Maybe Lude.Text) (\s a -> s {uri = a} :: ResourceURI)
{-# DEPRECATED ruURI "Use generic-lens or generic-optics with 'uri' instead." #-}

instance Lude.FromJSON ResourceURI where
  parseJSON =
    Lude.withObject
      "ResourceURI"
      ( \x ->
          ResourceURI'
            Lude.<$> (x Lude..:? "ResourceType") Lude.<*> (x Lude..:? "Uri")
      )

instance Lude.ToJSON ResourceURI where
  toJSON ResourceURI' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("Uri" Lude..=) Lude.<$> uri
          ]
      )
