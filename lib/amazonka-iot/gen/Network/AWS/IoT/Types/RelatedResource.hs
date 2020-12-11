-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RelatedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RelatedResource
  ( RelatedResource (..),

    -- * Smart constructor
    mkRelatedResource,

    -- * Lenses
    rrAdditionalInfo,
    rrResourceType,
    rrResourceIdentifier,
  )
where

import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a related resource.
--
-- /See:/ 'mkRelatedResource' smart constructor.
data RelatedResource = RelatedResource'
  { additionalInfo ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    resourceType :: Lude.Maybe ResourceType,
    resourceIdentifier :: Lude.Maybe ResourceIdentifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelatedResource' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - Other information about the resource.
-- * 'resourceIdentifier' - Information that identifies the resource.
-- * 'resourceType' - The type of resource.
mkRelatedResource ::
  RelatedResource
mkRelatedResource =
  RelatedResource'
    { additionalInfo = Lude.Nothing,
      resourceType = Lude.Nothing,
      resourceIdentifier = Lude.Nothing
    }

-- | Other information about the resource.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrAdditionalInfo :: Lens.Lens' RelatedResource (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rrAdditionalInfo = Lens.lens (additionalInfo :: RelatedResource -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {additionalInfo = a} :: RelatedResource)
{-# DEPRECATED rrAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrResourceType :: Lens.Lens' RelatedResource (Lude.Maybe ResourceType)
rrResourceType = Lens.lens (resourceType :: RelatedResource -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: RelatedResource)
{-# DEPRECATED rrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Information that identifies the resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrResourceIdentifier :: Lens.Lens' RelatedResource (Lude.Maybe ResourceIdentifier)
rrResourceIdentifier = Lens.lens (resourceIdentifier :: RelatedResource -> Lude.Maybe ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: RelatedResource)
{-# DEPRECATED rrResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.FromJSON RelatedResource where
  parseJSON =
    Lude.withObject
      "RelatedResource"
      ( \x ->
          RelatedResource'
            Lude.<$> (x Lude..:? "additionalInfo" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "resourceIdentifier")
      )
