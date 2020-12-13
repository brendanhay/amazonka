{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateResourceIdentifier
  ( AggregateResourceIdentifier (..),

    -- * Smart constructor
    mkAggregateResourceIdentifier,

    -- * Lenses
    ariResourceId,
    ariResourceType,
    ariSourceRegion,
    ariResourceName,
    ariSourceAccountId,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details that identify a resource that is collected by AWS Config aggregator, including the resource type, ID, (if available) the custom resource name, the source account, and source region.
--
-- /See:/ 'mkAggregateResourceIdentifier' smart constructor.
data AggregateResourceIdentifier = AggregateResourceIdentifier'
  { -- | The ID of the AWS resource.
    resourceId :: Lude.Text,
    -- | The type of the AWS resource.
    resourceType :: ResourceType,
    -- | The source region where data is aggregated.
    sourceRegion :: Lude.Text,
    -- | The name of the AWS resource.
    resourceName :: Lude.Maybe Lude.Text,
    -- | The 12-digit account ID of the source account.
    sourceAccountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AggregateResourceIdentifier' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the AWS resource.
-- * 'resourceType' - The type of the AWS resource.
-- * 'sourceRegion' - The source region where data is aggregated.
-- * 'resourceName' - The name of the AWS resource.
-- * 'sourceAccountId' - The 12-digit account ID of the source account.
mkAggregateResourceIdentifier ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'sourceRegion'
  Lude.Text ->
  -- | 'sourceAccountId'
  Lude.Text ->
  AggregateResourceIdentifier
mkAggregateResourceIdentifier
  pResourceId_
  pResourceType_
  pSourceRegion_
  pSourceAccountId_ =
    AggregateResourceIdentifier'
      { resourceId = pResourceId_,
        resourceType = pResourceType_,
        sourceRegion = pSourceRegion_,
        resourceName = Lude.Nothing,
        sourceAccountId = pSourceAccountId_
      }

-- | The ID of the AWS resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariResourceId :: Lens.Lens' AggregateResourceIdentifier Lude.Text
ariResourceId = Lens.lens (resourceId :: AggregateResourceIdentifier -> Lude.Text) (\s a -> s {resourceId = a} :: AggregateResourceIdentifier)
{-# DEPRECATED ariResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariResourceType :: Lens.Lens' AggregateResourceIdentifier ResourceType
ariResourceType = Lens.lens (resourceType :: AggregateResourceIdentifier -> ResourceType) (\s a -> s {resourceType = a} :: AggregateResourceIdentifier)
{-# DEPRECATED ariResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The source region where data is aggregated.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariSourceRegion :: Lens.Lens' AggregateResourceIdentifier Lude.Text
ariSourceRegion = Lens.lens (sourceRegion :: AggregateResourceIdentifier -> Lude.Text) (\s a -> s {sourceRegion = a} :: AggregateResourceIdentifier)
{-# DEPRECATED ariSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | The name of the AWS resource.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariResourceName :: Lens.Lens' AggregateResourceIdentifier (Lude.Maybe Lude.Text)
ariResourceName = Lens.lens (resourceName :: AggregateResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: AggregateResourceIdentifier)
{-# DEPRECATED ariResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'sourceAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariSourceAccountId :: Lens.Lens' AggregateResourceIdentifier Lude.Text
ariSourceAccountId = Lens.lens (sourceAccountId :: AggregateResourceIdentifier -> Lude.Text) (\s a -> s {sourceAccountId = a} :: AggregateResourceIdentifier)
{-# DEPRECATED ariSourceAccountId "Use generic-lens or generic-optics with 'sourceAccountId' instead." #-}

instance Lude.FromJSON AggregateResourceIdentifier where
  parseJSON =
    Lude.withObject
      "AggregateResourceIdentifier"
      ( \x ->
          AggregateResourceIdentifier'
            Lude.<$> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "ResourceType")
            Lude.<*> (x Lude..: "SourceRegion")
            Lude.<*> (x Lude..:? "ResourceName")
            Lude.<*> (x Lude..: "SourceAccountId")
      )

instance Lude.ToJSON AggregateResourceIdentifier where
  toJSON AggregateResourceIdentifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("SourceRegion" Lude..= sourceRegion),
            ("ResourceName" Lude..=) Lude.<$> resourceName,
            Lude.Just ("SourceAccountId" Lude..= sourceAccountId)
          ]
      )
