{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.AggregateResourceIdentifier
  ( AggregateResourceIdentifier (..)
  -- * Smart constructor
  , mkAggregateResourceIdentifier
  -- * Lenses
  , ariSourceAccountId
  , ariSourceRegion
  , ariResourceId
  , ariResourceType
  , ariResourceName
  ) where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.ResourceName as Types
import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details that identify a resource that is collected by AWS Config aggregator, including the resource type, ID, (if available) the custom resource name, the source account, and source region.
--
-- /See:/ 'mkAggregateResourceIdentifier' smart constructor.
data AggregateResourceIdentifier = AggregateResourceIdentifier'
  { sourceAccountId :: Types.AccountId
    -- ^ The 12-digit account ID of the source account.
  , sourceRegion :: Types.AwsRegion
    -- ^ The source region where data is aggregated.
  , resourceId :: Types.ResourceId
    -- ^ The ID of the AWS resource.
  , resourceType :: Types.ResourceType
    -- ^ The type of the AWS resource.
  , resourceName :: Core.Maybe Types.ResourceName
    -- ^ The name of the AWS resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AggregateResourceIdentifier' value with any optional fields omitted.
mkAggregateResourceIdentifier
    :: Types.AccountId -- ^ 'sourceAccountId'
    -> Types.AwsRegion -- ^ 'sourceRegion'
    -> Types.ResourceId -- ^ 'resourceId'
    -> Types.ResourceType -- ^ 'resourceType'
    -> AggregateResourceIdentifier
mkAggregateResourceIdentifier sourceAccountId sourceRegion
  resourceId resourceType
  = AggregateResourceIdentifier'{sourceAccountId, sourceRegion,
                                 resourceId, resourceType, resourceName = Core.Nothing}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'sourceAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariSourceAccountId :: Lens.Lens' AggregateResourceIdentifier Types.AccountId
ariSourceAccountId = Lens.field @"sourceAccountId"
{-# INLINEABLE ariSourceAccountId #-}
{-# DEPRECATED sourceAccountId "Use generic-lens or generic-optics with 'sourceAccountId' instead"  #-}

-- | The source region where data is aggregated.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariSourceRegion :: Lens.Lens' AggregateResourceIdentifier Types.AwsRegion
ariSourceRegion = Lens.field @"sourceRegion"
{-# INLINEABLE ariSourceRegion #-}
{-# DEPRECATED sourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead"  #-}

-- | The ID of the AWS resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariResourceId :: Lens.Lens' AggregateResourceIdentifier Types.ResourceId
ariResourceId = Lens.field @"resourceId"
{-# INLINEABLE ariResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of the AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariResourceType :: Lens.Lens' AggregateResourceIdentifier Types.ResourceType
ariResourceType = Lens.field @"resourceType"
{-# INLINEABLE ariResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The name of the AWS resource.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ariResourceName :: Lens.Lens' AggregateResourceIdentifier (Core.Maybe Types.ResourceName)
ariResourceName = Lens.field @"resourceName"
{-# INLINEABLE ariResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

instance Core.FromJSON AggregateResourceIdentifier where
        toJSON AggregateResourceIdentifier{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SourceAccountId" Core..= sourceAccountId),
                  Core.Just ("SourceRegion" Core..= sourceRegion),
                  Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("ResourceType" Core..= resourceType),
                  ("ResourceName" Core..=) Core.<$> resourceName])

instance Core.FromJSON AggregateResourceIdentifier where
        parseJSON
          = Core.withObject "AggregateResourceIdentifier" Core.$
              \ x ->
                AggregateResourceIdentifier' Core.<$>
                  (x Core..: "SourceAccountId") Core.<*> x Core..: "SourceRegion"
                    Core.<*> x Core..: "ResourceId"
                    Core.<*> x Core..: "ResourceType"
                    Core.<*> x Core..:? "ResourceName"
