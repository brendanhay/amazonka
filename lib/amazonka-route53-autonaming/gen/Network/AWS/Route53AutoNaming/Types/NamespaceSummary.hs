{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.NamespaceSummary
  ( NamespaceSummary (..)
  -- * Smart constructor
  , mkNamespaceSummary
  -- * Lenses
  , nsArn
  , nsCreateDate
  , nsDescription
  , nsId
  , nsName
  , nsProperties
  , nsServiceCount
  , nsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.Arn as Types
import qualified Network.AWS.Route53AutoNaming.Types.NamespaceName as Types
import qualified Network.AWS.Route53AutoNaming.Types.NamespaceProperties as Types
import qualified Network.AWS.Route53AutoNaming.Types.NamespaceType as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceDescription as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types

-- | A complex type that contains information about a namespace.
--
-- /See:/ 'mkNamespaceSummary' smart constructor.
data NamespaceSummary = NamespaceSummary'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
  , createDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the namespace was created.
  , description :: Core.Maybe Types.ResourceDescription
    -- ^ A description for the namespace.
  , id :: Core.Maybe Types.ResourceId
    -- ^ The ID of the namespace.
  , name :: Core.Maybe Types.NamespaceName
    -- ^ The name of the namespace. When you create a namespace, AWS Cloud Map automatically creates a Route 53 hosted zone that has the same name as the namespace.
  , properties :: Core.Maybe Types.NamespaceProperties
  , serviceCount :: Core.Maybe Core.Int
    -- ^ The number of services that were created using the namespace.
  , type' :: Core.Maybe Types.NamespaceType
    -- ^ The type of the namespace, either public or private.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'NamespaceSummary' value with any optional fields omitted.
mkNamespaceSummary
    :: NamespaceSummary
mkNamespaceSummary
  = NamespaceSummary'{arn = Core.Nothing, createDate = Core.Nothing,
                      description = Core.Nothing, id = Core.Nothing, name = Core.Nothing,
                      properties = Core.Nothing, serviceCount = Core.Nothing,
                      type' = Core.Nothing}

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsArn :: Lens.Lens' NamespaceSummary (Core.Maybe Types.Arn)
nsArn = Lens.field @"arn"
{-# INLINEABLE nsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time that the namespace was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCreateDate :: Lens.Lens' NamespaceSummary (Core.Maybe Core.NominalDiffTime)
nsCreateDate = Lens.field @"createDate"
{-# INLINEABLE nsCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsDescription :: Lens.Lens' NamespaceSummary (Core.Maybe Types.ResourceDescription)
nsDescription = Lens.field @"description"
{-# INLINEABLE nsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the namespace.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsId :: Lens.Lens' NamespaceSummary (Core.Maybe Types.ResourceId)
nsId = Lens.field @"id"
{-# INLINEABLE nsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the namespace. When you create a namespace, AWS Cloud Map automatically creates a Route 53 hosted zone that has the same name as the namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsName :: Lens.Lens' NamespaceSummary (Core.Maybe Types.NamespaceName)
nsName = Lens.field @"name"
{-# INLINEABLE nsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsProperties :: Lens.Lens' NamespaceSummary (Core.Maybe Types.NamespaceProperties)
nsProperties = Lens.field @"properties"
{-# INLINEABLE nsProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

-- | The number of services that were created using the namespace.
--
-- /Note:/ Consider using 'serviceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsServiceCount :: Lens.Lens' NamespaceSummary (Core.Maybe Core.Int)
nsServiceCount = Lens.field @"serviceCount"
{-# INLINEABLE nsServiceCount #-}
{-# DEPRECATED serviceCount "Use generic-lens or generic-optics with 'serviceCount' instead"  #-}

-- | The type of the namespace, either public or private.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsType :: Lens.Lens' NamespaceSummary (Core.Maybe Types.NamespaceType)
nsType = Lens.field @"type'"
{-# INLINEABLE nsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON NamespaceSummary where
        parseJSON
          = Core.withObject "NamespaceSummary" Core.$
              \ x ->
                NamespaceSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreateDate" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Properties"
                    Core.<*> x Core..:? "ServiceCount"
                    Core.<*> x Core..:? "Type"
