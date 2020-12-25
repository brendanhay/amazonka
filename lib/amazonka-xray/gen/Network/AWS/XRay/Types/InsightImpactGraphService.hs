{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightImpactGraphService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightImpactGraphService
  ( InsightImpactGraphService (..),

    -- * Smart constructor
    mkInsightImpactGraphService,

    -- * Lenses
    iigsAccountId,
    iigsEdges,
    iigsName,
    iigsNames,
    iigsReferenceId,
    iigsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.InsightImpactGraphEdge as Types
import qualified Network.AWS.XRay.Types.String as Types

-- | Information about an application that processed requests, users that made requests, or downstream services, resources, and applications that an application used.
--
-- /See:/ 'mkInsightImpactGraphService' smart constructor.
data InsightImpactGraphService = InsightImpactGraphService'
  { -- | Identifier of the AWS account in which the service runs.
    accountId :: Core.Maybe Types.String,
    -- | Connections to downstream services.
    edges :: Core.Maybe [Types.InsightImpactGraphEdge],
    -- | The canonical name of the service.
    name :: Core.Maybe Types.String,
    -- | A list of names for the service, including the canonical name.
    names :: Core.Maybe [Types.String],
    -- | Identifier for the service. Unique within the service map.
    referenceId :: Core.Maybe Core.Int,
    -- | Identifier for the service. Unique within the service map.
    --
    --
    --     * AWS Resource - The type of an AWS resource. For example, AWS::EC2::Instance for an application running on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table that the application used.
    --
    --
    --     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
    --
    --
    --     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
    --
    --
    --     * remote - A downstream service of indeterminate type.
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InsightImpactGraphService' value with any optional fields omitted.
mkInsightImpactGraphService ::
  InsightImpactGraphService
mkInsightImpactGraphService =
  InsightImpactGraphService'
    { accountId = Core.Nothing,
      edges = Core.Nothing,
      name = Core.Nothing,
      names = Core.Nothing,
      referenceId = Core.Nothing,
      type' = Core.Nothing
    }

-- | Identifier of the AWS account in which the service runs.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsAccountId :: Lens.Lens' InsightImpactGraphService (Core.Maybe Types.String)
iigsAccountId = Lens.field @"accountId"
{-# DEPRECATED iigsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Connections to downstream services.
--
-- /Note:/ Consider using 'edges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsEdges :: Lens.Lens' InsightImpactGraphService (Core.Maybe [Types.InsightImpactGraphEdge])
iigsEdges = Lens.field @"edges"
{-# DEPRECATED iigsEdges "Use generic-lens or generic-optics with 'edges' instead." #-}

-- | The canonical name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsName :: Lens.Lens' InsightImpactGraphService (Core.Maybe Types.String)
iigsName = Lens.field @"name"
{-# DEPRECATED iigsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of names for the service, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsNames :: Lens.Lens' InsightImpactGraphService (Core.Maybe [Types.String])
iigsNames = Lens.field @"names"
{-# DEPRECATED iigsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | Identifier for the service. Unique within the service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsReferenceId :: Lens.Lens' InsightImpactGraphService (Core.Maybe Core.Int)
iigsReferenceId = Lens.field @"referenceId"
{-# DEPRECATED iigsReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | Identifier for the service. Unique within the service map.
--
--
--     * AWS Resource - The type of an AWS resource. For example, AWS::EC2::Instance for an application running on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table that the application used.
--
--
--     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * remote - A downstream service of indeterminate type.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsType :: Lens.Lens' InsightImpactGraphService (Core.Maybe Types.String)
iigsType = Lens.field @"type'"
{-# DEPRECATED iigsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON InsightImpactGraphService where
  parseJSON =
    Core.withObject "InsightImpactGraphService" Core.$
      \x ->
        InsightImpactGraphService'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "Edges")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Names")
          Core.<*> (x Core..:? "ReferenceId")
          Core.<*> (x Core..:? "Type")
