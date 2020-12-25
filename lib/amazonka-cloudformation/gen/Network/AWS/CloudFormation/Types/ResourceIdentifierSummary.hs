{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceIdentifierSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceIdentifierSummary
  ( ResourceIdentifierSummary (..),

    -- * Smart constructor
    mkResourceIdentifierSummary,

    -- * Lenses
    risLogicalResourceIds,
    risResourceIdentifiers,
    risResourceType,
  )
where

import qualified Network.AWS.CloudFormation.Types.LogicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ResourceIdentifierPropertyKey as Types
import qualified Network.AWS.CloudFormation.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the target resources of a specific type in your import template (for example, all @AWS::S3::Bucket@ resources) and the properties you can provide during the import to identify resources of that type.
--
-- /See:/ 'mkResourceIdentifierSummary' smart constructor.
data ResourceIdentifierSummary = ResourceIdentifierSummary'
  { -- | The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
    logicalResourceIds :: Core.Maybe (Core.NonEmpty Types.LogicalResourceId),
    -- | The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
    resourceIdentifiers :: Core.Maybe [Types.ResourceIdentifierPropertyKey],
    -- | The template resource type of the target resources, such as @AWS::S3::Bucket@ .
    resourceType :: Core.Maybe Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceIdentifierSummary' value with any optional fields omitted.
mkResourceIdentifierSummary ::
  ResourceIdentifierSummary
mkResourceIdentifierSummary =
  ResourceIdentifierSummary'
    { logicalResourceIds = Core.Nothing,
      resourceIdentifiers = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
--
-- /Note:/ Consider using 'logicalResourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risLogicalResourceIds :: Lens.Lens' ResourceIdentifierSummary (Core.Maybe (Core.NonEmpty Types.LogicalResourceId))
risLogicalResourceIds = Lens.field @"logicalResourceIds"
{-# DEPRECATED risLogicalResourceIds "Use generic-lens or generic-optics with 'logicalResourceIds' instead." #-}

-- | The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risResourceIdentifiers :: Lens.Lens' ResourceIdentifierSummary (Core.Maybe [Types.ResourceIdentifierPropertyKey])
risResourceIdentifiers = Lens.field @"resourceIdentifiers"
{-# DEPRECATED risResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The template resource type of the target resources, such as @AWS::S3::Bucket@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risResourceType :: Lens.Lens' ResourceIdentifierSummary (Core.Maybe Types.ResourceType)
risResourceType = Lens.field @"resourceType"
{-# DEPRECATED risResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromXML ResourceIdentifierSummary where
  parseXML x =
    ResourceIdentifierSummary'
      Core.<$> ( x Core..@? "LogicalResourceIds"
                   Core..<@> Core.parseXMLNonEmpty "member"
               )
      Core.<*> ( x Core..@? "ResourceIdentifiers"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "ResourceType")
