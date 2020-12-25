{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceToImport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceToImport
  ( ResourceToImport (..),

    -- * Smart constructor
    mkResourceToImport,

    -- * Lenses
    rtiResourceType,
    rtiLogicalResourceId,
    rtiResourceIdentifier,
  )
where

import qualified Network.AWS.CloudFormation.Types.LogicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ResourceIdentifierPropertyKey as Types
import qualified Network.AWS.CloudFormation.Types.ResourceIdentifierPropertyValue as Types
import qualified Network.AWS.CloudFormation.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the target resource of an import operation.
--
-- /See:/ 'mkResourceToImport' smart constructor.
data ResourceToImport = ResourceToImport'
  { -- | The type of resource to import into your stack, such as @AWS::S3::Bucket@ . For a list of supported resource types, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations> in the AWS CloudFormation User Guide.
    resourceType :: Types.ResourceType,
    -- | The logical ID of the target resource as specified in the template.
    logicalResourceId :: Types.LogicalResourceId,
    -- | A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
    resourceIdentifier :: Core.HashMap Types.ResourceIdentifierPropertyKey Types.ResourceIdentifierPropertyValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceToImport' value with any optional fields omitted.
mkResourceToImport ::
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'logicalResourceId'
  Types.LogicalResourceId ->
  ResourceToImport
mkResourceToImport resourceType logicalResourceId =
  ResourceToImport'
    { resourceType,
      logicalResourceId,
      resourceIdentifier = Core.mempty
    }

-- | The type of resource to import into your stack, such as @AWS::S3::Bucket@ . For a list of supported resource types, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiResourceType :: Lens.Lens' ResourceToImport Types.ResourceType
rtiResourceType = Lens.field @"resourceType"
{-# DEPRECATED rtiResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The logical ID of the target resource as specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiLogicalResourceId :: Lens.Lens' ResourceToImport Types.LogicalResourceId
rtiLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED rtiLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiResourceIdentifier :: Lens.Lens' ResourceToImport (Core.HashMap Types.ResourceIdentifierPropertyKey Types.ResourceIdentifierPropertyValue)
rtiResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED rtiResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}
