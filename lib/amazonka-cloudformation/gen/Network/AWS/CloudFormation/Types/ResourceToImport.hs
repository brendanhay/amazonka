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
    rtiLogicalResourceId,
    rtiResourceType,
    rtiResourceIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the target resource of an import operation.
--
-- /See:/ 'mkResourceToImport' smart constructor.
data ResourceToImport = ResourceToImport'
  { -- | The logical ID of the target resource as specified in the template.
    logicalResourceId :: Lude.Text,
    -- | The type of resource to import into your stack, such as @AWS::S3::Bucket@ . For a list of supported resource types, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations> in the AWS CloudFormation User Guide.
    resourceType :: Lude.Text,
    -- | A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
    resourceIdentifier :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceToImport' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical ID of the target resource as specified in the template.
-- * 'resourceType' - The type of resource to import into your stack, such as @AWS::S3::Bucket@ . For a list of supported resource types, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations> in the AWS CloudFormation User Guide.
-- * 'resourceIdentifier' - A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
mkResourceToImport ::
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  ResourceToImport
mkResourceToImport pLogicalResourceId_ pResourceType_ =
  ResourceToImport'
    { logicalResourceId = pLogicalResourceId_,
      resourceType = pResourceType_,
      resourceIdentifier = Lude.mempty
    }

-- | The logical ID of the target resource as specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiLogicalResourceId :: Lens.Lens' ResourceToImport Lude.Text
rtiLogicalResourceId = Lens.lens (logicalResourceId :: ResourceToImport -> Lude.Text) (\s a -> s {logicalResourceId = a} :: ResourceToImport)
{-# DEPRECATED rtiLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The type of resource to import into your stack, such as @AWS::S3::Bucket@ . For a list of supported resource types, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiResourceType :: Lens.Lens' ResourceToImport Lude.Text
rtiResourceType = Lens.lens (resourceType :: ResourceToImport -> Lude.Text) (\s a -> s {resourceType = a} :: ResourceToImport)
{-# DEPRECATED rtiResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiResourceIdentifier :: Lens.Lens' ResourceToImport (Lude.HashMap Lude.Text (Lude.Text))
rtiResourceIdentifier = Lens.lens (resourceIdentifier :: ResourceToImport -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {resourceIdentifier = a} :: ResourceToImport)
{-# DEPRECATED rtiResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.ToQuery ResourceToImport where
  toQuery ResourceToImport' {..} =
    Lude.mconcat
      [ "LogicalResourceId" Lude.=: logicalResourceId,
        "ResourceType" Lude.=: resourceType,
        "ResourceIdentifier"
          Lude.=: Lude.toQueryMap "entry" "key" "value" resourceIdentifier
      ]
