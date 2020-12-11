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
    risResourceType,
    risLogicalResourceIds,
    risResourceIdentifiers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the target resources of a specific type in your import template (for example, all @AWS::S3::Bucket@ resources) and the properties you can provide during the import to identify resources of that type.
--
-- /See:/ 'mkResourceIdentifierSummary' smart constructor.
data ResourceIdentifierSummary = ResourceIdentifierSummary'
  { resourceType ::
      Lude.Maybe Lude.Text,
    logicalResourceIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    resourceIdentifiers ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceIdentifierSummary' with the minimum fields required to make a request.
--
-- * 'logicalResourceIds' - The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
-- * 'resourceIdentifiers' - The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
-- * 'resourceType' - The template resource type of the target resources, such as @AWS::S3::Bucket@ .
mkResourceIdentifierSummary ::
  ResourceIdentifierSummary
mkResourceIdentifierSummary =
  ResourceIdentifierSummary'
    { resourceType = Lude.Nothing,
      logicalResourceIds = Lude.Nothing,
      resourceIdentifiers = Lude.Nothing
    }

-- | The template resource type of the target resources, such as @AWS::S3::Bucket@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risResourceType :: Lens.Lens' ResourceIdentifierSummary (Lude.Maybe Lude.Text)
risResourceType = Lens.lens (resourceType :: ResourceIdentifierSummary -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ResourceIdentifierSummary)
{-# DEPRECATED risResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
--
-- /Note:/ Consider using 'logicalResourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risLogicalResourceIds :: Lens.Lens' ResourceIdentifierSummary (Lude.Maybe (Lude.NonEmpty Lude.Text))
risLogicalResourceIds = Lens.lens (logicalResourceIds :: ResourceIdentifierSummary -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {logicalResourceIds = a} :: ResourceIdentifierSummary)
{-# DEPRECATED risLogicalResourceIds "Use generic-lens or generic-optics with 'logicalResourceIds' instead." #-}

-- | The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risResourceIdentifiers :: Lens.Lens' ResourceIdentifierSummary (Lude.Maybe [Lude.Text])
risResourceIdentifiers = Lens.lens (resourceIdentifiers :: ResourceIdentifierSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceIdentifiers = a} :: ResourceIdentifierSummary)
{-# DEPRECATED risResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

instance Lude.FromXML ResourceIdentifierSummary where
  parseXML x =
    ResourceIdentifierSummary'
      Lude.<$> (x Lude..@? "ResourceType")
      Lude.<*> ( x Lude..@? "LogicalResourceIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLNonEmpty "member")
               )
      Lude.<*> ( x Lude..@? "ResourceIdentifiers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
