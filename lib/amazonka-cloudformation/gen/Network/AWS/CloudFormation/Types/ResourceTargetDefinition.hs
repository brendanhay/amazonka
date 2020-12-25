{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceTargetDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceTargetDefinition
  ( ResourceTargetDefinition (..),

    -- * Smart constructor
    mkResourceTargetDefinition,

    -- * Lenses
    rtdAttribute,
    rtdName,
    rtdRequiresRecreation,
  )
where

import qualified Network.AWS.CloudFormation.Types.PropertyName as Types
import qualified Network.AWS.CloudFormation.Types.RequiresRecreation as Types
import qualified Network.AWS.CloudFormation.Types.ResourceAttribute as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The field that AWS CloudFormation will change, such as the name of a resource's property, and whether the resource will be recreated.
--
-- /See:/ 'mkResourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { -- | Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
    attribute :: Core.Maybe Types.ResourceAttribute,
    -- | If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
    name :: Core.Maybe Types.PropertyName,
    -- | If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
    requiresRecreation :: Core.Maybe Types.RequiresRecreation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTargetDefinition' value with any optional fields omitted.
mkResourceTargetDefinition ::
  ResourceTargetDefinition
mkResourceTargetDefinition =
  ResourceTargetDefinition'
    { attribute = Core.Nothing,
      name = Core.Nothing,
      requiresRecreation = Core.Nothing
    }

-- | Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdAttribute :: Lens.Lens' ResourceTargetDefinition (Core.Maybe Types.ResourceAttribute)
rtdAttribute = Lens.field @"attribute"
{-# DEPRECATED rtdAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdName :: Lens.Lens' ResourceTargetDefinition (Core.Maybe Types.PropertyName)
rtdName = Lens.field @"name"
{-# DEPRECATED rtdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'requiresRecreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdRequiresRecreation :: Lens.Lens' ResourceTargetDefinition (Core.Maybe Types.RequiresRecreation)
rtdRequiresRecreation = Lens.field @"requiresRecreation"
{-# DEPRECATED rtdRequiresRecreation "Use generic-lens or generic-optics with 'requiresRecreation' instead." #-}

instance Core.FromXML ResourceTargetDefinition where
  parseXML x =
    ResourceTargetDefinition'
      Core.<$> (x Core..@? "Attribute")
      Core.<*> (x Core..@? "Name")
      Core.<*> (x Core..@? "RequiresRecreation")
