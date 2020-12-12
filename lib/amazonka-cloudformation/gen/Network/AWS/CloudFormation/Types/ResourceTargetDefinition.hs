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
    rtdRequiresRecreation,
    rtdName,
  )
where

import Network.AWS.CloudFormation.Types.RequiresRecreation
import Network.AWS.CloudFormation.Types.ResourceAttribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The field that AWS CloudFormation will change, such as the name of a resource's property, and whether the resource will be recreated.
--
-- /See:/ 'mkResourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { attribute ::
      Lude.Maybe ResourceAttribute,
    requiresRecreation ::
      Lude.Maybe RequiresRecreation,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTargetDefinition' with the minimum fields required to make a request.
--
-- * 'attribute' - Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
-- * 'name' - If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
-- * 'requiresRecreation' - If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
mkResourceTargetDefinition ::
  ResourceTargetDefinition
mkResourceTargetDefinition =
  ResourceTargetDefinition'
    { attribute = Lude.Nothing,
      requiresRecreation = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdAttribute :: Lens.Lens' ResourceTargetDefinition (Lude.Maybe ResourceAttribute)
rtdAttribute = Lens.lens (attribute :: ResourceTargetDefinition -> Lude.Maybe ResourceAttribute) (\s a -> s {attribute = a} :: ResourceTargetDefinition)
{-# DEPRECATED rtdAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'requiresRecreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdRequiresRecreation :: Lens.Lens' ResourceTargetDefinition (Lude.Maybe RequiresRecreation)
rtdRequiresRecreation = Lens.lens (requiresRecreation :: ResourceTargetDefinition -> Lude.Maybe RequiresRecreation) (\s a -> s {requiresRecreation = a} :: ResourceTargetDefinition)
{-# DEPRECATED rtdRequiresRecreation "Use generic-lens or generic-optics with 'requiresRecreation' instead." #-}

-- | If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdName :: Lens.Lens' ResourceTargetDefinition (Lude.Maybe Lude.Text)
rtdName = Lens.lens (name :: ResourceTargetDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResourceTargetDefinition)
{-# DEPRECATED rtdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML ResourceTargetDefinition where
  parseXML x =
    ResourceTargetDefinition'
      Lude.<$> (x Lude..@? "Attribute")
      Lude.<*> (x Lude..@? "RequiresRecreation")
      Lude.<*> (x Lude..@? "Name")
