{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AttributesResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AttributesResource
  ( AttributesResource (..),

    -- * Smart constructor
    mkAttributesResource,

    -- * Lenses
    arAttributeType,
    arApplicationId,
    arAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the type and the names of attributes that were removed from all the endpoints that are associated with an application.
--
-- /See:/ 'mkAttributesResource' smart constructor.
data AttributesResource = AttributesResource'
  { -- | The type of attribute or attributes that were removed from the endpoints. Valid values are:
    --
    --
    --     * endpoint-custom-attributes - Custom attributes that describe endpoints.
    --
    --
    --     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints.
    --
    --
    --     * endpoint-user-attributes - Custom attributes that describe users.
    attributeType :: Core.Text,
    -- | The unique identifier for the application.
    applicationId :: Core.Text,
    -- | An array that specifies the names of the attributes that were removed from the endpoints.
    attributes :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributesResource' value with any optional fields omitted.
mkAttributesResource ::
  -- | 'attributeType'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  AttributesResource
mkAttributesResource attributeType applicationId =
  AttributesResource'
    { attributeType,
      applicationId,
      attributes = Core.Nothing
    }

-- | The type of attribute or attributes that were removed from the endpoints. Valid values are:
--
--
--     * endpoint-custom-attributes - Custom attributes that describe endpoints.
--
--
--     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints.
--
--
--     * endpoint-user-attributes - Custom attributes that describe users.
--
--
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAttributeType :: Lens.Lens' AttributesResource Core.Text
arAttributeType = Lens.field @"attributeType"
{-# DEPRECATED arAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The unique identifier for the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApplicationId :: Lens.Lens' AttributesResource Core.Text
arApplicationId = Lens.field @"applicationId"
{-# DEPRECATED arApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | An array that specifies the names of the attributes that were removed from the endpoints.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAttributes :: Lens.Lens' AttributesResource (Core.Maybe [Core.Text])
arAttributes = Lens.field @"attributes"
{-# DEPRECATED arAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.FromJSON AttributesResource where
  parseJSON =
    Core.withObject "AttributesResource" Core.$
      \x ->
        AttributesResource'
          Core.<$> (x Core..: "AttributeType")
          Core.<*> (x Core..: "ApplicationId")
          Core.<*> (x Core..:? "Attributes")
