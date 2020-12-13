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
import qualified Network.AWS.Prelude as Lude

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
    attributeType :: Lude.Text,
    -- | The unique identifier for the application.
    applicationId :: Lude.Text,
    -- | An array that specifies the names of the attributes that were removed from the endpoints.
    attributes :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributesResource' with the minimum fields required to make a request.
--
-- * 'attributeType' - The type of attribute or attributes that were removed from the endpoints. Valid values are:
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
-- * 'applicationId' - The unique identifier for the application.
-- * 'attributes' - An array that specifies the names of the attributes that were removed from the endpoints.
mkAttributesResource ::
  -- | 'attributeType'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  AttributesResource
mkAttributesResource pAttributeType_ pApplicationId_ =
  AttributesResource'
    { attributeType = pAttributeType_,
      applicationId = pApplicationId_,
      attributes = Lude.Nothing
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
arAttributeType :: Lens.Lens' AttributesResource Lude.Text
arAttributeType = Lens.lens (attributeType :: AttributesResource -> Lude.Text) (\s a -> s {attributeType = a} :: AttributesResource)
{-# DEPRECATED arAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The unique identifier for the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApplicationId :: Lens.Lens' AttributesResource Lude.Text
arApplicationId = Lens.lens (applicationId :: AttributesResource -> Lude.Text) (\s a -> s {applicationId = a} :: AttributesResource)
{-# DEPRECATED arApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | An array that specifies the names of the attributes that were removed from the endpoints.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAttributes :: Lens.Lens' AttributesResource (Lude.Maybe [Lude.Text])
arAttributes = Lens.lens (attributes :: AttributesResource -> Lude.Maybe [Lude.Text]) (\s a -> s {attributes = a} :: AttributesResource)
{-# DEPRECATED arAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON AttributesResource where
  parseJSON =
    Lude.withObject
      "AttributesResource"
      ( \x ->
          AttributesResource'
            Lude.<$> (x Lude..: "AttributeType")
            Lude.<*> (x Lude..: "ApplicationId")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )
