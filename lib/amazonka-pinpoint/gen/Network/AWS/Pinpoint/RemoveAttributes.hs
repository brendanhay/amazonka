{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.RemoveAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more attributes, of the same attribute type, from all the endpoints that are associated with an application.
module Network.AWS.Pinpoint.RemoveAttributes
  ( -- * Creating a request
    RemoveAttributes (..),
    mkRemoveAttributes,

    -- ** Request lenses
    raAttributeType,
    raApplicationId,
    raUpdateAttributesRequest,

    -- * Destructuring the response
    RemoveAttributesResponse (..),
    mkRemoveAttributesResponse,

    -- ** Response lenses
    rarsResponseStatus,
    rarsAttributesResource,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveAttributes' smart constructor.
data RemoveAttributes = RemoveAttributes'
  { attributeType ::
      Lude.Text,
    applicationId :: Lude.Text,
    updateAttributesRequest :: UpdateAttributesRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAttributes' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'attributeType' - The type of attribute or attributes to remove. Valid values are:
--
--
--     * endpoint-custom-attributes - Custom attributes that describe endpoints, such as the date when an associated user opted in or out of receiving communications from you through a specific type of channel.
--
--
--     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints, such as the number of app sessions or the number of items left in a cart.
--
--
--     * endpoint-user-attributes - Custom attributes that describe users, such as first name, last name, and age.
--
--
-- * 'updateAttributesRequest' - Undocumented field.
mkRemoveAttributes ::
  -- | 'attributeType'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'updateAttributesRequest'
  UpdateAttributesRequest ->
  RemoveAttributes
mkRemoveAttributes
  pAttributeType_
  pApplicationId_
  pUpdateAttributesRequest_ =
    RemoveAttributes'
      { attributeType = pAttributeType_,
        applicationId = pApplicationId_,
        updateAttributesRequest = pUpdateAttributesRequest_
      }

-- | The type of attribute or attributes to remove. Valid values are:
--
--
--     * endpoint-custom-attributes - Custom attributes that describe endpoints, such as the date when an associated user opted in or out of receiving communications from you through a specific type of channel.
--
--
--     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints, such as the number of app sessions or the number of items left in a cart.
--
--
--     * endpoint-user-attributes - Custom attributes that describe users, such as first name, last name, and age.
--
--
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAttributeType :: Lens.Lens' RemoveAttributes Lude.Text
raAttributeType = Lens.lens (attributeType :: RemoveAttributes -> Lude.Text) (\s a -> s {attributeType = a} :: RemoveAttributes)
{-# DEPRECATED raAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raApplicationId :: Lens.Lens' RemoveAttributes Lude.Text
raApplicationId = Lens.lens (applicationId :: RemoveAttributes -> Lude.Text) (\s a -> s {applicationId = a} :: RemoveAttributes)
{-# DEPRECATED raApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateAttributesRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUpdateAttributesRequest :: Lens.Lens' RemoveAttributes UpdateAttributesRequest
raUpdateAttributesRequest = Lens.lens (updateAttributesRequest :: RemoveAttributes -> UpdateAttributesRequest) (\s a -> s {updateAttributesRequest = a} :: RemoveAttributes)
{-# DEPRECATED raUpdateAttributesRequest "Use generic-lens or generic-optics with 'updateAttributesRequest' instead." #-}

instance Lude.AWSRequest RemoveAttributes where
  type Rs RemoveAttributes = RemoveAttributesResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          RemoveAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders RemoveAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveAttributes where
  toJSON RemoveAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("UpdateAttributesRequest" Lude..= updateAttributesRequest)
          ]
      )

instance Lude.ToPath RemoveAttributes where
  toPath RemoveAttributes' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/attributes/",
        Lude.toBS attributeType
      ]

instance Lude.ToQuery RemoveAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveAttributesResponse' smart constructor.
data RemoveAttributesResponse = RemoveAttributesResponse'
  { responseStatus ::
      Lude.Int,
    attributesResource :: AttributesResource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributesResource' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRemoveAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'attributesResource'
  AttributesResource ->
  RemoveAttributesResponse
mkRemoveAttributesResponse pResponseStatus_ pAttributesResource_ =
  RemoveAttributesResponse'
    { responseStatus = pResponseStatus_,
      attributesResource = pAttributesResource_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarsResponseStatus :: Lens.Lens' RemoveAttributesResponse Lude.Int
rarsResponseStatus = Lens.lens (responseStatus :: RemoveAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveAttributesResponse)
{-# DEPRECATED rarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'attributesResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarsAttributesResource :: Lens.Lens' RemoveAttributesResponse AttributesResource
rarsAttributesResource = Lens.lens (attributesResource :: RemoveAttributesResponse -> AttributesResource) (\s a -> s {attributesResource = a} :: RemoveAttributesResponse)
{-# DEPRECATED rarsAttributesResource "Use generic-lens or generic-optics with 'attributesResource' instead." #-}
