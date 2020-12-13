{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests updates to the properties of the specified provisioned product.
module Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
  ( -- * Creating a request
    UpdateProvisionedProductProperties (..),
    mkUpdateProvisionedProductProperties,

    -- ** Request lenses
    upppIdempotencyToken,
    upppProvisionedProductProperties,
    upppAcceptLanguage,
    upppProvisionedProductId,

    -- * Destructuring the response
    UpdateProvisionedProductPropertiesResponse (..),
    mkUpdateProvisionedProductPropertiesResponse,

    -- ** Response lenses
    uppprsStatus,
    uppprsProvisionedProductProperties,
    uppprsRecordId,
    uppprsProvisionedProductId,
    uppprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdateProvisionedProductProperties' smart constructor.
data UpdateProvisionedProductProperties = UpdateProvisionedProductProperties'
  { -- | The idempotency token that uniquely identifies the provisioning product update request.
    idempotencyToken :: Lude.Text,
    -- | A map that contains the provisioned product properties to be updated.
    --
    -- The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an administrator to call @UpdateProvisionedProductProperties@ to update the launch role that is associated with a provisioned product. This role is used when an end user calls a provisioning operation such as @UpdateProvisionedProduct@ , @TerminateProvisionedProduct@ , or @ExecuteProvisionedProductServiceAction@ . Only a role ARN is valid. A user ARN is invalid.
    -- The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user that has permission to see, update, terminate, and execute service actions in the provisioned product.
    -- The administrator can change the owner of a provisioned product to another IAM user within the same account. Both end user owners and administrators can see ownership history of the provisioned product using the @ListRecordHistory@ API. The new owner can describe all past records for the provisioned product using the @DescribeRecord@ API. The previous owner can no longer use @DescribeRecord@ , but can still see the product's history from when he was an owner using @ListRecordHistory@ .
    -- If a provisioned product ownership is assigned to an end user, they can see and perform any action through the API or Service Catalog console such as update, terminate, and execute service actions. If an end user provisions a product and the owner is updated to someone else, they will no longer be able to see or perform any actions through API or the Service Catalog console on that provisioned product.
    provisionedProductProperties :: Lude.HashMap PropertyKey (Lude.Text),
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The identifier of the provisioned product.
    provisionedProductId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisionedProductProperties' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - The idempotency token that uniquely identifies the provisioning product update request.
-- * 'provisionedProductProperties' - A map that contains the provisioned product properties to be updated.
--
-- The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an administrator to call @UpdateProvisionedProductProperties@ to update the launch role that is associated with a provisioned product. This role is used when an end user calls a provisioning operation such as @UpdateProvisionedProduct@ , @TerminateProvisionedProduct@ , or @ExecuteProvisionedProductServiceAction@ . Only a role ARN is valid. A user ARN is invalid.
-- The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user that has permission to see, update, terminate, and execute service actions in the provisioned product.
-- The administrator can change the owner of a provisioned product to another IAM user within the same account. Both end user owners and administrators can see ownership history of the provisioned product using the @ListRecordHistory@ API. The new owner can describe all past records for the provisioned product using the @DescribeRecord@ API. The previous owner can no longer use @DescribeRecord@ , but can still see the product's history from when he was an owner using @ListRecordHistory@ .
-- If a provisioned product ownership is assigned to an end user, they can see and perform any action through the API or Service Catalog console such as update, terminate, and execute service actions. If an end user provisions a product and the owner is updated to someone else, they will no longer be able to see or perform any actions through API or the Service Catalog console on that provisioned product.
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'provisionedProductId' - The identifier of the provisioned product.
mkUpdateProvisionedProductProperties ::
  -- | 'idempotencyToken'
  Lude.Text ->
  -- | 'provisionedProductId'
  Lude.Text ->
  UpdateProvisionedProductProperties
mkUpdateProvisionedProductProperties
  pIdempotencyToken_
  pProvisionedProductId_ =
    UpdateProvisionedProductProperties'
      { idempotencyToken =
          pIdempotencyToken_,
        provisionedProductProperties = Lude.mempty,
        acceptLanguage = Lude.Nothing,
        provisionedProductId = pProvisionedProductId_
      }

-- | The idempotency token that uniquely identifies the provisioning product update request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upppIdempotencyToken :: Lens.Lens' UpdateProvisionedProductProperties Lude.Text
upppIdempotencyToken = Lens.lens (idempotencyToken :: UpdateProvisionedProductProperties -> Lude.Text) (\s a -> s {idempotencyToken = a} :: UpdateProvisionedProductProperties)
{-# DEPRECATED upppIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | A map that contains the provisioned product properties to be updated.
--
-- The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an administrator to call @UpdateProvisionedProductProperties@ to update the launch role that is associated with a provisioned product. This role is used when an end user calls a provisioning operation such as @UpdateProvisionedProduct@ , @TerminateProvisionedProduct@ , or @ExecuteProvisionedProductServiceAction@ . Only a role ARN is valid. A user ARN is invalid.
-- The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user that has permission to see, update, terminate, and execute service actions in the provisioned product.
-- The administrator can change the owner of a provisioned product to another IAM user within the same account. Both end user owners and administrators can see ownership history of the provisioned product using the @ListRecordHistory@ API. The new owner can describe all past records for the provisioned product using the @DescribeRecord@ API. The previous owner can no longer use @DescribeRecord@ , but can still see the product's history from when he was an owner using @ListRecordHistory@ .
-- If a provisioned product ownership is assigned to an end user, they can see and perform any action through the API or Service Catalog console such as update, terminate, and execute service actions. If an end user provisions a product and the owner is updated to someone else, they will no longer be able to see or perform any actions through API or the Service Catalog console on that provisioned product.
--
-- /Note:/ Consider using 'provisionedProductProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upppProvisionedProductProperties :: Lens.Lens' UpdateProvisionedProductProperties (Lude.HashMap PropertyKey (Lude.Text))
upppProvisionedProductProperties = Lens.lens (provisionedProductProperties :: UpdateProvisionedProductProperties -> Lude.HashMap PropertyKey (Lude.Text)) (\s a -> s {provisionedProductProperties = a} :: UpdateProvisionedProductProperties)
{-# DEPRECATED upppProvisionedProductProperties "Use generic-lens or generic-optics with 'provisionedProductProperties' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upppAcceptLanguage :: Lens.Lens' UpdateProvisionedProductProperties (Lude.Maybe Lude.Text)
upppAcceptLanguage = Lens.lens (acceptLanguage :: UpdateProvisionedProductProperties -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: UpdateProvisionedProductProperties)
{-# DEPRECATED upppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upppProvisionedProductId :: Lens.Lens' UpdateProvisionedProductProperties Lude.Text
upppProvisionedProductId = Lens.lens (provisionedProductId :: UpdateProvisionedProductProperties -> Lude.Text) (\s a -> s {provisionedProductId = a} :: UpdateProvisionedProductProperties)
{-# DEPRECATED upppProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

instance Lude.AWSRequest UpdateProvisionedProductProperties where
  type
    Rs UpdateProvisionedProductProperties =
      UpdateProvisionedProductPropertiesResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateProvisionedProductPropertiesResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "ProvisionedProductProperties" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "RecordId")
            Lude.<*> (x Lude..?> "ProvisionedProductId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProvisionedProductProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.UpdateProvisionedProductProperties" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProvisionedProductProperties where
  toJSON UpdateProvisionedProductProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdempotencyToken" Lude..= idempotencyToken),
            Lude.Just
              ( "ProvisionedProductProperties"
                  Lude..= provisionedProductProperties
              ),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("ProvisionedProductId" Lude..= provisionedProductId)
          ]
      )

instance Lude.ToPath UpdateProvisionedProductProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProvisionedProductProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProvisionedProductPropertiesResponse' smart constructor.
data UpdateProvisionedProductPropertiesResponse = UpdateProvisionedProductPropertiesResponse'
  { -- | The status of the request.
    status :: Lude.Maybe RecordStatus,
    -- | A map that contains the properties updated.
    provisionedProductProperties :: Lude.Maybe (Lude.HashMap PropertyKey (Lude.Text)),
    -- | The identifier of the record.
    recordId :: Lude.Maybe Lude.Text,
    -- | The provisioned product identifier.
    provisionedProductId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisionedProductPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the request.
-- * 'provisionedProductProperties' - A map that contains the properties updated.
-- * 'recordId' - The identifier of the record.
-- * 'provisionedProductId' - The provisioned product identifier.
-- * 'responseStatus' - The response status code.
mkUpdateProvisionedProductPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProvisionedProductPropertiesResponse
mkUpdateProvisionedProductPropertiesResponse pResponseStatus_ =
  UpdateProvisionedProductPropertiesResponse'
    { status =
        Lude.Nothing,
      provisionedProductProperties = Lude.Nothing,
      recordId = Lude.Nothing,
      provisionedProductId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppprsStatus :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Lude.Maybe RecordStatus)
uppprsStatus = Lens.lens (status :: UpdateProvisionedProductPropertiesResponse -> Lude.Maybe RecordStatus) (\s a -> s {status = a} :: UpdateProvisionedProductPropertiesResponse)
{-# DEPRECATED uppprsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A map that contains the properties updated.
--
-- /Note:/ Consider using 'provisionedProductProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppprsProvisionedProductProperties :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Lude.Maybe (Lude.HashMap PropertyKey (Lude.Text)))
uppprsProvisionedProductProperties = Lens.lens (provisionedProductProperties :: UpdateProvisionedProductPropertiesResponse -> Lude.Maybe (Lude.HashMap PropertyKey (Lude.Text))) (\s a -> s {provisionedProductProperties = a} :: UpdateProvisionedProductPropertiesResponse)
{-# DEPRECATED uppprsProvisionedProductProperties "Use generic-lens or generic-optics with 'provisionedProductProperties' instead." #-}

-- | The identifier of the record.
--
-- /Note:/ Consider using 'recordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppprsRecordId :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Lude.Maybe Lude.Text)
uppprsRecordId = Lens.lens (recordId :: UpdateProvisionedProductPropertiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {recordId = a} :: UpdateProvisionedProductPropertiesResponse)
{-# DEPRECATED uppprsRecordId "Use generic-lens or generic-optics with 'recordId' instead." #-}

-- | The provisioned product identifier.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppprsProvisionedProductId :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Lude.Maybe Lude.Text)
uppprsProvisionedProductId = Lens.lens (provisionedProductId :: UpdateProvisionedProductPropertiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductId = a} :: UpdateProvisionedProductPropertiesResponse)
{-# DEPRECATED uppprsProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppprsResponseStatus :: Lens.Lens' UpdateProvisionedProductPropertiesResponse Lude.Int
uppprsResponseStatus = Lens.lens (responseStatus :: UpdateProvisionedProductPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProvisionedProductPropertiesResponse)
{-# DEPRECATED uppprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
