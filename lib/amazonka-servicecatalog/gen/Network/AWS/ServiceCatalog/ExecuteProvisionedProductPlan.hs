{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions or modifies a product based on the resource changes for the specified plan.
module Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
  ( -- * Creating a request
    ExecuteProvisionedProductPlan (..),
    mkExecuteProvisionedProductPlan,

    -- ** Request lenses
    epppIdempotencyToken,
    epppPlanId,
    epppAcceptLanguage,

    -- * Destructuring the response
    ExecuteProvisionedProductPlanResponse (..),
    mkExecuteProvisionedProductPlanResponse,

    -- ** Response lenses
    eppprsRecordDetail,
    eppprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkExecuteProvisionedProductPlan' smart constructor.
data ExecuteProvisionedProductPlan = ExecuteProvisionedProductPlan'
  { -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Lude.Text,
    -- | The plan identifier.
    planId :: Lude.Text,
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
    acceptLanguage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteProvisionedProductPlan' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'planId' - The plan identifier.
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
mkExecuteProvisionedProductPlan ::
  -- | 'idempotencyToken'
  Lude.Text ->
  -- | 'planId'
  Lude.Text ->
  ExecuteProvisionedProductPlan
mkExecuteProvisionedProductPlan pIdempotencyToken_ pPlanId_ =
  ExecuteProvisionedProductPlan'
    { idempotencyToken =
        pIdempotencyToken_,
      planId = pPlanId_,
      acceptLanguage = Lude.Nothing
    }

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epppIdempotencyToken :: Lens.Lens' ExecuteProvisionedProductPlan Lude.Text
epppIdempotencyToken = Lens.lens (idempotencyToken :: ExecuteProvisionedProductPlan -> Lude.Text) (\s a -> s {idempotencyToken = a} :: ExecuteProvisionedProductPlan)
{-# DEPRECATED epppIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epppPlanId :: Lens.Lens' ExecuteProvisionedProductPlan Lude.Text
epppPlanId = Lens.lens (planId :: ExecuteProvisionedProductPlan -> Lude.Text) (\s a -> s {planId = a} :: ExecuteProvisionedProductPlan)
{-# DEPRECATED epppPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

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
epppAcceptLanguage :: Lens.Lens' ExecuteProvisionedProductPlan (Lude.Maybe Lude.Text)
epppAcceptLanguage = Lens.lens (acceptLanguage :: ExecuteProvisionedProductPlan -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ExecuteProvisionedProductPlan)
{-# DEPRECATED epppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Lude.AWSRequest ExecuteProvisionedProductPlan where
  type
    Rs ExecuteProvisionedProductPlan =
      ExecuteProvisionedProductPlanResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExecuteProvisionedProductPlanResponse'
            Lude.<$> (x Lude..?> "RecordDetail") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExecuteProvisionedProductPlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ExecuteProvisionedProductPlan" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExecuteProvisionedProductPlan where
  toJSON ExecuteProvisionedProductPlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdempotencyToken" Lude..= idempotencyToken),
            Lude.Just ("PlanId" Lude..= planId),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage
          ]
      )

instance Lude.ToPath ExecuteProvisionedProductPlan where
  toPath = Lude.const "/"

instance Lude.ToQuery ExecuteProvisionedProductPlan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExecuteProvisionedProductPlanResponse' smart constructor.
data ExecuteProvisionedProductPlanResponse = ExecuteProvisionedProductPlanResponse'
  { -- | Information about the result of provisioning the product.
    recordDetail :: Lude.Maybe RecordDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- * 'recordDetail' - Information about the result of provisioning the product.
-- * 'responseStatus' - The response status code.
mkExecuteProvisionedProductPlanResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExecuteProvisionedProductPlanResponse
mkExecuteProvisionedProductPlanResponse pResponseStatus_ =
  ExecuteProvisionedProductPlanResponse'
    { recordDetail =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the result of provisioning the product.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eppprsRecordDetail :: Lens.Lens' ExecuteProvisionedProductPlanResponse (Lude.Maybe RecordDetail)
eppprsRecordDetail = Lens.lens (recordDetail :: ExecuteProvisionedProductPlanResponse -> Lude.Maybe RecordDetail) (\s a -> s {recordDetail = a} :: ExecuteProvisionedProductPlanResponse)
{-# DEPRECATED eppprsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eppprsResponseStatus :: Lens.Lens' ExecuteProvisionedProductPlanResponse Lude.Int
eppprsResponseStatus = Lens.lens (responseStatus :: ExecuteProvisionedProductPlanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExecuteProvisionedProductPlanResponse)
{-# DEPRECATED eppprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
