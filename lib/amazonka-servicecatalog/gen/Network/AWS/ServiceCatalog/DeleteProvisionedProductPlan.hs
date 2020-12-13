{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified plan.
module Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
  ( -- * Creating a request
    DeleteProvisionedProductPlan (..),
    mkDeleteProvisionedProductPlan,

    -- ** Request lenses
    dpppPlanId,
    dpppAcceptLanguage,
    dpppIgnoreErrors,

    -- * Destructuring the response
    DeleteProvisionedProductPlanResponse (..),
    mkDeleteProvisionedProductPlanResponse,

    -- ** Response lenses
    dppprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDeleteProvisionedProductPlan' smart constructor.
data DeleteProvisionedProductPlan = DeleteProvisionedProductPlan'
  { -- | The plan identifier.
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
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
    ignoreErrors :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisionedProductPlan' with the minimum fields required to make a request.
--
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
--
--
-- * 'ignoreErrors' - If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
mkDeleteProvisionedProductPlan ::
  -- | 'planId'
  Lude.Text ->
  DeleteProvisionedProductPlan
mkDeleteProvisionedProductPlan pPlanId_ =
  DeleteProvisionedProductPlan'
    { planId = pPlanId_,
      acceptLanguage = Lude.Nothing,
      ignoreErrors = Lude.Nothing
    }

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppPlanId :: Lens.Lens' DeleteProvisionedProductPlan Lude.Text
dpppPlanId = Lens.lens (planId :: DeleteProvisionedProductPlan -> Lude.Text) (\s a -> s {planId = a} :: DeleteProvisionedProductPlan)
{-# DEPRECATED dpppPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

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
dpppAcceptLanguage :: Lens.Lens' DeleteProvisionedProductPlan (Lude.Maybe Lude.Text)
dpppAcceptLanguage = Lens.lens (acceptLanguage :: DeleteProvisionedProductPlan -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DeleteProvisionedProductPlan)
{-# DEPRECATED dpppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
--
-- /Note:/ Consider using 'ignoreErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppIgnoreErrors :: Lens.Lens' DeleteProvisionedProductPlan (Lude.Maybe Lude.Bool)
dpppIgnoreErrors = Lens.lens (ignoreErrors :: DeleteProvisionedProductPlan -> Lude.Maybe Lude.Bool) (\s a -> s {ignoreErrors = a} :: DeleteProvisionedProductPlan)
{-# DEPRECATED dpppIgnoreErrors "Use generic-lens or generic-optics with 'ignoreErrors' instead." #-}

instance Lude.AWSRequest DeleteProvisionedProductPlan where
  type
    Rs DeleteProvisionedProductPlan =
      DeleteProvisionedProductPlanResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProvisionedProductPlanResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProvisionedProductPlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DeleteProvisionedProductPlan" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProvisionedProductPlan where
  toJSON DeleteProvisionedProductPlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PlanId" Lude..= planId),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("IgnoreErrors" Lude..=) Lude.<$> ignoreErrors
          ]
      )

instance Lude.ToPath DeleteProvisionedProductPlan where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProvisionedProductPlan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProvisionedProductPlanResponse' smart constructor.
newtype DeleteProvisionedProductPlanResponse = DeleteProvisionedProductPlanResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProvisionedProductPlanResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProvisionedProductPlanResponse
mkDeleteProvisionedProductPlanResponse pResponseStatus_ =
  DeleteProvisionedProductPlanResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprsResponseStatus :: Lens.Lens' DeleteProvisionedProductPlanResponse Lude.Int
dppprsResponseStatus = Lens.lens (responseStatus :: DeleteProvisionedProductPlanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProvisionedProductPlanResponse)
{-# DEPRECATED dppprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
