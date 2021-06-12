{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified plan.
module Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
  ( -- * Creating a Request
    DeleteProvisionedProductPlan (..),
    newDeleteProvisionedProductPlan,

    -- * Request Lenses
    deleteProvisionedProductPlan_ignoreErrors,
    deleteProvisionedProductPlan_acceptLanguage,
    deleteProvisionedProductPlan_planId,

    -- * Destructuring the Response
    DeleteProvisionedProductPlanResponse (..),
    newDeleteProvisionedProductPlanResponse,

    -- * Response Lenses
    deleteProvisionedProductPlanResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDeleteProvisionedProductPlan' smart constructor.
data DeleteProvisionedProductPlan = DeleteProvisionedProductPlan'
  { -- | If set to true, AWS Service Catalog stops managing the specified
    -- provisioned product even if it cannot delete the underlying resources.
    ignoreErrors :: Core.Maybe Core.Bool,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The plan identifier.
    planId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProvisionedProductPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignoreErrors', 'deleteProvisionedProductPlan_ignoreErrors' - If set to true, AWS Service Catalog stops managing the specified
-- provisioned product even if it cannot delete the underlying resources.
--
-- 'acceptLanguage', 'deleteProvisionedProductPlan_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'planId', 'deleteProvisionedProductPlan_planId' - The plan identifier.
newDeleteProvisionedProductPlan ::
  -- | 'planId'
  Core.Text ->
  DeleteProvisionedProductPlan
newDeleteProvisionedProductPlan pPlanId_ =
  DeleteProvisionedProductPlan'
    { ignoreErrors =
        Core.Nothing,
      acceptLanguage = Core.Nothing,
      planId = pPlanId_
    }

-- | If set to true, AWS Service Catalog stops managing the specified
-- provisioned product even if it cannot delete the underlying resources.
deleteProvisionedProductPlan_ignoreErrors :: Lens.Lens' DeleteProvisionedProductPlan (Core.Maybe Core.Bool)
deleteProvisionedProductPlan_ignoreErrors = Lens.lens (\DeleteProvisionedProductPlan' {ignoreErrors} -> ignoreErrors) (\s@DeleteProvisionedProductPlan' {} a -> s {ignoreErrors = a} :: DeleteProvisionedProductPlan)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteProvisionedProductPlan_acceptLanguage :: Lens.Lens' DeleteProvisionedProductPlan (Core.Maybe Core.Text)
deleteProvisionedProductPlan_acceptLanguage = Lens.lens (\DeleteProvisionedProductPlan' {acceptLanguage} -> acceptLanguage) (\s@DeleteProvisionedProductPlan' {} a -> s {acceptLanguage = a} :: DeleteProvisionedProductPlan)

-- | The plan identifier.
deleteProvisionedProductPlan_planId :: Lens.Lens' DeleteProvisionedProductPlan Core.Text
deleteProvisionedProductPlan_planId = Lens.lens (\DeleteProvisionedProductPlan' {planId} -> planId) (\s@DeleteProvisionedProductPlan' {} a -> s {planId = a} :: DeleteProvisionedProductPlan)

instance Core.AWSRequest DeleteProvisionedProductPlan where
  type
    AWSResponse DeleteProvisionedProductPlan =
      DeleteProvisionedProductPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisionedProductPlanResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProvisionedProductPlan

instance Core.NFData DeleteProvisionedProductPlan

instance Core.ToHeaders DeleteProvisionedProductPlan where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DeleteProvisionedProductPlan" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProvisionedProductPlan where
  toJSON DeleteProvisionedProductPlan' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IgnoreErrors" Core..=) Core.<$> ignoreErrors,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PlanId" Core..= planId)
          ]
      )

instance Core.ToPath DeleteProvisionedProductPlan where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProvisionedProductPlan where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProvisionedProductPlanResponse' smart constructor.
data DeleteProvisionedProductPlanResponse = DeleteProvisionedProductPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProvisionedProductPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProvisionedProductPlanResponse_httpStatus' - The response's http status code.
newDeleteProvisionedProductPlanResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProvisionedProductPlanResponse
newDeleteProvisionedProductPlanResponse pHttpStatus_ =
  DeleteProvisionedProductPlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProvisionedProductPlanResponse_httpStatus :: Lens.Lens' DeleteProvisionedProductPlanResponse Core.Int
deleteProvisionedProductPlanResponse_httpStatus = Lens.lens (\DeleteProvisionedProductPlanResponse' {httpStatus} -> httpStatus) (\s@DeleteProvisionedProductPlanResponse' {} a -> s {httpStatus = a} :: DeleteProvisionedProductPlanResponse)

instance
  Core.NFData
    DeleteProvisionedProductPlanResponse
