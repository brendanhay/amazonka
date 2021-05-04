{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDeleteProvisionedProductPlan' smart constructor.
data DeleteProvisionedProductPlan = DeleteProvisionedProductPlan'
  { -- | If set to true, AWS Service Catalog stops managing the specified
    -- provisioned product even if it cannot delete the underlying resources.
    ignoreErrors :: Prelude.Maybe Prelude.Bool,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The plan identifier.
    planId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteProvisionedProductPlan
newDeleteProvisionedProductPlan pPlanId_ =
  DeleteProvisionedProductPlan'
    { ignoreErrors =
        Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      planId = pPlanId_
    }

-- | If set to true, AWS Service Catalog stops managing the specified
-- provisioned product even if it cannot delete the underlying resources.
deleteProvisionedProductPlan_ignoreErrors :: Lens.Lens' DeleteProvisionedProductPlan (Prelude.Maybe Prelude.Bool)
deleteProvisionedProductPlan_ignoreErrors = Lens.lens (\DeleteProvisionedProductPlan' {ignoreErrors} -> ignoreErrors) (\s@DeleteProvisionedProductPlan' {} a -> s {ignoreErrors = a} :: DeleteProvisionedProductPlan)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteProvisionedProductPlan_acceptLanguage :: Lens.Lens' DeleteProvisionedProductPlan (Prelude.Maybe Prelude.Text)
deleteProvisionedProductPlan_acceptLanguage = Lens.lens (\DeleteProvisionedProductPlan' {acceptLanguage} -> acceptLanguage) (\s@DeleteProvisionedProductPlan' {} a -> s {acceptLanguage = a} :: DeleteProvisionedProductPlan)

-- | The plan identifier.
deleteProvisionedProductPlan_planId :: Lens.Lens' DeleteProvisionedProductPlan Prelude.Text
deleteProvisionedProductPlan_planId = Lens.lens (\DeleteProvisionedProductPlan' {planId} -> planId) (\s@DeleteProvisionedProductPlan' {} a -> s {planId = a} :: DeleteProvisionedProductPlan)

instance
  Prelude.AWSRequest
    DeleteProvisionedProductPlan
  where
  type
    Rs DeleteProvisionedProductPlan =
      DeleteProvisionedProductPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisionedProductPlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteProvisionedProductPlan

instance Prelude.NFData DeleteProvisionedProductPlan

instance
  Prelude.ToHeaders
    DeleteProvisionedProductPlan
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DeleteProvisionedProductPlan" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProvisionedProductPlan where
  toJSON DeleteProvisionedProductPlan' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IgnoreErrors" Prelude..=)
              Prelude.<$> ignoreErrors,
            ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PlanId" Prelude..= planId)
          ]
      )

instance Prelude.ToPath DeleteProvisionedProductPlan where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProvisionedProductPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProvisionedProductPlanResponse' smart constructor.
data DeleteProvisionedProductPlanResponse = DeleteProvisionedProductPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteProvisionedProductPlanResponse
newDeleteProvisionedProductPlanResponse pHttpStatus_ =
  DeleteProvisionedProductPlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProvisionedProductPlanResponse_httpStatus :: Lens.Lens' DeleteProvisionedProductPlanResponse Prelude.Int
deleteProvisionedProductPlanResponse_httpStatus = Lens.lens (\DeleteProvisionedProductPlanResponse' {httpStatus} -> httpStatus) (\s@DeleteProvisionedProductPlanResponse' {} a -> s {httpStatus = a} :: DeleteProvisionedProductPlanResponse)

instance
  Prelude.NFData
    DeleteProvisionedProductPlanResponse
