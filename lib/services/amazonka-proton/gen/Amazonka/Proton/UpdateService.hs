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
-- Module      : Amazonka.Proton.UpdateService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edit a service description or use a spec to add and delete service
-- instances.
--
-- Existing service instances and the service pipeline /can\'t/ be edited
-- using this API. They can only be deleted.
--
-- Use the @description@ parameter to modify the description.
--
-- Edit the @spec@ parameter to add or delete instances.
--
-- You can\'t delete a service instance (remove it from the spec) if it has
-- an attached component.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
module Amazonka.Proton.UpdateService
  ( -- * Creating a Request
    UpdateService (..),
    newUpdateService,

    -- * Request Lenses
    updateService_description,
    updateService_spec,
    updateService_name,

    -- * Destructuring the Response
    UpdateServiceResponse (..),
    newUpdateServiceResponse,

    -- * Response Lenses
    updateServiceResponse_httpStatus,
    updateServiceResponse_service,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateService' smart constructor.
data UpdateService = UpdateService'
  { -- | The edited service description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Lists the service instances to add and the existing service instances to
    -- remain. Omit the existing service instances to delete from the list.
    -- /Don\'t/ include edits to the existing service instances or pipeline.
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-svc-update.html Edit a service>
    -- in the /Proton User Guide/.
    spec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the service to edit.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateService_description' - The edited service description.
--
-- 'spec', 'updateService_spec' - Lists the service instances to add and the existing service instances to
-- remain. Omit the existing service instances to delete from the list.
-- /Don\'t/ include edits to the existing service instances or pipeline.
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-svc-update.html Edit a service>
-- in the /Proton User Guide/.
--
-- 'name', 'updateService_name' - The name of the service to edit.
newUpdateService ::
  -- | 'name'
  Prelude.Text ->
  UpdateService
newUpdateService pName_ =
  UpdateService'
    { description = Prelude.Nothing,
      spec = Prelude.Nothing,
      name = pName_
    }

-- | The edited service description.
updateService_description :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Text)
updateService_description = Lens.lens (\UpdateService' {description} -> description) (\s@UpdateService' {} a -> s {description = a} :: UpdateService) Prelude.. Lens.mapping Data._Sensitive

-- | Lists the service instances to add and the existing service instances to
-- remain. Omit the existing service instances to delete from the list.
-- /Don\'t/ include edits to the existing service instances or pipeline.
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-svc-update.html Edit a service>
-- in the /Proton User Guide/.
updateService_spec :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Text)
updateService_spec = Lens.lens (\UpdateService' {spec} -> spec) (\s@UpdateService' {} a -> s {spec = a} :: UpdateService) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the service to edit.
updateService_name :: Lens.Lens' UpdateService Prelude.Text
updateService_name = Lens.lens (\UpdateService' {name} -> name) (\s@UpdateService' {} a -> s {name = a} :: UpdateService)

instance Core.AWSRequest UpdateService where
  type
    AWSResponse UpdateService =
      UpdateServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "service")
      )

instance Prelude.Hashable UpdateService where
  hashWithSalt _salt UpdateService' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateService where
  rnf UpdateService' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf spec `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders UpdateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateService where
  toJSON UpdateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("spec" Data..=) Prelude.<$> spec,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateService where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service detail data that\'s returned by Proton.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServiceResponse_httpStatus' - The response's http status code.
--
-- 'service', 'updateServiceResponse_service' - The service detail data that\'s returned by Proton.
newUpdateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'service'
  Service ->
  UpdateServiceResponse
newUpdateServiceResponse pHttpStatus_ pService_ =
  UpdateServiceResponse'
    { httpStatus = pHttpStatus_,
      service = pService_
    }

-- | The response's http status code.
updateServiceResponse_httpStatus :: Lens.Lens' UpdateServiceResponse Prelude.Int
updateServiceResponse_httpStatus = Lens.lens (\UpdateServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceResponse' {} a -> s {httpStatus = a} :: UpdateServiceResponse)

-- | The service detail data that\'s returned by Proton.
updateServiceResponse_service :: Lens.Lens' UpdateServiceResponse Service
updateServiceResponse_service = Lens.lens (\UpdateServiceResponse' {service} -> service) (\s@UpdateServiceResponse' {} a -> s {service = a} :: UpdateServiceResponse)

instance Prelude.NFData UpdateServiceResponse where
  rnf UpdateServiceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf service
