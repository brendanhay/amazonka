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
-- Module      : Amazonka.Route53.DeleteHealthCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a health check.
--
-- Amazon Route 53 does not prevent you from deleting a health check even
-- if the health check is associated with one or more resource record sets.
-- If you delete a health check and you don\'t update the associated
-- resource record sets, the future status of the health check can\'t be
-- predicted and may change. This will affect the routing of DNS queries
-- for your DNS failover configuration. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html#health-checks-deleting.html Replacing and Deleting Health Checks>
-- in the /Amazon Route 53 Developer Guide/.
--
-- If you\'re using Cloud Map and you configured Cloud Map to create a
-- Route 53 health check when you register an instance, you can\'t use the
-- Route 53 @DeleteHealthCheck@ command to delete the health check. The
-- health check is deleted automatically when you deregister the instance;
-- there can be a delay of several hours before the health check is deleted
-- from Route 53.
module Amazonka.Route53.DeleteHealthCheck
  ( -- * Creating a Request
    DeleteHealthCheck (..),
    newDeleteHealthCheck,

    -- * Request Lenses
    deleteHealthCheck_healthCheckId,

    -- * Destructuring the Response
    DeleteHealthCheckResponse (..),
    newDeleteHealthCheckResponse,

    -- * Response Lenses
    deleteHealthCheckResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | This action deletes a health check.
--
-- /See:/ 'newDeleteHealthCheck' smart constructor.
data DeleteHealthCheck = DeleteHealthCheck'
  { -- | The ID of the health check that you want to delete.
    healthCheckId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckId', 'deleteHealthCheck_healthCheckId' - The ID of the health check that you want to delete.
newDeleteHealthCheck ::
  -- | 'healthCheckId'
  Prelude.Text ->
  DeleteHealthCheck
newDeleteHealthCheck pHealthCheckId_ =
  DeleteHealthCheck' {healthCheckId = pHealthCheckId_}

-- | The ID of the health check that you want to delete.
deleteHealthCheck_healthCheckId :: Lens.Lens' DeleteHealthCheck Prelude.Text
deleteHealthCheck_healthCheckId = Lens.lens (\DeleteHealthCheck' {healthCheckId} -> healthCheckId) (\s@DeleteHealthCheck' {} a -> s {healthCheckId = a} :: DeleteHealthCheck)

instance Core.AWSRequest DeleteHealthCheck where
  type
    AWSResponse DeleteHealthCheck =
      DeleteHealthCheckResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteHealthCheckResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteHealthCheck where
  hashWithSalt _salt DeleteHealthCheck' {..} =
    _salt `Prelude.hashWithSalt` healthCheckId

instance Prelude.NFData DeleteHealthCheck where
  rnf DeleteHealthCheck' {..} =
    Prelude.rnf healthCheckId

instance Data.ToHeaders DeleteHealthCheck where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteHealthCheck where
  toPath DeleteHealthCheck' {..} =
    Prelude.mconcat
      ["/2013-04-01/healthcheck/", Data.toBS healthCheckId]

instance Data.ToQuery DeleteHealthCheck where
  toQuery = Prelude.const Prelude.mempty

-- | An empty element.
--
-- /See:/ 'newDeleteHealthCheckResponse' smart constructor.
data DeleteHealthCheckResponse = DeleteHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHealthCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteHealthCheckResponse_httpStatus' - The response's http status code.
newDeleteHealthCheckResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteHealthCheckResponse
newDeleteHealthCheckResponse pHttpStatus_ =
  DeleteHealthCheckResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteHealthCheckResponse_httpStatus :: Lens.Lens' DeleteHealthCheckResponse Prelude.Int
deleteHealthCheckResponse_httpStatus = Lens.lens (\DeleteHealthCheckResponse' {httpStatus} -> httpStatus) (\s@DeleteHealthCheckResponse' {} a -> s {httpStatus = a} :: DeleteHealthCheckResponse)

instance Prelude.NFData DeleteHealthCheckResponse where
  rnf DeleteHealthCheckResponse' {..} =
    Prelude.rnf httpStatus
