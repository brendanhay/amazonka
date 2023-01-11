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
-- Module      : Amazonka.IoT.DeleteCustomMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender detect custom metric.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteCustomMetric>
-- action.
--
-- Before you can delete a custom metric, you must first remove the custom
-- metric from all security profiles it\'s a part of. The security profile
-- associated with the custom metric can be found using the
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_ListSecurityProfiles.html ListSecurityProfiles>
-- API with @metricName@ set to your custom metric name.
module Amazonka.IoT.DeleteCustomMetric
  ( -- * Creating a Request
    DeleteCustomMetric (..),
    newDeleteCustomMetric,

    -- * Request Lenses
    deleteCustomMetric_metricName,

    -- * Destructuring the Response
    DeleteCustomMetricResponse (..),
    newDeleteCustomMetricResponse,

    -- * Response Lenses
    deleteCustomMetricResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomMetric' smart constructor.
data DeleteCustomMetric = DeleteCustomMetric'
  { -- | The name of the custom metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'deleteCustomMetric_metricName' - The name of the custom metric.
newDeleteCustomMetric ::
  -- | 'metricName'
  Prelude.Text ->
  DeleteCustomMetric
newDeleteCustomMetric pMetricName_ =
  DeleteCustomMetric' {metricName = pMetricName_}

-- | The name of the custom metric.
deleteCustomMetric_metricName :: Lens.Lens' DeleteCustomMetric Prelude.Text
deleteCustomMetric_metricName = Lens.lens (\DeleteCustomMetric' {metricName} -> metricName) (\s@DeleteCustomMetric' {} a -> s {metricName = a} :: DeleteCustomMetric)

instance Core.AWSRequest DeleteCustomMetric where
  type
    AWSResponse DeleteCustomMetric =
      DeleteCustomMetricResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomMetricResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomMetric where
  hashWithSalt _salt DeleteCustomMetric' {..} =
    _salt `Prelude.hashWithSalt` metricName

instance Prelude.NFData DeleteCustomMetric where
  rnf DeleteCustomMetric' {..} = Prelude.rnf metricName

instance Data.ToHeaders DeleteCustomMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCustomMetric where
  toPath DeleteCustomMetric' {..} =
    Prelude.mconcat
      ["/custom-metric/", Data.toBS metricName]

instance Data.ToQuery DeleteCustomMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomMetricResponse' smart constructor.
data DeleteCustomMetricResponse = DeleteCustomMetricResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCustomMetricResponse_httpStatus' - The response's http status code.
newDeleteCustomMetricResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomMetricResponse
newDeleteCustomMetricResponse pHttpStatus_ =
  DeleteCustomMetricResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCustomMetricResponse_httpStatus :: Lens.Lens' DeleteCustomMetricResponse Prelude.Int
deleteCustomMetricResponse_httpStatus = Lens.lens (\DeleteCustomMetricResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomMetricResponse' {} a -> s {httpStatus = a} :: DeleteCustomMetricResponse)

instance Prelude.NFData DeleteCustomMetricResponse where
  rnf DeleteCustomMetricResponse' {..} =
    Prelude.rnf httpStatus
