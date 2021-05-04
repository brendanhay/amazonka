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
-- Module      : Network.AWS.IoT.DeleteCustomMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Before you can delete a custom metric, you must first remove the custom
-- metric from all security profiles it\'s a part of. The security profile
-- associated with the custom metric can be found using the
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_ListSecurityProfiles.html ListSecurityProfiles>
-- API with @metricName@ set to your custom metric name.
--
-- Deletes a Device Defender detect custom metric.
module Network.AWS.IoT.DeleteCustomMetric
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCustomMetric' smart constructor.
data DeleteCustomMetric = DeleteCustomMetric'
  { -- | The name of the custom metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCustomMetric where
  type
    Rs DeleteCustomMetric =
      DeleteCustomMetricResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomMetricResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomMetric

instance Prelude.NFData DeleteCustomMetric

instance Prelude.ToHeaders DeleteCustomMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteCustomMetric where
  toPath DeleteCustomMetric' {..} =
    Prelude.mconcat
      ["/custom-metric/", Prelude.toBS metricName]

instance Prelude.ToQuery DeleteCustomMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomMetricResponse' smart constructor.
data DeleteCustomMetricResponse = DeleteCustomMetricResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteCustomMetricResponse
