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
-- Module      : Amazonka.IoT.DeleteFleetMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified fleet metric. Returns successfully with no error
-- if the deletion is successful or you specify a fleet metric that
-- doesn\'t exist.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteFleetMetric>
-- action.
module Amazonka.IoT.DeleteFleetMetric
  ( -- * Creating a Request
    DeleteFleetMetric (..),
    newDeleteFleetMetric,

    -- * Request Lenses
    deleteFleetMetric_expectedVersion,
    deleteFleetMetric_metricName,

    -- * Destructuring the Response
    DeleteFleetMetricResponse (..),
    newDeleteFleetMetricResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFleetMetric' smart constructor.
data DeleteFleetMetric = DeleteFleetMetric'
  { -- | The expected version of the fleet metric to delete.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the fleet metric to delete.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'deleteFleetMetric_expectedVersion' - The expected version of the fleet metric to delete.
--
-- 'metricName', 'deleteFleetMetric_metricName' - The name of the fleet metric to delete.
newDeleteFleetMetric ::
  -- | 'metricName'
  Prelude.Text ->
  DeleteFleetMetric
newDeleteFleetMetric pMetricName_ =
  DeleteFleetMetric'
    { expectedVersion =
        Prelude.Nothing,
      metricName = pMetricName_
    }

-- | The expected version of the fleet metric to delete.
deleteFleetMetric_expectedVersion :: Lens.Lens' DeleteFleetMetric (Prelude.Maybe Prelude.Integer)
deleteFleetMetric_expectedVersion = Lens.lens (\DeleteFleetMetric' {expectedVersion} -> expectedVersion) (\s@DeleteFleetMetric' {} a -> s {expectedVersion = a} :: DeleteFleetMetric)

-- | The name of the fleet metric to delete.
deleteFleetMetric_metricName :: Lens.Lens' DeleteFleetMetric Prelude.Text
deleteFleetMetric_metricName = Lens.lens (\DeleteFleetMetric' {metricName} -> metricName) (\s@DeleteFleetMetric' {} a -> s {metricName = a} :: DeleteFleetMetric)

instance Core.AWSRequest DeleteFleetMetric where
  type
    AWSResponse DeleteFleetMetric =
      DeleteFleetMetricResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteFleetMetricResponse'

instance Prelude.Hashable DeleteFleetMetric where
  hashWithSalt _salt DeleteFleetMetric' {..} =
    _salt
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData DeleteFleetMetric where
  rnf DeleteFleetMetric' {..} =
    Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf metricName

instance Data.ToHeaders DeleteFleetMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteFleetMetric where
  toPath DeleteFleetMetric' {..} =
    Prelude.mconcat
      ["/fleet-metric/", Data.toBS metricName]

instance Data.ToQuery DeleteFleetMetric where
  toQuery DeleteFleetMetric' {..} =
    Prelude.mconcat
      ["expectedVersion" Data.=: expectedVersion]

-- | /See:/ 'newDeleteFleetMetricResponse' smart constructor.
data DeleteFleetMetricResponse = DeleteFleetMetricResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFleetMetricResponse ::
  DeleteFleetMetricResponse
newDeleteFleetMetricResponse =
  DeleteFleetMetricResponse'

instance Prelude.NFData DeleteFleetMetricResponse where
  rnf _ = ()
