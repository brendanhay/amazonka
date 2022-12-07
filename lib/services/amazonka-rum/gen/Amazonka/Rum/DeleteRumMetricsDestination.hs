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
-- Module      : Amazonka.Rum.DeleteRumMetricsDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a destination for CloudWatch RUM extended metrics, so that the
-- specified app monitor stops sending extended metrics to that
-- destination.
module Amazonka.Rum.DeleteRumMetricsDestination
  ( -- * Creating a Request
    DeleteRumMetricsDestination (..),
    newDeleteRumMetricsDestination,

    -- * Request Lenses
    deleteRumMetricsDestination_destinationArn,
    deleteRumMetricsDestination_appMonitorName,
    deleteRumMetricsDestination_destination,

    -- * Destructuring the Response
    DeleteRumMetricsDestinationResponse (..),
    newDeleteRumMetricsDestinationResponse,

    -- * Response Lenses
    deleteRumMetricsDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newDeleteRumMetricsDestination' smart constructor.
data DeleteRumMetricsDestination = DeleteRumMetricsDestination'
  { -- | This parameter is required if @Destination@ is @Evidently@. If
    -- @Destination@ is @CloudWatch@, do not use this parameter. This parameter
    -- specifies the ARN of the Evidently experiment that corresponds to the
    -- destination to delete.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the app monitor that is sending metrics to the destination
    -- that you want to delete.
    appMonitorName :: Prelude.Text,
    -- | The type of destination to delete. Valid values are @CloudWatch@ and
    -- @Evidently@.
    destination :: MetricDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRumMetricsDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'deleteRumMetricsDestination_destinationArn' - This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter. This parameter
-- specifies the ARN of the Evidently experiment that corresponds to the
-- destination to delete.
--
-- 'appMonitorName', 'deleteRumMetricsDestination_appMonitorName' - The name of the app monitor that is sending metrics to the destination
-- that you want to delete.
--
-- 'destination', 'deleteRumMetricsDestination_destination' - The type of destination to delete. Valid values are @CloudWatch@ and
-- @Evidently@.
newDeleteRumMetricsDestination ::
  -- | 'appMonitorName'
  Prelude.Text ->
  -- | 'destination'
  MetricDestination ->
  DeleteRumMetricsDestination
newDeleteRumMetricsDestination
  pAppMonitorName_
  pDestination_ =
    DeleteRumMetricsDestination'
      { destinationArn =
          Prelude.Nothing,
        appMonitorName = pAppMonitorName_,
        destination = pDestination_
      }

-- | This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter. This parameter
-- specifies the ARN of the Evidently experiment that corresponds to the
-- destination to delete.
deleteRumMetricsDestination_destinationArn :: Lens.Lens' DeleteRumMetricsDestination (Prelude.Maybe Prelude.Text)
deleteRumMetricsDestination_destinationArn = Lens.lens (\DeleteRumMetricsDestination' {destinationArn} -> destinationArn) (\s@DeleteRumMetricsDestination' {} a -> s {destinationArn = a} :: DeleteRumMetricsDestination)

-- | The name of the app monitor that is sending metrics to the destination
-- that you want to delete.
deleteRumMetricsDestination_appMonitorName :: Lens.Lens' DeleteRumMetricsDestination Prelude.Text
deleteRumMetricsDestination_appMonitorName = Lens.lens (\DeleteRumMetricsDestination' {appMonitorName} -> appMonitorName) (\s@DeleteRumMetricsDestination' {} a -> s {appMonitorName = a} :: DeleteRumMetricsDestination)

-- | The type of destination to delete. Valid values are @CloudWatch@ and
-- @Evidently@.
deleteRumMetricsDestination_destination :: Lens.Lens' DeleteRumMetricsDestination MetricDestination
deleteRumMetricsDestination_destination = Lens.lens (\DeleteRumMetricsDestination' {destination} -> destination) (\s@DeleteRumMetricsDestination' {} a -> s {destination = a} :: DeleteRumMetricsDestination)

instance Core.AWSRequest DeleteRumMetricsDestination where
  type
    AWSResponse DeleteRumMetricsDestination =
      DeleteRumMetricsDestinationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRumMetricsDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRumMetricsDestination where
  hashWithSalt _salt DeleteRumMetricsDestination' {..} =
    _salt `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` appMonitorName
      `Prelude.hashWithSalt` destination

instance Prelude.NFData DeleteRumMetricsDestination where
  rnf DeleteRumMetricsDestination' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf appMonitorName
      `Prelude.seq` Prelude.rnf destination

instance Data.ToHeaders DeleteRumMetricsDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRumMetricsDestination where
  toPath DeleteRumMetricsDestination' {..} =
    Prelude.mconcat
      [ "/rummetrics/",
        Data.toBS appMonitorName,
        "/metricsdestination"
      ]

instance Data.ToQuery DeleteRumMetricsDestination where
  toQuery DeleteRumMetricsDestination' {..} =
    Prelude.mconcat
      [ "destinationArn" Data.=: destinationArn,
        "destination" Data.=: destination
      ]

-- | /See:/ 'newDeleteRumMetricsDestinationResponse' smart constructor.
data DeleteRumMetricsDestinationResponse = DeleteRumMetricsDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRumMetricsDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRumMetricsDestinationResponse_httpStatus' - The response's http status code.
newDeleteRumMetricsDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRumMetricsDestinationResponse
newDeleteRumMetricsDestinationResponse pHttpStatus_ =
  DeleteRumMetricsDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRumMetricsDestinationResponse_httpStatus :: Lens.Lens' DeleteRumMetricsDestinationResponse Prelude.Int
deleteRumMetricsDestinationResponse_httpStatus = Lens.lens (\DeleteRumMetricsDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteRumMetricsDestinationResponse' {} a -> s {httpStatus = a} :: DeleteRumMetricsDestinationResponse)

instance
  Prelude.NFData
    DeleteRumMetricsDestinationResponse
  where
  rnf DeleteRumMetricsDestinationResponse' {..} =
    Prelude.rnf httpStatus
