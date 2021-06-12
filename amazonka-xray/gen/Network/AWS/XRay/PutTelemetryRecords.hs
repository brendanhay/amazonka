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
-- Module      : Network.AWS.XRay.PutTelemetryRecords
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by the AWS X-Ray daemon to upload telemetry.
module Network.AWS.XRay.PutTelemetryRecords
  ( -- * Creating a Request
    PutTelemetryRecords (..),
    newPutTelemetryRecords,

    -- * Request Lenses
    putTelemetryRecords_resourceARN,
    putTelemetryRecords_hostname,
    putTelemetryRecords_eC2InstanceId,
    putTelemetryRecords_telemetryRecords,

    -- * Destructuring the Response
    PutTelemetryRecordsResponse (..),
    newPutTelemetryRecordsResponse,

    -- * Response Lenses
    putTelemetryRecordsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newPutTelemetryRecords' smart constructor.
data PutTelemetryRecords = PutTelemetryRecords'
  { resourceARN :: Core.Maybe Core.Text,
    hostname :: Core.Maybe Core.Text,
    eC2InstanceId :: Core.Maybe Core.Text,
    telemetryRecords :: [TelemetryRecord]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutTelemetryRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'putTelemetryRecords_resourceARN' -
--
-- 'hostname', 'putTelemetryRecords_hostname' -
--
-- 'eC2InstanceId', 'putTelemetryRecords_eC2InstanceId' -
--
-- 'telemetryRecords', 'putTelemetryRecords_telemetryRecords' -
newPutTelemetryRecords ::
  PutTelemetryRecords
newPutTelemetryRecords =
  PutTelemetryRecords'
    { resourceARN = Core.Nothing,
      hostname = Core.Nothing,
      eC2InstanceId = Core.Nothing,
      telemetryRecords = Core.mempty
    }

-- |
putTelemetryRecords_resourceARN :: Lens.Lens' PutTelemetryRecords (Core.Maybe Core.Text)
putTelemetryRecords_resourceARN = Lens.lens (\PutTelemetryRecords' {resourceARN} -> resourceARN) (\s@PutTelemetryRecords' {} a -> s {resourceARN = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_hostname :: Lens.Lens' PutTelemetryRecords (Core.Maybe Core.Text)
putTelemetryRecords_hostname = Lens.lens (\PutTelemetryRecords' {hostname} -> hostname) (\s@PutTelemetryRecords' {} a -> s {hostname = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_eC2InstanceId :: Lens.Lens' PutTelemetryRecords (Core.Maybe Core.Text)
putTelemetryRecords_eC2InstanceId = Lens.lens (\PutTelemetryRecords' {eC2InstanceId} -> eC2InstanceId) (\s@PutTelemetryRecords' {} a -> s {eC2InstanceId = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_telemetryRecords :: Lens.Lens' PutTelemetryRecords [TelemetryRecord]
putTelemetryRecords_telemetryRecords = Lens.lens (\PutTelemetryRecords' {telemetryRecords} -> telemetryRecords) (\s@PutTelemetryRecords' {} a -> s {telemetryRecords = a} :: PutTelemetryRecords) Core.. Lens._Coerce

instance Core.AWSRequest PutTelemetryRecords where
  type
    AWSResponse PutTelemetryRecords =
      PutTelemetryRecordsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutTelemetryRecordsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutTelemetryRecords

instance Core.NFData PutTelemetryRecords

instance Core.ToHeaders PutTelemetryRecords where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PutTelemetryRecords where
  toJSON PutTelemetryRecords' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceARN" Core..=) Core.<$> resourceARN,
            ("Hostname" Core..=) Core.<$> hostname,
            ("EC2InstanceId" Core..=) Core.<$> eC2InstanceId,
            Core.Just
              ("TelemetryRecords" Core..= telemetryRecords)
          ]
      )

instance Core.ToPath PutTelemetryRecords where
  toPath = Core.const "/TelemetryRecords"

instance Core.ToQuery PutTelemetryRecords where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutTelemetryRecordsResponse' smart constructor.
data PutTelemetryRecordsResponse = PutTelemetryRecordsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutTelemetryRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putTelemetryRecordsResponse_httpStatus' - The response's http status code.
newPutTelemetryRecordsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutTelemetryRecordsResponse
newPutTelemetryRecordsResponse pHttpStatus_ =
  PutTelemetryRecordsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putTelemetryRecordsResponse_httpStatus :: Lens.Lens' PutTelemetryRecordsResponse Core.Int
putTelemetryRecordsResponse_httpStatus = Lens.lens (\PutTelemetryRecordsResponse' {httpStatus} -> httpStatus) (\s@PutTelemetryRecordsResponse' {} a -> s {httpStatus = a} :: PutTelemetryRecordsResponse)

instance Core.NFData PutTelemetryRecordsResponse
