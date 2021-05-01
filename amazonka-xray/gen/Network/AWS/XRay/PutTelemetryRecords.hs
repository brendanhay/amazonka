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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newPutTelemetryRecords' smart constructor.
data PutTelemetryRecords = PutTelemetryRecords'
  { resourceARN :: Prelude.Maybe Prelude.Text,
    hostname :: Prelude.Maybe Prelude.Text,
    eC2InstanceId :: Prelude.Maybe Prelude.Text,
    telemetryRecords :: [TelemetryRecord]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceARN = Prelude.Nothing,
      hostname = Prelude.Nothing,
      eC2InstanceId = Prelude.Nothing,
      telemetryRecords = Prelude.mempty
    }

-- |
putTelemetryRecords_resourceARN :: Lens.Lens' PutTelemetryRecords (Prelude.Maybe Prelude.Text)
putTelemetryRecords_resourceARN = Lens.lens (\PutTelemetryRecords' {resourceARN} -> resourceARN) (\s@PutTelemetryRecords' {} a -> s {resourceARN = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_hostname :: Lens.Lens' PutTelemetryRecords (Prelude.Maybe Prelude.Text)
putTelemetryRecords_hostname = Lens.lens (\PutTelemetryRecords' {hostname} -> hostname) (\s@PutTelemetryRecords' {} a -> s {hostname = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_eC2InstanceId :: Lens.Lens' PutTelemetryRecords (Prelude.Maybe Prelude.Text)
putTelemetryRecords_eC2InstanceId = Lens.lens (\PutTelemetryRecords' {eC2InstanceId} -> eC2InstanceId) (\s@PutTelemetryRecords' {} a -> s {eC2InstanceId = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_telemetryRecords :: Lens.Lens' PutTelemetryRecords [TelemetryRecord]
putTelemetryRecords_telemetryRecords = Lens.lens (\PutTelemetryRecords' {telemetryRecords} -> telemetryRecords) (\s@PutTelemetryRecords' {} a -> s {telemetryRecords = a} :: PutTelemetryRecords) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest PutTelemetryRecords where
  type
    Rs PutTelemetryRecords =
      PutTelemetryRecordsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutTelemetryRecordsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutTelemetryRecords

instance Prelude.NFData PutTelemetryRecords

instance Prelude.ToHeaders PutTelemetryRecords where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON PutTelemetryRecords where
  toJSON PutTelemetryRecords' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceARN" Prelude..=) Prelude.<$> resourceARN,
            ("Hostname" Prelude..=) Prelude.<$> hostname,
            ("EC2InstanceId" Prelude..=)
              Prelude.<$> eC2InstanceId,
            Prelude.Just
              ("TelemetryRecords" Prelude..= telemetryRecords)
          ]
      )

instance Prelude.ToPath PutTelemetryRecords where
  toPath = Prelude.const "/TelemetryRecords"

instance Prelude.ToQuery PutTelemetryRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutTelemetryRecordsResponse' smart constructor.
data PutTelemetryRecordsResponse = PutTelemetryRecordsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PutTelemetryRecordsResponse
newPutTelemetryRecordsResponse pHttpStatus_ =
  PutTelemetryRecordsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putTelemetryRecordsResponse_httpStatus :: Lens.Lens' PutTelemetryRecordsResponse Prelude.Int
putTelemetryRecordsResponse_httpStatus = Lens.lens (\PutTelemetryRecordsResponse' {httpStatus} -> httpStatus) (\s@PutTelemetryRecordsResponse' {} a -> s {httpStatus = a} :: PutTelemetryRecordsResponse)

instance Prelude.NFData PutTelemetryRecordsResponse
