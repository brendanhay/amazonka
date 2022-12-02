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
-- Module      : Amazonka.XRay.PutTelemetryRecords
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by the Amazon Web Services X-Ray daemon to upload telemetry.
module Amazonka.XRay.PutTelemetryRecords
  ( -- * Creating a Request
    PutTelemetryRecords (..),
    newPutTelemetryRecords,

    -- * Request Lenses
    putTelemetryRecords_eC2InstanceId,
    putTelemetryRecords_hostname,
    putTelemetryRecords_resourceARN,
    putTelemetryRecords_telemetryRecords,

    -- * Destructuring the Response
    PutTelemetryRecordsResponse (..),
    newPutTelemetryRecordsResponse,

    -- * Response Lenses
    putTelemetryRecordsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newPutTelemetryRecords' smart constructor.
data PutTelemetryRecords = PutTelemetryRecords'
  { eC2InstanceId :: Prelude.Maybe Prelude.Text,
    hostname :: Prelude.Maybe Prelude.Text,
    resourceARN :: Prelude.Maybe Prelude.Text,
    telemetryRecords :: [TelemetryRecord]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutTelemetryRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2InstanceId', 'putTelemetryRecords_eC2InstanceId' -
--
-- 'hostname', 'putTelemetryRecords_hostname' -
--
-- 'resourceARN', 'putTelemetryRecords_resourceARN' -
--
-- 'telemetryRecords', 'putTelemetryRecords_telemetryRecords' -
newPutTelemetryRecords ::
  PutTelemetryRecords
newPutTelemetryRecords =
  PutTelemetryRecords'
    { eC2InstanceId =
        Prelude.Nothing,
      hostname = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      telemetryRecords = Prelude.mempty
    }

-- |
putTelemetryRecords_eC2InstanceId :: Lens.Lens' PutTelemetryRecords (Prelude.Maybe Prelude.Text)
putTelemetryRecords_eC2InstanceId = Lens.lens (\PutTelemetryRecords' {eC2InstanceId} -> eC2InstanceId) (\s@PutTelemetryRecords' {} a -> s {eC2InstanceId = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_hostname :: Lens.Lens' PutTelemetryRecords (Prelude.Maybe Prelude.Text)
putTelemetryRecords_hostname = Lens.lens (\PutTelemetryRecords' {hostname} -> hostname) (\s@PutTelemetryRecords' {} a -> s {hostname = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_resourceARN :: Lens.Lens' PutTelemetryRecords (Prelude.Maybe Prelude.Text)
putTelemetryRecords_resourceARN = Lens.lens (\PutTelemetryRecords' {resourceARN} -> resourceARN) (\s@PutTelemetryRecords' {} a -> s {resourceARN = a} :: PutTelemetryRecords)

-- |
putTelemetryRecords_telemetryRecords :: Lens.Lens' PutTelemetryRecords [TelemetryRecord]
putTelemetryRecords_telemetryRecords = Lens.lens (\PutTelemetryRecords' {telemetryRecords} -> telemetryRecords) (\s@PutTelemetryRecords' {} a -> s {telemetryRecords = a} :: PutTelemetryRecords) Prelude.. Lens.coerced

instance Core.AWSRequest PutTelemetryRecords where
  type
    AWSResponse PutTelemetryRecords =
      PutTelemetryRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutTelemetryRecordsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutTelemetryRecords where
  hashWithSalt _salt PutTelemetryRecords' {..} =
    _salt `Prelude.hashWithSalt` eC2InstanceId
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` telemetryRecords

instance Prelude.NFData PutTelemetryRecords where
  rnf PutTelemetryRecords' {..} =
    Prelude.rnf eC2InstanceId
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf telemetryRecords

instance Data.ToHeaders PutTelemetryRecords where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutTelemetryRecords where
  toJSON PutTelemetryRecords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EC2InstanceId" Data..=) Prelude.<$> eC2InstanceId,
            ("Hostname" Data..=) Prelude.<$> hostname,
            ("ResourceARN" Data..=) Prelude.<$> resourceARN,
            Prelude.Just
              ("TelemetryRecords" Data..= telemetryRecords)
          ]
      )

instance Data.ToPath PutTelemetryRecords where
  toPath = Prelude.const "/TelemetryRecords"

instance Data.ToQuery PutTelemetryRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutTelemetryRecordsResponse' smart constructor.
data PutTelemetryRecordsResponse = PutTelemetryRecordsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData PutTelemetryRecordsResponse where
  rnf PutTelemetryRecordsResponse' {..} =
    Prelude.rnf httpStatus
