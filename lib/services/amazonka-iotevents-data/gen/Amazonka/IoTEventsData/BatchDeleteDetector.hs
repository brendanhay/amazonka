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
-- Module      : Amazonka.IoTEventsData.BatchDeleteDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more detectors that were created. When a detector is
-- deleted, its state will be cleared and the detector will be removed from
-- the list of detectors. The deleted detector will no longer appear if
-- referenced in the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_ListDetectors.html ListDetectors>
-- API call.
module Amazonka.IoTEventsData.BatchDeleteDetector
  ( -- * Creating a Request
    BatchDeleteDetector (..),
    newBatchDeleteDetector,

    -- * Request Lenses
    batchDeleteDetector_detectors,

    -- * Destructuring the Response
    BatchDeleteDetectorResponse (..),
    newBatchDeleteDetectorResponse,

    -- * Response Lenses
    batchDeleteDetectorResponse_batchDeleteDetectorErrorEntries,
    batchDeleteDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteDetector' smart constructor.
data BatchDeleteDetector = BatchDeleteDetector'
  { -- | The list of one or more detectors to be deleted.
    detectors :: Prelude.NonEmpty DeleteDetectorRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectors', 'batchDeleteDetector_detectors' - The list of one or more detectors to be deleted.
newBatchDeleteDetector ::
  -- | 'detectors'
  Prelude.NonEmpty DeleteDetectorRequest ->
  BatchDeleteDetector
newBatchDeleteDetector pDetectors_ =
  BatchDeleteDetector'
    { detectors =
        Lens.coerced Lens.# pDetectors_
    }

-- | The list of one or more detectors to be deleted.
batchDeleteDetector_detectors :: Lens.Lens' BatchDeleteDetector (Prelude.NonEmpty DeleteDetectorRequest)
batchDeleteDetector_detectors = Lens.lens (\BatchDeleteDetector' {detectors} -> detectors) (\s@BatchDeleteDetector' {} a -> s {detectors = a} :: BatchDeleteDetector) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteDetector where
  type
    AWSResponse BatchDeleteDetector =
      BatchDeleteDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteDetectorResponse'
            Prelude.<$> ( x Data..?> "batchDeleteDetectorErrorEntries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteDetector where
  hashWithSalt _salt BatchDeleteDetector' {..} =
    _salt `Prelude.hashWithSalt` detectors

instance Prelude.NFData BatchDeleteDetector where
  rnf BatchDeleteDetector' {..} = Prelude.rnf detectors

instance Data.ToHeaders BatchDeleteDetector where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchDeleteDetector where
  toJSON BatchDeleteDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("detectors" Data..= detectors)]
      )

instance Data.ToPath BatchDeleteDetector where
  toPath = Prelude.const "/detectors/delete"

instance Data.ToQuery BatchDeleteDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteDetectorResponse' smart constructor.
data BatchDeleteDetectorResponse = BatchDeleteDetectorResponse'
  { -- | A list of errors associated with the request, or an empty array (@[]@)
    -- if there are no errors. Each error entry contains a @messageId@ that
    -- helps you identify the entry that failed.
    batchDeleteDetectorErrorEntries :: Prelude.Maybe [BatchDeleteDetectorErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchDeleteDetectorErrorEntries', 'batchDeleteDetectorResponse_batchDeleteDetectorErrorEntries' - A list of errors associated with the request, or an empty array (@[]@)
-- if there are no errors. Each error entry contains a @messageId@ that
-- helps you identify the entry that failed.
--
-- 'httpStatus', 'batchDeleteDetectorResponse_httpStatus' - The response's http status code.
newBatchDeleteDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteDetectorResponse
newBatchDeleteDetectorResponse pHttpStatus_ =
  BatchDeleteDetectorResponse'
    { batchDeleteDetectorErrorEntries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors associated with the request, or an empty array (@[]@)
-- if there are no errors. Each error entry contains a @messageId@ that
-- helps you identify the entry that failed.
batchDeleteDetectorResponse_batchDeleteDetectorErrorEntries :: Lens.Lens' BatchDeleteDetectorResponse (Prelude.Maybe [BatchDeleteDetectorErrorEntry])
batchDeleteDetectorResponse_batchDeleteDetectorErrorEntries = Lens.lens (\BatchDeleteDetectorResponse' {batchDeleteDetectorErrorEntries} -> batchDeleteDetectorErrorEntries) (\s@BatchDeleteDetectorResponse' {} a -> s {batchDeleteDetectorErrorEntries = a} :: BatchDeleteDetectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteDetectorResponse_httpStatus :: Lens.Lens' BatchDeleteDetectorResponse Prelude.Int
batchDeleteDetectorResponse_httpStatus = Lens.lens (\BatchDeleteDetectorResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteDetectorResponse' {} a -> s {httpStatus = a} :: BatchDeleteDetectorResponse)

instance Prelude.NFData BatchDeleteDetectorResponse where
  rnf BatchDeleteDetectorResponse' {..} =
    Prelude.rnf batchDeleteDetectorErrorEntries
      `Prelude.seq` Prelude.rnf httpStatus
