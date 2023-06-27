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
-- Module      : Amazonka.IoTEventsData.BatchUpdateDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state, variable values, and timer settings of one or more
-- detectors (instances) of a specified detector model.
module Amazonka.IoTEventsData.BatchUpdateDetector
  ( -- * Creating a Request
    BatchUpdateDetector (..),
    newBatchUpdateDetector,

    -- * Request Lenses
    batchUpdateDetector_detectors,

    -- * Destructuring the Response
    BatchUpdateDetectorResponse (..),
    newBatchUpdateDetectorResponse,

    -- * Response Lenses
    batchUpdateDetectorResponse_batchUpdateDetectorErrorEntries,
    batchUpdateDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateDetector' smart constructor.
data BatchUpdateDetector = BatchUpdateDetector'
  { -- | The list of detectors (instances) to update, along with the values to
    -- update.
    detectors :: Prelude.NonEmpty UpdateDetectorRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectors', 'batchUpdateDetector_detectors' - The list of detectors (instances) to update, along with the values to
-- update.
newBatchUpdateDetector ::
  -- | 'detectors'
  Prelude.NonEmpty UpdateDetectorRequest ->
  BatchUpdateDetector
newBatchUpdateDetector pDetectors_ =
  BatchUpdateDetector'
    { detectors =
        Lens.coerced Lens.# pDetectors_
    }

-- | The list of detectors (instances) to update, along with the values to
-- update.
batchUpdateDetector_detectors :: Lens.Lens' BatchUpdateDetector (Prelude.NonEmpty UpdateDetectorRequest)
batchUpdateDetector_detectors = Lens.lens (\BatchUpdateDetector' {detectors} -> detectors) (\s@BatchUpdateDetector' {} a -> s {detectors = a} :: BatchUpdateDetector) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpdateDetector where
  type
    AWSResponse BatchUpdateDetector =
      BatchUpdateDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateDetectorResponse'
            Prelude.<$> ( x
                            Data..?> "batchUpdateDetectorErrorEntries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdateDetector where
  hashWithSalt _salt BatchUpdateDetector' {..} =
    _salt `Prelude.hashWithSalt` detectors

instance Prelude.NFData BatchUpdateDetector where
  rnf BatchUpdateDetector' {..} = Prelude.rnf detectors

instance Data.ToHeaders BatchUpdateDetector where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchUpdateDetector where
  toJSON BatchUpdateDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("detectors" Data..= detectors)]
      )

instance Data.ToPath BatchUpdateDetector where
  toPath = Prelude.const "/detectors"

instance Data.ToQuery BatchUpdateDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateDetectorResponse' smart constructor.
data BatchUpdateDetectorResponse = BatchUpdateDetectorResponse'
  { -- | A list of those detector updates that resulted in errors. (If an error
    -- is listed here, the specific update did not occur.)
    batchUpdateDetectorErrorEntries :: Prelude.Maybe [BatchUpdateDetectorErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchUpdateDetectorErrorEntries', 'batchUpdateDetectorResponse_batchUpdateDetectorErrorEntries' - A list of those detector updates that resulted in errors. (If an error
-- is listed here, the specific update did not occur.)
--
-- 'httpStatus', 'batchUpdateDetectorResponse_httpStatus' - The response's http status code.
newBatchUpdateDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateDetectorResponse
newBatchUpdateDetectorResponse pHttpStatus_ =
  BatchUpdateDetectorResponse'
    { batchUpdateDetectorErrorEntries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of those detector updates that resulted in errors. (If an error
-- is listed here, the specific update did not occur.)
batchUpdateDetectorResponse_batchUpdateDetectorErrorEntries :: Lens.Lens' BatchUpdateDetectorResponse (Prelude.Maybe [BatchUpdateDetectorErrorEntry])
batchUpdateDetectorResponse_batchUpdateDetectorErrorEntries = Lens.lens (\BatchUpdateDetectorResponse' {batchUpdateDetectorErrorEntries} -> batchUpdateDetectorErrorEntries) (\s@BatchUpdateDetectorResponse' {} a -> s {batchUpdateDetectorErrorEntries = a} :: BatchUpdateDetectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateDetectorResponse_httpStatus :: Lens.Lens' BatchUpdateDetectorResponse Prelude.Int
batchUpdateDetectorResponse_httpStatus = Lens.lens (\BatchUpdateDetectorResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateDetectorResponse' {} a -> s {httpStatus = a} :: BatchUpdateDetectorResponse)

instance Prelude.NFData BatchUpdateDetectorResponse where
  rnf BatchUpdateDetectorResponse' {..} =
    Prelude.rnf batchUpdateDetectorErrorEntries
      `Prelude.seq` Prelude.rnf httpStatus
