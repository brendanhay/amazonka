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
-- Module      : Amazonka.IoTEventsData.DescribeDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified detector (instance).
module Amazonka.IoTEventsData.DescribeDetector
  ( -- * Creating a Request
    DescribeDetector (..),
    newDescribeDetector,

    -- * Request Lenses
    describeDetector_keyValue,
    describeDetector_detectorModelName,

    -- * Destructuring the Response
    DescribeDetectorResponse (..),
    newDescribeDetectorResponse,

    -- * Response Lenses
    describeDetectorResponse_detector,
    describeDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDetector' smart constructor.
data DescribeDetector = DescribeDetector'
  { -- | A filter used to limit results to detectors (instances) created because
    -- of the given key ID.
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector model whose detectors (instances) you want
    -- information about.
    detectorModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'describeDetector_keyValue' - A filter used to limit results to detectors (instances) created because
-- of the given key ID.
--
-- 'detectorModelName', 'describeDetector_detectorModelName' - The name of the detector model whose detectors (instances) you want
-- information about.
newDescribeDetector ::
  -- | 'detectorModelName'
  Prelude.Text ->
  DescribeDetector
newDescribeDetector pDetectorModelName_ =
  DescribeDetector'
    { keyValue = Prelude.Nothing,
      detectorModelName = pDetectorModelName_
    }

-- | A filter used to limit results to detectors (instances) created because
-- of the given key ID.
describeDetector_keyValue :: Lens.Lens' DescribeDetector (Prelude.Maybe Prelude.Text)
describeDetector_keyValue = Lens.lens (\DescribeDetector' {keyValue} -> keyValue) (\s@DescribeDetector' {} a -> s {keyValue = a} :: DescribeDetector)

-- | The name of the detector model whose detectors (instances) you want
-- information about.
describeDetector_detectorModelName :: Lens.Lens' DescribeDetector Prelude.Text
describeDetector_detectorModelName = Lens.lens (\DescribeDetector' {detectorModelName} -> detectorModelName) (\s@DescribeDetector' {} a -> s {detectorModelName = a} :: DescribeDetector)

instance Core.AWSRequest DescribeDetector where
  type
    AWSResponse DescribeDetector =
      DescribeDetectorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDetectorResponse'
            Prelude.<$> (x Core..?> "detector")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDetector where
  hashWithSalt _salt DescribeDetector' {..} =
    _salt `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` detectorModelName

instance Prelude.NFData DescribeDetector where
  rnf DescribeDetector' {..} =
    Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf detectorModelName

instance Core.ToHeaders DescribeDetector where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDetector where
  toPath DescribeDetector' {..} =
    Prelude.mconcat
      [ "/detectors/",
        Core.toBS detectorModelName,
        "/keyValues/"
      ]

instance Core.ToQuery DescribeDetector where
  toQuery DescribeDetector' {..} =
    Prelude.mconcat ["keyValue" Core.=: keyValue]

-- | /See:/ 'newDescribeDetectorResponse' smart constructor.
data DescribeDetectorResponse = DescribeDetectorResponse'
  { -- | Information about the detector (instance).
    detector :: Prelude.Maybe Detector,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detector', 'describeDetectorResponse_detector' - Information about the detector (instance).
--
-- 'httpStatus', 'describeDetectorResponse_httpStatus' - The response's http status code.
newDescribeDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDetectorResponse
newDescribeDetectorResponse pHttpStatus_ =
  DescribeDetectorResponse'
    { detector =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the detector (instance).
describeDetectorResponse_detector :: Lens.Lens' DescribeDetectorResponse (Prelude.Maybe Detector)
describeDetectorResponse_detector = Lens.lens (\DescribeDetectorResponse' {detector} -> detector) (\s@DescribeDetectorResponse' {} a -> s {detector = a} :: DescribeDetectorResponse)

-- | The response's http status code.
describeDetectorResponse_httpStatus :: Lens.Lens' DescribeDetectorResponse Prelude.Int
describeDetectorResponse_httpStatus = Lens.lens (\DescribeDetectorResponse' {httpStatus} -> httpStatus) (\s@DescribeDetectorResponse' {} a -> s {httpStatus = a} :: DescribeDetectorResponse)

instance Prelude.NFData DescribeDetectorResponse where
  rnf DescribeDetectorResponse' {..} =
    Prelude.rnf detector
      `Prelude.seq` Prelude.rnf httpStatus
