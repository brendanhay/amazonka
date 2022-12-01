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
-- Module      : Amazonka.LookoutMetrics.DescribeAnomalyDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a detector.
--
-- Amazon Lookout for Metrics API actions are eventually consistent. If you
-- do a read operation on a resource immediately after creating or
-- modifying it, use retries to allow time for the write operation to
-- complete.
module Amazonka.LookoutMetrics.DescribeAnomalyDetector
  ( -- * Creating a Request
    DescribeAnomalyDetector (..),
    newDescribeAnomalyDetector,

    -- * Request Lenses
    describeAnomalyDetector_anomalyDetectorArn,

    -- * Destructuring the Response
    DescribeAnomalyDetectorResponse (..),
    newDescribeAnomalyDetectorResponse,

    -- * Response Lenses
    describeAnomalyDetectorResponse_lastModificationTime,
    describeAnomalyDetectorResponse_anomalyDetectorArn,
    describeAnomalyDetectorResponse_failureType,
    describeAnomalyDetectorResponse_status,
    describeAnomalyDetectorResponse_kmsKeyArn,
    describeAnomalyDetectorResponse_anomalyDetectorName,
    describeAnomalyDetectorResponse_anomalyDetectorDescription,
    describeAnomalyDetectorResponse_creationTime,
    describeAnomalyDetectorResponse_anomalyDetectorConfig,
    describeAnomalyDetectorResponse_failureReason,
    describeAnomalyDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAnomalyDetector' smart constructor.
data DescribeAnomalyDetector = DescribeAnomalyDetector'
  { -- | The ARN of the detector to describe.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'describeAnomalyDetector_anomalyDetectorArn' - The ARN of the detector to describe.
newDescribeAnomalyDetector ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  DescribeAnomalyDetector
newDescribeAnomalyDetector pAnomalyDetectorArn_ =
  DescribeAnomalyDetector'
    { anomalyDetectorArn =
        pAnomalyDetectorArn_
    }

-- | The ARN of the detector to describe.
describeAnomalyDetector_anomalyDetectorArn :: Lens.Lens' DescribeAnomalyDetector Prelude.Text
describeAnomalyDetector_anomalyDetectorArn = Lens.lens (\DescribeAnomalyDetector' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@DescribeAnomalyDetector' {} a -> s {anomalyDetectorArn = a} :: DescribeAnomalyDetector)

instance Core.AWSRequest DescribeAnomalyDetector where
  type
    AWSResponse DescribeAnomalyDetector =
      DescribeAnomalyDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAnomalyDetectorResponse'
            Prelude.<$> (x Core..?> "LastModificationTime")
            Prelude.<*> (x Core..?> "AnomalyDetectorArn")
            Prelude.<*> (x Core..?> "FailureType")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "KmsKeyArn")
            Prelude.<*> (x Core..?> "AnomalyDetectorName")
            Prelude.<*> (x Core..?> "AnomalyDetectorDescription")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "AnomalyDetectorConfig")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAnomalyDetector where
  hashWithSalt _salt DescribeAnomalyDetector' {..} =
    _salt `Prelude.hashWithSalt` anomalyDetectorArn

instance Prelude.NFData DescribeAnomalyDetector where
  rnf DescribeAnomalyDetector' {..} =
    Prelude.rnf anomalyDetectorArn

instance Core.ToHeaders DescribeAnomalyDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAnomalyDetector where
  toJSON DescribeAnomalyDetector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AnomalyDetectorArn" Core..= anomalyDetectorArn)
          ]
      )

instance Core.ToPath DescribeAnomalyDetector where
  toPath = Prelude.const "/DescribeAnomalyDetector"

instance Core.ToQuery DescribeAnomalyDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAnomalyDetectorResponse' smart constructor.
data DescribeAnomalyDetectorResponse = DescribeAnomalyDetectorResponse'
  { -- | The time at which the detector was last modified.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the detector.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The process that caused the detector to fail.
    failureType :: Prelude.Maybe AnomalyDetectorFailureType,
    -- | The status of the detector.
    status :: Prelude.Maybe AnomalyDetectorStatus,
    -- | The ARN of the KMS key to use to encrypt your data.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector.
    anomalyDetectorName :: Prelude.Maybe Prelude.Text,
    -- | A description of the detector.
    anomalyDetectorDescription :: Prelude.Maybe Prelude.Text,
    -- | The time at which the detector was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Contains information about the detector\'s configuration.
    anomalyDetectorConfig :: Prelude.Maybe AnomalyDetectorConfigSummary,
    -- | The reason that the detector failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'describeAnomalyDetectorResponse_lastModificationTime' - The time at which the detector was last modified.
--
-- 'anomalyDetectorArn', 'describeAnomalyDetectorResponse_anomalyDetectorArn' - The ARN of the detector.
--
-- 'failureType', 'describeAnomalyDetectorResponse_failureType' - The process that caused the detector to fail.
--
-- 'status', 'describeAnomalyDetectorResponse_status' - The status of the detector.
--
-- 'kmsKeyArn', 'describeAnomalyDetectorResponse_kmsKeyArn' - The ARN of the KMS key to use to encrypt your data.
--
-- 'anomalyDetectorName', 'describeAnomalyDetectorResponse_anomalyDetectorName' - The name of the detector.
--
-- 'anomalyDetectorDescription', 'describeAnomalyDetectorResponse_anomalyDetectorDescription' - A description of the detector.
--
-- 'creationTime', 'describeAnomalyDetectorResponse_creationTime' - The time at which the detector was created.
--
-- 'anomalyDetectorConfig', 'describeAnomalyDetectorResponse_anomalyDetectorConfig' - Contains information about the detector\'s configuration.
--
-- 'failureReason', 'describeAnomalyDetectorResponse_failureReason' - The reason that the detector failed.
--
-- 'httpStatus', 'describeAnomalyDetectorResponse_httpStatus' - The response's http status code.
newDescribeAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAnomalyDetectorResponse
newDescribeAnomalyDetectorResponse pHttpStatus_ =
  DescribeAnomalyDetectorResponse'
    { lastModificationTime =
        Prelude.Nothing,
      anomalyDetectorArn = Prelude.Nothing,
      failureType = Prelude.Nothing,
      status = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      anomalyDetectorName = Prelude.Nothing,
      anomalyDetectorDescription =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      anomalyDetectorConfig = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the detector was last modified.
describeAnomalyDetectorResponse_lastModificationTime :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe Prelude.UTCTime)
describeAnomalyDetectorResponse_lastModificationTime = Lens.lens (\DescribeAnomalyDetectorResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeAnomalyDetectorResponse' {} a -> s {lastModificationTime = a} :: DescribeAnomalyDetectorResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of the detector.
describeAnomalyDetectorResponse_anomalyDetectorArn :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe Prelude.Text)
describeAnomalyDetectorResponse_anomalyDetectorArn = Lens.lens (\DescribeAnomalyDetectorResponse' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@DescribeAnomalyDetectorResponse' {} a -> s {anomalyDetectorArn = a} :: DescribeAnomalyDetectorResponse)

-- | The process that caused the detector to fail.
describeAnomalyDetectorResponse_failureType :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe AnomalyDetectorFailureType)
describeAnomalyDetectorResponse_failureType = Lens.lens (\DescribeAnomalyDetectorResponse' {failureType} -> failureType) (\s@DescribeAnomalyDetectorResponse' {} a -> s {failureType = a} :: DescribeAnomalyDetectorResponse)

-- | The status of the detector.
describeAnomalyDetectorResponse_status :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe AnomalyDetectorStatus)
describeAnomalyDetectorResponse_status = Lens.lens (\DescribeAnomalyDetectorResponse' {status} -> status) (\s@DescribeAnomalyDetectorResponse' {} a -> s {status = a} :: DescribeAnomalyDetectorResponse)

-- | The ARN of the KMS key to use to encrypt your data.
describeAnomalyDetectorResponse_kmsKeyArn :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe Prelude.Text)
describeAnomalyDetectorResponse_kmsKeyArn = Lens.lens (\DescribeAnomalyDetectorResponse' {kmsKeyArn} -> kmsKeyArn) (\s@DescribeAnomalyDetectorResponse' {} a -> s {kmsKeyArn = a} :: DescribeAnomalyDetectorResponse)

-- | The name of the detector.
describeAnomalyDetectorResponse_anomalyDetectorName :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe Prelude.Text)
describeAnomalyDetectorResponse_anomalyDetectorName = Lens.lens (\DescribeAnomalyDetectorResponse' {anomalyDetectorName} -> anomalyDetectorName) (\s@DescribeAnomalyDetectorResponse' {} a -> s {anomalyDetectorName = a} :: DescribeAnomalyDetectorResponse)

-- | A description of the detector.
describeAnomalyDetectorResponse_anomalyDetectorDescription :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe Prelude.Text)
describeAnomalyDetectorResponse_anomalyDetectorDescription = Lens.lens (\DescribeAnomalyDetectorResponse' {anomalyDetectorDescription} -> anomalyDetectorDescription) (\s@DescribeAnomalyDetectorResponse' {} a -> s {anomalyDetectorDescription = a} :: DescribeAnomalyDetectorResponse)

-- | The time at which the detector was created.
describeAnomalyDetectorResponse_creationTime :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe Prelude.UTCTime)
describeAnomalyDetectorResponse_creationTime = Lens.lens (\DescribeAnomalyDetectorResponse' {creationTime} -> creationTime) (\s@DescribeAnomalyDetectorResponse' {} a -> s {creationTime = a} :: DescribeAnomalyDetectorResponse) Prelude.. Lens.mapping Core._Time

-- | Contains information about the detector\'s configuration.
describeAnomalyDetectorResponse_anomalyDetectorConfig :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe AnomalyDetectorConfigSummary)
describeAnomalyDetectorResponse_anomalyDetectorConfig = Lens.lens (\DescribeAnomalyDetectorResponse' {anomalyDetectorConfig} -> anomalyDetectorConfig) (\s@DescribeAnomalyDetectorResponse' {} a -> s {anomalyDetectorConfig = a} :: DescribeAnomalyDetectorResponse)

-- | The reason that the detector failed.
describeAnomalyDetectorResponse_failureReason :: Lens.Lens' DescribeAnomalyDetectorResponse (Prelude.Maybe Prelude.Text)
describeAnomalyDetectorResponse_failureReason = Lens.lens (\DescribeAnomalyDetectorResponse' {failureReason} -> failureReason) (\s@DescribeAnomalyDetectorResponse' {} a -> s {failureReason = a} :: DescribeAnomalyDetectorResponse)

-- | The response's http status code.
describeAnomalyDetectorResponse_httpStatus :: Lens.Lens' DescribeAnomalyDetectorResponse Prelude.Int
describeAnomalyDetectorResponse_httpStatus = Lens.lens (\DescribeAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@DescribeAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: DescribeAnomalyDetectorResponse)

instance
  Prelude.NFData
    DescribeAnomalyDetectorResponse
  where
  rnf DescribeAnomalyDetectorResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf failureType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf anomalyDetectorName
      `Prelude.seq` Prelude.rnf anomalyDetectorDescription
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf anomalyDetectorConfig
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
