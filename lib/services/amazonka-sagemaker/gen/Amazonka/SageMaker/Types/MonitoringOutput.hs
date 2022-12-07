{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.MonitoringOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringS3Output

-- | The output object for a monitoring job.
--
-- /See:/ 'newMonitoringOutput' smart constructor.
data MonitoringOutput = MonitoringOutput'
  { -- | The Amazon S3 storage location where the results of a monitoring job are
    -- saved.
    s3Output :: MonitoringS3Output
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Output', 'monitoringOutput_s3Output' - The Amazon S3 storage location where the results of a monitoring job are
-- saved.
newMonitoringOutput ::
  -- | 's3Output'
  MonitoringS3Output ->
  MonitoringOutput
newMonitoringOutput pS3Output_ =
  MonitoringOutput' {s3Output = pS3Output_}

-- | The Amazon S3 storage location where the results of a monitoring job are
-- saved.
monitoringOutput_s3Output :: Lens.Lens' MonitoringOutput MonitoringS3Output
monitoringOutput_s3Output = Lens.lens (\MonitoringOutput' {s3Output} -> s3Output) (\s@MonitoringOutput' {} a -> s {s3Output = a} :: MonitoringOutput)

instance Data.FromJSON MonitoringOutput where
  parseJSON =
    Data.withObject
      "MonitoringOutput"
      ( \x ->
          MonitoringOutput' Prelude.<$> (x Data..: "S3Output")
      )

instance Prelude.Hashable MonitoringOutput where
  hashWithSalt _salt MonitoringOutput' {..} =
    _salt `Prelude.hashWithSalt` s3Output

instance Prelude.NFData MonitoringOutput where
  rnf MonitoringOutput' {..} = Prelude.rnf s3Output

instance Data.ToJSON MonitoringOutput where
  toJSON MonitoringOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Output" Data..= s3Output)]
      )
