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
-- Module      : Amazonka.SageMaker.Types.MonitoringConstraintsResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringConstraintsResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The constraints resource for a monitoring job.
--
-- /See:/ 'newMonitoringConstraintsResource' smart constructor.
data MonitoringConstraintsResource = MonitoringConstraintsResource'
  { -- | The Amazon S3 URI for the constraints resource.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringConstraintsResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'monitoringConstraintsResource_s3Uri' - The Amazon S3 URI for the constraints resource.
newMonitoringConstraintsResource ::
  MonitoringConstraintsResource
newMonitoringConstraintsResource =
  MonitoringConstraintsResource'
    { s3Uri =
        Prelude.Nothing
    }

-- | The Amazon S3 URI for the constraints resource.
monitoringConstraintsResource_s3Uri :: Lens.Lens' MonitoringConstraintsResource (Prelude.Maybe Prelude.Text)
monitoringConstraintsResource_s3Uri = Lens.lens (\MonitoringConstraintsResource' {s3Uri} -> s3Uri) (\s@MonitoringConstraintsResource' {} a -> s {s3Uri = a} :: MonitoringConstraintsResource)

instance Data.FromJSON MonitoringConstraintsResource where
  parseJSON =
    Data.withObject
      "MonitoringConstraintsResource"
      ( \x ->
          MonitoringConstraintsResource'
            Prelude.<$> (x Data..:? "S3Uri")
      )

instance
  Prelude.Hashable
    MonitoringConstraintsResource
  where
  hashWithSalt _salt MonitoringConstraintsResource' {..} =
    _salt `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData MonitoringConstraintsResource where
  rnf MonitoringConstraintsResource' {..} =
    Prelude.rnf s3Uri

instance Data.ToJSON MonitoringConstraintsResource where
  toJSON MonitoringConstraintsResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Uri" Data..=) Prelude.<$> s3Uri]
      )
