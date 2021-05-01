{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.MonitoringConstraintsResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringConstraintsResource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The constraints resource for a monitoring job.
--
-- /See:/ 'newMonitoringConstraintsResource' smart constructor.
data MonitoringConstraintsResource = MonitoringConstraintsResource'
  { -- | The Amazon S3 URI for the constraints resource.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromJSON
    MonitoringConstraintsResource
  where
  parseJSON =
    Prelude.withObject
      "MonitoringConstraintsResource"
      ( \x ->
          MonitoringConstraintsResource'
            Prelude.<$> (x Prelude..:? "S3Uri")
      )

instance
  Prelude.Hashable
    MonitoringConstraintsResource

instance Prelude.NFData MonitoringConstraintsResource

instance Prelude.ToJSON MonitoringConstraintsResource where
  toJSON MonitoringConstraintsResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("S3Uri" Prelude..=) Prelude.<$> s3Uri]
      )
