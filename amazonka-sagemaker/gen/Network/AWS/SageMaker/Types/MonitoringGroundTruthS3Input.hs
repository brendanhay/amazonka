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
-- Module      : Network.AWS.SageMaker.Types.MonitoringGroundTruthS3Input
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringGroundTruthS3Input where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The ground truth labels for the dataset used for the monitoring job.
--
-- /See:/ 'newMonitoringGroundTruthS3Input' smart constructor.
data MonitoringGroundTruthS3Input = MonitoringGroundTruthS3Input'
  { -- | The address of the Amazon S3 location of the ground truth labels.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MonitoringGroundTruthS3Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'monitoringGroundTruthS3Input_s3Uri' - The address of the Amazon S3 location of the ground truth labels.
newMonitoringGroundTruthS3Input ::
  MonitoringGroundTruthS3Input
newMonitoringGroundTruthS3Input =
  MonitoringGroundTruthS3Input'
    { s3Uri =
        Prelude.Nothing
    }

-- | The address of the Amazon S3 location of the ground truth labels.
monitoringGroundTruthS3Input_s3Uri :: Lens.Lens' MonitoringGroundTruthS3Input (Prelude.Maybe Prelude.Text)
monitoringGroundTruthS3Input_s3Uri = Lens.lens (\MonitoringGroundTruthS3Input' {s3Uri} -> s3Uri) (\s@MonitoringGroundTruthS3Input' {} a -> s {s3Uri = a} :: MonitoringGroundTruthS3Input)

instance
  Prelude.FromJSON
    MonitoringGroundTruthS3Input
  where
  parseJSON =
    Prelude.withObject
      "MonitoringGroundTruthS3Input"
      ( \x ->
          MonitoringGroundTruthS3Input'
            Prelude.<$> (x Prelude..:? "S3Uri")
      )

instance
  Prelude.Hashable
    MonitoringGroundTruthS3Input

instance Prelude.NFData MonitoringGroundTruthS3Input

instance Prelude.ToJSON MonitoringGroundTruthS3Input where
  toJSON MonitoringGroundTruthS3Input' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("S3Uri" Prelude..=) Prelude.<$> s3Uri]
      )
