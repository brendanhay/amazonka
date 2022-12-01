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
-- Module      : Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindingsResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindingsResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.EbsVolumesResult
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of whether Malware
-- Protection for EC2 instances with findings will be enabled as a data
-- source.
--
-- /See:/ 'newScanEc2InstanceWithFindingsResult' smart constructor.
data ScanEc2InstanceWithFindingsResult = ScanEc2InstanceWithFindingsResult'
  { -- | Describes the configuration of scanning EBS volumes as a data source.
    ebsVolumes :: Prelude.Maybe EbsVolumesResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanEc2InstanceWithFindingsResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsVolumes', 'scanEc2InstanceWithFindingsResult_ebsVolumes' - Describes the configuration of scanning EBS volumes as a data source.
newScanEc2InstanceWithFindingsResult ::
  ScanEc2InstanceWithFindingsResult
newScanEc2InstanceWithFindingsResult =
  ScanEc2InstanceWithFindingsResult'
    { ebsVolumes =
        Prelude.Nothing
    }

-- | Describes the configuration of scanning EBS volumes as a data source.
scanEc2InstanceWithFindingsResult_ebsVolumes :: Lens.Lens' ScanEc2InstanceWithFindingsResult (Prelude.Maybe EbsVolumesResult)
scanEc2InstanceWithFindingsResult_ebsVolumes = Lens.lens (\ScanEc2InstanceWithFindingsResult' {ebsVolumes} -> ebsVolumes) (\s@ScanEc2InstanceWithFindingsResult' {} a -> s {ebsVolumes = a} :: ScanEc2InstanceWithFindingsResult)

instance
  Core.FromJSON
    ScanEc2InstanceWithFindingsResult
  where
  parseJSON =
    Core.withObject
      "ScanEc2InstanceWithFindingsResult"
      ( \x ->
          ScanEc2InstanceWithFindingsResult'
            Prelude.<$> (x Core..:? "ebsVolumes")
      )

instance
  Prelude.Hashable
    ScanEc2InstanceWithFindingsResult
  where
  hashWithSalt
    _salt
    ScanEc2InstanceWithFindingsResult' {..} =
      _salt `Prelude.hashWithSalt` ebsVolumes

instance
  Prelude.NFData
    ScanEc2InstanceWithFindingsResult
  where
  rnf ScanEc2InstanceWithFindingsResult' {..} =
    Prelude.rnf ebsVolumes
