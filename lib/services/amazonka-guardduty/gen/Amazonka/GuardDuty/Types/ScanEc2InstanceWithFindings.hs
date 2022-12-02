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
-- Module      : Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes whether Malware Protection for EC2 instances with findings
-- will be enabled as a data source.
--
-- /See:/ 'newScanEc2InstanceWithFindings' smart constructor.
data ScanEc2InstanceWithFindings = ScanEc2InstanceWithFindings'
  { -- | Describes the configuration for scanning EBS volumes as data source.
    ebsVolumes :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanEc2InstanceWithFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsVolumes', 'scanEc2InstanceWithFindings_ebsVolumes' - Describes the configuration for scanning EBS volumes as data source.
newScanEc2InstanceWithFindings ::
  ScanEc2InstanceWithFindings
newScanEc2InstanceWithFindings =
  ScanEc2InstanceWithFindings'
    { ebsVolumes =
        Prelude.Nothing
    }

-- | Describes the configuration for scanning EBS volumes as data source.
scanEc2InstanceWithFindings_ebsVolumes :: Lens.Lens' ScanEc2InstanceWithFindings (Prelude.Maybe Prelude.Bool)
scanEc2InstanceWithFindings_ebsVolumes = Lens.lens (\ScanEc2InstanceWithFindings' {ebsVolumes} -> ebsVolumes) (\s@ScanEc2InstanceWithFindings' {} a -> s {ebsVolumes = a} :: ScanEc2InstanceWithFindings)

instance Prelude.Hashable ScanEc2InstanceWithFindings where
  hashWithSalt _salt ScanEc2InstanceWithFindings' {..} =
    _salt `Prelude.hashWithSalt` ebsVolumes

instance Prelude.NFData ScanEc2InstanceWithFindings where
  rnf ScanEc2InstanceWithFindings' {..} =
    Prelude.rnf ebsVolumes

instance Data.ToJSON ScanEc2InstanceWithFindings where
  toJSON ScanEc2InstanceWithFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ebsVolumes" Data..=) Prelude.<$> ebsVolumes]
      )
