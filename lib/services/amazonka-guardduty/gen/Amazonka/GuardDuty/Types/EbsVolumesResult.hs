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
-- Module      : Amazonka.GuardDuty.Types.EbsVolumesResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.EbsVolumesResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourceStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of scanning EBS volumes as a data source.
--
-- /See:/ 'newEbsVolumesResult' smart constructor.
data EbsVolumesResult = EbsVolumesResult'
  { -- | Specifies the reason why scanning EBS volumes (Malware Protection) was
    -- not enabled as a data source.
    reason :: Prelude.Maybe Prelude.Text,
    -- | Describes whether scanning EBS volumes is enabled as a data source.
    status :: Prelude.Maybe DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsVolumesResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'ebsVolumesResult_reason' - Specifies the reason why scanning EBS volumes (Malware Protection) was
-- not enabled as a data source.
--
-- 'status', 'ebsVolumesResult_status' - Describes whether scanning EBS volumes is enabled as a data source.
newEbsVolumesResult ::
  EbsVolumesResult
newEbsVolumesResult =
  EbsVolumesResult'
    { reason = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the reason why scanning EBS volumes (Malware Protection) was
-- not enabled as a data source.
ebsVolumesResult_reason :: Lens.Lens' EbsVolumesResult (Prelude.Maybe Prelude.Text)
ebsVolumesResult_reason = Lens.lens (\EbsVolumesResult' {reason} -> reason) (\s@EbsVolumesResult' {} a -> s {reason = a} :: EbsVolumesResult)

-- | Describes whether scanning EBS volumes is enabled as a data source.
ebsVolumesResult_status :: Lens.Lens' EbsVolumesResult (Prelude.Maybe DataSourceStatus)
ebsVolumesResult_status = Lens.lens (\EbsVolumesResult' {status} -> status) (\s@EbsVolumesResult' {} a -> s {status = a} :: EbsVolumesResult)

instance Data.FromJSON EbsVolumesResult where
  parseJSON =
    Data.withObject
      "EbsVolumesResult"
      ( \x ->
          EbsVolumesResult'
            Prelude.<$> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable EbsVolumesResult where
  hashWithSalt _salt EbsVolumesResult' {..} =
    _salt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData EbsVolumesResult where
  rnf EbsVolumesResult' {..} =
    Prelude.rnf reason `Prelude.seq` Prelude.rnf status
