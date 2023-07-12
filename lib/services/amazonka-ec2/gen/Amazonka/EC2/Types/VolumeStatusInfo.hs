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
-- Module      : Amazonka.EC2.Types.VolumeStatusInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeStatusInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VolumeStatusDetails
import Amazonka.EC2.Types.VolumeStatusInfoStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a volume.
--
-- /See:/ 'newVolumeStatusInfo' smart constructor.
data VolumeStatusInfo = VolumeStatusInfo'
  { -- | The details of the volume status.
    details :: Prelude.Maybe [VolumeStatusDetails],
    -- | The status of the volume.
    status :: Prelude.Maybe VolumeStatusInfoStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'volumeStatusInfo_details' - The details of the volume status.
--
-- 'status', 'volumeStatusInfo_status' - The status of the volume.
newVolumeStatusInfo ::
  VolumeStatusInfo
newVolumeStatusInfo =
  VolumeStatusInfo'
    { details = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The details of the volume status.
volumeStatusInfo_details :: Lens.Lens' VolumeStatusInfo (Prelude.Maybe [VolumeStatusDetails])
volumeStatusInfo_details = Lens.lens (\VolumeStatusInfo' {details} -> details) (\s@VolumeStatusInfo' {} a -> s {details = a} :: VolumeStatusInfo) Prelude.. Lens.mapping Lens.coerced

-- | The status of the volume.
volumeStatusInfo_status :: Lens.Lens' VolumeStatusInfo (Prelude.Maybe VolumeStatusInfoStatus)
volumeStatusInfo_status = Lens.lens (\VolumeStatusInfo' {status} -> status) (\s@VolumeStatusInfo' {} a -> s {status = a} :: VolumeStatusInfo)

instance Data.FromXML VolumeStatusInfo where
  parseXML x =
    VolumeStatusInfo'
      Prelude.<$> ( x
                      Data..@? "details"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "status")

instance Prelude.Hashable VolumeStatusInfo where
  hashWithSalt _salt VolumeStatusInfo' {..} =
    _salt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` status

instance Prelude.NFData VolumeStatusInfo where
  rnf VolumeStatusInfo' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf status
