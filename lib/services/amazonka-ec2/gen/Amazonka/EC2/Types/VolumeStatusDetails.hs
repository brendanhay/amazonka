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
-- Module      : Amazonka.EC2.Types.VolumeStatusDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeStatusDetails where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VolumeStatusName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a volume status.
--
-- /See:/ 'newVolumeStatusDetails' smart constructor.
data VolumeStatusDetails = VolumeStatusDetails'
  { -- | The intended status of the volume status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the volume status.
    name :: Prelude.Maybe VolumeStatusName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'volumeStatusDetails_status' - The intended status of the volume status.
--
-- 'name', 'volumeStatusDetails_name' - The name of the volume status.
newVolumeStatusDetails ::
  VolumeStatusDetails
newVolumeStatusDetails =
  VolumeStatusDetails'
    { status = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The intended status of the volume status.
volumeStatusDetails_status :: Lens.Lens' VolumeStatusDetails (Prelude.Maybe Prelude.Text)
volumeStatusDetails_status = Lens.lens (\VolumeStatusDetails' {status} -> status) (\s@VolumeStatusDetails' {} a -> s {status = a} :: VolumeStatusDetails)

-- | The name of the volume status.
volumeStatusDetails_name :: Lens.Lens' VolumeStatusDetails (Prelude.Maybe VolumeStatusName)
volumeStatusDetails_name = Lens.lens (\VolumeStatusDetails' {name} -> name) (\s@VolumeStatusDetails' {} a -> s {name = a} :: VolumeStatusDetails)

instance Core.FromXML VolumeStatusDetails where
  parseXML x =
    VolumeStatusDetails'
      Prelude.<$> (x Core..@? "status") Prelude.<*> (x Core..@? "name")

instance Prelude.Hashable VolumeStatusDetails where
  hashWithSalt _salt VolumeStatusDetails' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` name

instance Prelude.NFData VolumeStatusDetails where
  rnf VolumeStatusDetails' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf name
