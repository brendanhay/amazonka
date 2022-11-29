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
-- Module      : Amazonka.SMS.Types.VmServerAddress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.VmServerAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a VM server location.
--
-- /See:/ 'newVmServerAddress' smart constructor.
data VmServerAddress = VmServerAddress'
  { -- | The ID of the VM manager.
    vmManagerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VM.
    vmId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VmServerAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vmManagerId', 'vmServerAddress_vmManagerId' - The ID of the VM manager.
--
-- 'vmId', 'vmServerAddress_vmId' - The ID of the VM.
newVmServerAddress ::
  VmServerAddress
newVmServerAddress =
  VmServerAddress'
    { vmManagerId = Prelude.Nothing,
      vmId = Prelude.Nothing
    }

-- | The ID of the VM manager.
vmServerAddress_vmManagerId :: Lens.Lens' VmServerAddress (Prelude.Maybe Prelude.Text)
vmServerAddress_vmManagerId = Lens.lens (\VmServerAddress' {vmManagerId} -> vmManagerId) (\s@VmServerAddress' {} a -> s {vmManagerId = a} :: VmServerAddress)

-- | The ID of the VM.
vmServerAddress_vmId :: Lens.Lens' VmServerAddress (Prelude.Maybe Prelude.Text)
vmServerAddress_vmId = Lens.lens (\VmServerAddress' {vmId} -> vmId) (\s@VmServerAddress' {} a -> s {vmId = a} :: VmServerAddress)

instance Core.FromJSON VmServerAddress where
  parseJSON =
    Core.withObject
      "VmServerAddress"
      ( \x ->
          VmServerAddress'
            Prelude.<$> (x Core..:? "vmManagerId")
            Prelude.<*> (x Core..:? "vmId")
      )

instance Prelude.Hashable VmServerAddress where
  hashWithSalt _salt VmServerAddress' {..} =
    _salt `Prelude.hashWithSalt` vmManagerId
      `Prelude.hashWithSalt` vmId

instance Prelude.NFData VmServerAddress where
  rnf VmServerAddress' {..} =
    Prelude.rnf vmManagerId
      `Prelude.seq` Prelude.rnf vmId

instance Core.ToJSON VmServerAddress where
  toJSON VmServerAddress' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("vmManagerId" Core..=) Prelude.<$> vmManagerId,
            ("vmId" Core..=) Prelude.<$> vmId
          ]
      )
