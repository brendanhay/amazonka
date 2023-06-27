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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.VmServerAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a VM server location.
--
-- /See:/ 'newVmServerAddress' smart constructor.
data VmServerAddress = VmServerAddress'
  { -- | The ID of the VM.
    vmId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VM manager.
    vmManagerId :: Prelude.Maybe Prelude.Text
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
-- 'vmId', 'vmServerAddress_vmId' - The ID of the VM.
--
-- 'vmManagerId', 'vmServerAddress_vmManagerId' - The ID of the VM manager.
newVmServerAddress ::
  VmServerAddress
newVmServerAddress =
  VmServerAddress'
    { vmId = Prelude.Nothing,
      vmManagerId = Prelude.Nothing
    }

-- | The ID of the VM.
vmServerAddress_vmId :: Lens.Lens' VmServerAddress (Prelude.Maybe Prelude.Text)
vmServerAddress_vmId = Lens.lens (\VmServerAddress' {vmId} -> vmId) (\s@VmServerAddress' {} a -> s {vmId = a} :: VmServerAddress)

-- | The ID of the VM manager.
vmServerAddress_vmManagerId :: Lens.Lens' VmServerAddress (Prelude.Maybe Prelude.Text)
vmServerAddress_vmManagerId = Lens.lens (\VmServerAddress' {vmManagerId} -> vmManagerId) (\s@VmServerAddress' {} a -> s {vmManagerId = a} :: VmServerAddress)

instance Data.FromJSON VmServerAddress where
  parseJSON =
    Data.withObject
      "VmServerAddress"
      ( \x ->
          VmServerAddress'
            Prelude.<$> (x Data..:? "vmId")
            Prelude.<*> (x Data..:? "vmManagerId")
      )

instance Prelude.Hashable VmServerAddress where
  hashWithSalt _salt VmServerAddress' {..} =
    _salt
      `Prelude.hashWithSalt` vmId
      `Prelude.hashWithSalt` vmManagerId

instance Prelude.NFData VmServerAddress where
  rnf VmServerAddress' {..} =
    Prelude.rnf vmId
      `Prelude.seq` Prelude.rnf vmManagerId

instance Data.ToJSON VmServerAddress where
  toJSON VmServerAddress' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("vmId" Data..=) Prelude.<$> vmId,
            ("vmManagerId" Data..=) Prelude.<$> vmManagerId
          ]
      )
