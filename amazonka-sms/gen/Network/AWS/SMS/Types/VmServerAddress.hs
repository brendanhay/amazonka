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
-- Module      : Network.AWS.SMS.Types.VmServerAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VmServerAddress where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a VM server location.
--
-- /See:/ 'newVmServerAddress' smart constructor.
data VmServerAddress = VmServerAddress'
  { -- | The ID of the VM manager.
    vmManagerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VM.
    vmId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON VmServerAddress where
  parseJSON =
    Prelude.withObject
      "VmServerAddress"
      ( \x ->
          VmServerAddress'
            Prelude.<$> (x Prelude..:? "vmManagerId")
            Prelude.<*> (x Prelude..:? "vmId")
      )

instance Prelude.Hashable VmServerAddress

instance Prelude.NFData VmServerAddress

instance Prelude.ToJSON VmServerAddress where
  toJSON VmServerAddress' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("vmManagerId" Prelude..=) Prelude.<$> vmManagerId,
            ("vmId" Prelude..=) Prelude.<$> vmId
          ]
      )
