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
-- Module      : Amazonka.DrS.Types.IdentificationHints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.IdentificationHints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Hints used to uniquely identify a machine.
--
-- /See:/ 'newIdentificationHints' smart constructor.
data IdentificationHints = IdentificationHints'
  { -- | AWS Instance ID identification hint.
    awsInstanceID :: Prelude.Maybe Prelude.Text,
    -- | Fully Qualified Domain Name identification hint.
    fqdn :: Prelude.Maybe Prelude.Text,
    -- | Hostname identification hint.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | vCenter VM path identification hint.
    vmWareUuid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentificationHints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsInstanceID', 'identificationHints_awsInstanceID' - AWS Instance ID identification hint.
--
-- 'fqdn', 'identificationHints_fqdn' - Fully Qualified Domain Name identification hint.
--
-- 'hostname', 'identificationHints_hostname' - Hostname identification hint.
--
-- 'vmWareUuid', 'identificationHints_vmWareUuid' - vCenter VM path identification hint.
newIdentificationHints ::
  IdentificationHints
newIdentificationHints =
  IdentificationHints'
    { awsInstanceID =
        Prelude.Nothing,
      fqdn = Prelude.Nothing,
      hostname = Prelude.Nothing,
      vmWareUuid = Prelude.Nothing
    }

-- | AWS Instance ID identification hint.
identificationHints_awsInstanceID :: Lens.Lens' IdentificationHints (Prelude.Maybe Prelude.Text)
identificationHints_awsInstanceID = Lens.lens (\IdentificationHints' {awsInstanceID} -> awsInstanceID) (\s@IdentificationHints' {} a -> s {awsInstanceID = a} :: IdentificationHints)

-- | Fully Qualified Domain Name identification hint.
identificationHints_fqdn :: Lens.Lens' IdentificationHints (Prelude.Maybe Prelude.Text)
identificationHints_fqdn = Lens.lens (\IdentificationHints' {fqdn} -> fqdn) (\s@IdentificationHints' {} a -> s {fqdn = a} :: IdentificationHints)

-- | Hostname identification hint.
identificationHints_hostname :: Lens.Lens' IdentificationHints (Prelude.Maybe Prelude.Text)
identificationHints_hostname = Lens.lens (\IdentificationHints' {hostname} -> hostname) (\s@IdentificationHints' {} a -> s {hostname = a} :: IdentificationHints)

-- | vCenter VM path identification hint.
identificationHints_vmWareUuid :: Lens.Lens' IdentificationHints (Prelude.Maybe Prelude.Text)
identificationHints_vmWareUuid = Lens.lens (\IdentificationHints' {vmWareUuid} -> vmWareUuid) (\s@IdentificationHints' {} a -> s {vmWareUuid = a} :: IdentificationHints)

instance Core.FromJSON IdentificationHints where
  parseJSON =
    Core.withObject
      "IdentificationHints"
      ( \x ->
          IdentificationHints'
            Prelude.<$> (x Core..:? "awsInstanceID")
            Prelude.<*> (x Core..:? "fqdn")
            Prelude.<*> (x Core..:? "hostname")
            Prelude.<*> (x Core..:? "vmWareUuid")
      )

instance Prelude.Hashable IdentificationHints where
  hashWithSalt _salt IdentificationHints' {..} =
    _salt `Prelude.hashWithSalt` awsInstanceID
      `Prelude.hashWithSalt` fqdn
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` vmWareUuid

instance Prelude.NFData IdentificationHints where
  rnf IdentificationHints' {..} =
    Prelude.rnf awsInstanceID
      `Prelude.seq` Prelude.rnf fqdn
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf vmWareUuid
