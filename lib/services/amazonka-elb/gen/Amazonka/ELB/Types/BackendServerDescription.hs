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
-- Module      : Amazonka.ELB.Types.BackendServerDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.BackendServerDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the configuration of an EC2 instance.
--
-- /See:/ 'newBackendServerDescription' smart constructor.
data BackendServerDescription = BackendServerDescription'
  { -- | The names of the policies enabled for the EC2 instance.
    policyNames :: Prelude.Maybe [Prelude.Text],
    -- | The port on which the EC2 instance is listening.
    instancePort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendServerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyNames', 'backendServerDescription_policyNames' - The names of the policies enabled for the EC2 instance.
--
-- 'instancePort', 'backendServerDescription_instancePort' - The port on which the EC2 instance is listening.
newBackendServerDescription ::
  BackendServerDescription
newBackendServerDescription =
  BackendServerDescription'
    { policyNames =
        Prelude.Nothing,
      instancePort = Prelude.Nothing
    }

-- | The names of the policies enabled for the EC2 instance.
backendServerDescription_policyNames :: Lens.Lens' BackendServerDescription (Prelude.Maybe [Prelude.Text])
backendServerDescription_policyNames = Lens.lens (\BackendServerDescription' {policyNames} -> policyNames) (\s@BackendServerDescription' {} a -> s {policyNames = a} :: BackendServerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The port on which the EC2 instance is listening.
backendServerDescription_instancePort :: Lens.Lens' BackendServerDescription (Prelude.Maybe Prelude.Natural)
backendServerDescription_instancePort = Lens.lens (\BackendServerDescription' {instancePort} -> instancePort) (\s@BackendServerDescription' {} a -> s {instancePort = a} :: BackendServerDescription)

instance Data.FromXML BackendServerDescription where
  parseXML x =
    BackendServerDescription'
      Prelude.<$> ( x Data..@? "PolicyNames" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "InstancePort")

instance Prelude.Hashable BackendServerDescription where
  hashWithSalt _salt BackendServerDescription' {..} =
    _salt `Prelude.hashWithSalt` policyNames
      `Prelude.hashWithSalt` instancePort

instance Prelude.NFData BackendServerDescription where
  rnf BackendServerDescription' {..} =
    Prelude.rnf policyNames
      `Prelude.seq` Prelude.rnf instancePort
