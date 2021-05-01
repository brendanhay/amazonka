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
-- Module      : Network.AWS.ELB.Types.BackendServerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.BackendServerDescription where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the configuration of an EC2 instance.
--
-- /See:/ 'newBackendServerDescription' smart constructor.
data BackendServerDescription = BackendServerDescription'
  { -- | The port on which the EC2 instance is listening.
    instancePort :: Prelude.Maybe Prelude.Natural,
    -- | The names of the policies enabled for the EC2 instance.
    policyNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BackendServerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancePort', 'backendServerDescription_instancePort' - The port on which the EC2 instance is listening.
--
-- 'policyNames', 'backendServerDescription_policyNames' - The names of the policies enabled for the EC2 instance.
newBackendServerDescription ::
  BackendServerDescription
newBackendServerDescription =
  BackendServerDescription'
    { instancePort =
        Prelude.Nothing,
      policyNames = Prelude.Nothing
    }

-- | The port on which the EC2 instance is listening.
backendServerDescription_instancePort :: Lens.Lens' BackendServerDescription (Prelude.Maybe Prelude.Natural)
backendServerDescription_instancePort = Lens.lens (\BackendServerDescription' {instancePort} -> instancePort) (\s@BackendServerDescription' {} a -> s {instancePort = a} :: BackendServerDescription)

-- | The names of the policies enabled for the EC2 instance.
backendServerDescription_policyNames :: Lens.Lens' BackendServerDescription (Prelude.Maybe [Prelude.Text])
backendServerDescription_policyNames = Lens.lens (\BackendServerDescription' {policyNames} -> policyNames) (\s@BackendServerDescription' {} a -> s {policyNames = a} :: BackendServerDescription) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML BackendServerDescription where
  parseXML x =
    BackendServerDescription'
      Prelude.<$> (x Prelude..@? "InstancePort")
      Prelude.<*> ( x Prelude..@? "PolicyNames"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable BackendServerDescription

instance Prelude.NFData BackendServerDescription
