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

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the configuration of an EC2 instance.
--
-- /See:/ 'newBackendServerDescription' smart constructor.
data BackendServerDescription = BackendServerDescription'
  { -- | The port on which the EC2 instance is listening.
    instancePort :: Core.Maybe Core.Natural,
    -- | The names of the policies enabled for the EC2 instance.
    policyNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      policyNames = Core.Nothing
    }

-- | The port on which the EC2 instance is listening.
backendServerDescription_instancePort :: Lens.Lens' BackendServerDescription (Core.Maybe Core.Natural)
backendServerDescription_instancePort = Lens.lens (\BackendServerDescription' {instancePort} -> instancePort) (\s@BackendServerDescription' {} a -> s {instancePort = a} :: BackendServerDescription)

-- | The names of the policies enabled for the EC2 instance.
backendServerDescription_policyNames :: Lens.Lens' BackendServerDescription (Core.Maybe [Core.Text])
backendServerDescription_policyNames = Lens.lens (\BackendServerDescription' {policyNames} -> policyNames) (\s@BackendServerDescription' {} a -> s {policyNames = a} :: BackendServerDescription) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML BackendServerDescription where
  parseXML x =
    BackendServerDescription'
      Core.<$> (x Core..@? "InstancePort")
      Core.<*> ( x Core..@? "PolicyNames" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable BackendServerDescription

instance Core.NFData BackendServerDescription
