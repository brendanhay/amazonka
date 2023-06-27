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
-- Module      : Amazonka.SSMSAP.Types.Host
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.Host where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.HostRole

-- | Describes the properties of the Dedicated Host.
--
-- /See:/ 'newHost' smart constructor.
data Host = Host'
  { -- | The IP address of the Dedicated Host.
    hostIp :: Prelude.Maybe Prelude.Text,
    -- | The name of the Dedicated Host.
    hostName :: Prelude.Maybe Prelude.Text,
    -- | The role of the Dedicated Host.
    hostRole :: Prelude.Maybe HostRole,
    -- | The instance ID of the instance on the Dedicated Host.
    instanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Host' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostIp', 'host_hostIp' - The IP address of the Dedicated Host.
--
-- 'hostName', 'host_hostName' - The name of the Dedicated Host.
--
-- 'hostRole', 'host_hostRole' - The role of the Dedicated Host.
--
-- 'instanceId', 'host_instanceId' - The instance ID of the instance on the Dedicated Host.
newHost ::
  Host
newHost =
  Host'
    { hostIp = Prelude.Nothing,
      hostName = Prelude.Nothing,
      hostRole = Prelude.Nothing,
      instanceId = Prelude.Nothing
    }

-- | The IP address of the Dedicated Host.
host_hostIp :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostIp = Lens.lens (\Host' {hostIp} -> hostIp) (\s@Host' {} a -> s {hostIp = a} :: Host)

-- | The name of the Dedicated Host.
host_hostName :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostName = Lens.lens (\Host' {hostName} -> hostName) (\s@Host' {} a -> s {hostName = a} :: Host)

-- | The role of the Dedicated Host.
host_hostRole :: Lens.Lens' Host (Prelude.Maybe HostRole)
host_hostRole = Lens.lens (\Host' {hostRole} -> hostRole) (\s@Host' {} a -> s {hostRole = a} :: Host)

-- | The instance ID of the instance on the Dedicated Host.
host_instanceId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_instanceId = Lens.lens (\Host' {instanceId} -> instanceId) (\s@Host' {} a -> s {instanceId = a} :: Host)

instance Data.FromJSON Host where
  parseJSON =
    Data.withObject
      "Host"
      ( \x ->
          Host'
            Prelude.<$> (x Data..:? "HostIp")
            Prelude.<*> (x Data..:? "HostName")
            Prelude.<*> (x Data..:? "HostRole")
            Prelude.<*> (x Data..:? "InstanceId")
      )

instance Prelude.Hashable Host where
  hashWithSalt _salt Host' {..} =
    _salt
      `Prelude.hashWithSalt` hostIp
      `Prelude.hashWithSalt` hostName
      `Prelude.hashWithSalt` hostRole
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData Host where
  rnf Host' {..} =
    Prelude.rnf hostIp
      `Prelude.seq` Prelude.rnf hostName
      `Prelude.seq` Prelude.rnf hostRole
      `Prelude.seq` Prelude.rnf instanceId
