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
-- Copyright   : (c) 2013-2022 Brendan Hay
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

-- |
--
-- /See:/ 'newHost' smart constructor.
data Host = Host'
  { hostRole :: Prelude.Maybe HostRole,
    hostIp :: Prelude.Maybe Prelude.Text,
    hostName :: Prelude.Maybe Prelude.Text,
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
-- 'hostRole', 'host_hostRole' -
--
-- 'hostIp', 'host_hostIp' -
--
-- 'hostName', 'host_hostName' -
--
-- 'instanceId', 'host_instanceId' -
newHost ::
  Host
newHost =
  Host'
    { hostRole = Prelude.Nothing,
      hostIp = Prelude.Nothing,
      hostName = Prelude.Nothing,
      instanceId = Prelude.Nothing
    }

-- |
host_hostRole :: Lens.Lens' Host (Prelude.Maybe HostRole)
host_hostRole = Lens.lens (\Host' {hostRole} -> hostRole) (\s@Host' {} a -> s {hostRole = a} :: Host)

-- |
host_hostIp :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostIp = Lens.lens (\Host' {hostIp} -> hostIp) (\s@Host' {} a -> s {hostIp = a} :: Host)

-- |
host_hostName :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostName = Lens.lens (\Host' {hostName} -> hostName) (\s@Host' {} a -> s {hostName = a} :: Host)

-- |
host_instanceId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_instanceId = Lens.lens (\Host' {instanceId} -> instanceId) (\s@Host' {} a -> s {instanceId = a} :: Host)

instance Data.FromJSON Host where
  parseJSON =
    Data.withObject
      "Host"
      ( \x ->
          Host'
            Prelude.<$> (x Data..:? "HostRole")
            Prelude.<*> (x Data..:? "HostIp")
            Prelude.<*> (x Data..:? "HostName")
            Prelude.<*> (x Data..:? "InstanceId")
      )

instance Prelude.Hashable Host where
  hashWithSalt _salt Host' {..} =
    _salt `Prelude.hashWithSalt` hostRole
      `Prelude.hashWithSalt` hostIp
      `Prelude.hashWithSalt` hostName
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData Host where
  rnf Host' {..} =
    Prelude.rnf hostRole
      `Prelude.seq` Prelude.rnf hostIp
      `Prelude.seq` Prelude.rnf hostName
      `Prelude.seq` Prelude.rnf instanceId
