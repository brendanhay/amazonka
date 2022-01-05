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
-- Module      : Amazonka.SMS.Types.ServerGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.Server

-- | Logical grouping of servers.
--
-- /See:/ 'newServerGroup' smart constructor.
data ServerGroup = ServerGroup'
  { -- | The servers that belong to a server group.
    serverList :: Prelude.Maybe [Server],
    -- | The name of a server group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of a server group.
    serverGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverList', 'serverGroup_serverList' - The servers that belong to a server group.
--
-- 'name', 'serverGroup_name' - The name of a server group.
--
-- 'serverGroupId', 'serverGroup_serverGroupId' - The ID of a server group.
newServerGroup ::
  ServerGroup
newServerGroup =
  ServerGroup'
    { serverList = Prelude.Nothing,
      name = Prelude.Nothing,
      serverGroupId = Prelude.Nothing
    }

-- | The servers that belong to a server group.
serverGroup_serverList :: Lens.Lens' ServerGroup (Prelude.Maybe [Server])
serverGroup_serverList = Lens.lens (\ServerGroup' {serverList} -> serverList) (\s@ServerGroup' {} a -> s {serverList = a} :: ServerGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of a server group.
serverGroup_name :: Lens.Lens' ServerGroup (Prelude.Maybe Prelude.Text)
serverGroup_name = Lens.lens (\ServerGroup' {name} -> name) (\s@ServerGroup' {} a -> s {name = a} :: ServerGroup)

-- | The ID of a server group.
serverGroup_serverGroupId :: Lens.Lens' ServerGroup (Prelude.Maybe Prelude.Text)
serverGroup_serverGroupId = Lens.lens (\ServerGroup' {serverGroupId} -> serverGroupId) (\s@ServerGroup' {} a -> s {serverGroupId = a} :: ServerGroup)

instance Core.FromJSON ServerGroup where
  parseJSON =
    Core.withObject
      "ServerGroup"
      ( \x ->
          ServerGroup'
            Prelude.<$> (x Core..:? "serverList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "serverGroupId")
      )

instance Prelude.Hashable ServerGroup where
  hashWithSalt _salt ServerGroup' {..} =
    _salt `Prelude.hashWithSalt` serverList
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverGroupId

instance Prelude.NFData ServerGroup where
  rnf ServerGroup' {..} =
    Prelude.rnf serverList
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverGroupId

instance Core.ToJSON ServerGroup where
  toJSON ServerGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("serverList" Core..=) Prelude.<$> serverList,
            ("name" Core..=) Prelude.<$> name,
            ("serverGroupId" Core..=) Prelude.<$> serverGroupId
          ]
      )
