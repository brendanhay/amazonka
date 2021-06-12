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
-- Module      : Network.AWS.SMS.Types.ServerGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.Server

-- | Logical grouping of servers.
--
-- /See:/ 'newServerGroup' smart constructor.
data ServerGroup = ServerGroup'
  { -- | The ID of a server group.
    serverGroupId :: Core.Maybe Core.Text,
    -- | The name of a server group.
    name :: Core.Maybe Core.Text,
    -- | The servers that belong to a server group.
    serverList :: Core.Maybe [Server]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupId', 'serverGroup_serverGroupId' - The ID of a server group.
--
-- 'name', 'serverGroup_name' - The name of a server group.
--
-- 'serverList', 'serverGroup_serverList' - The servers that belong to a server group.
newServerGroup ::
  ServerGroup
newServerGroup =
  ServerGroup'
    { serverGroupId = Core.Nothing,
      name = Core.Nothing,
      serverList = Core.Nothing
    }

-- | The ID of a server group.
serverGroup_serverGroupId :: Lens.Lens' ServerGroup (Core.Maybe Core.Text)
serverGroup_serverGroupId = Lens.lens (\ServerGroup' {serverGroupId} -> serverGroupId) (\s@ServerGroup' {} a -> s {serverGroupId = a} :: ServerGroup)

-- | The name of a server group.
serverGroup_name :: Lens.Lens' ServerGroup (Core.Maybe Core.Text)
serverGroup_name = Lens.lens (\ServerGroup' {name} -> name) (\s@ServerGroup' {} a -> s {name = a} :: ServerGroup)

-- | The servers that belong to a server group.
serverGroup_serverList :: Lens.Lens' ServerGroup (Core.Maybe [Server])
serverGroup_serverList = Lens.lens (\ServerGroup' {serverList} -> serverList) (\s@ServerGroup' {} a -> s {serverList = a} :: ServerGroup) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ServerGroup where
  parseJSON =
    Core.withObject
      "ServerGroup"
      ( \x ->
          ServerGroup'
            Core.<$> (x Core..:? "serverGroupId")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "serverList" Core..!= Core.mempty)
      )

instance Core.Hashable ServerGroup

instance Core.NFData ServerGroup

instance Core.ToJSON ServerGroup where
  toJSON ServerGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serverGroupId" Core..=) Core.<$> serverGroupId,
            ("name" Core..=) Core.<$> name,
            ("serverList" Core..=) Core.<$> serverList
          ]
      )
