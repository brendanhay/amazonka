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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.Server

-- | Logical grouping of servers.
--
-- /See:/ 'newServerGroup' smart constructor.
data ServerGroup = ServerGroup'
  { -- | The name of a server group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of a server group.
    serverGroupId :: Prelude.Maybe Prelude.Text,
    -- | The servers that belong to a server group.
    serverList :: Prelude.Maybe [Server]
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
-- 'name', 'serverGroup_name' - The name of a server group.
--
-- 'serverGroupId', 'serverGroup_serverGroupId' - The ID of a server group.
--
-- 'serverList', 'serverGroup_serverList' - The servers that belong to a server group.
newServerGroup ::
  ServerGroup
newServerGroup =
  ServerGroup'
    { name = Prelude.Nothing,
      serverGroupId = Prelude.Nothing,
      serverList = Prelude.Nothing
    }

-- | The name of a server group.
serverGroup_name :: Lens.Lens' ServerGroup (Prelude.Maybe Prelude.Text)
serverGroup_name = Lens.lens (\ServerGroup' {name} -> name) (\s@ServerGroup' {} a -> s {name = a} :: ServerGroup)

-- | The ID of a server group.
serverGroup_serverGroupId :: Lens.Lens' ServerGroup (Prelude.Maybe Prelude.Text)
serverGroup_serverGroupId = Lens.lens (\ServerGroup' {serverGroupId} -> serverGroupId) (\s@ServerGroup' {} a -> s {serverGroupId = a} :: ServerGroup)

-- | The servers that belong to a server group.
serverGroup_serverList :: Lens.Lens' ServerGroup (Prelude.Maybe [Server])
serverGroup_serverList = Lens.lens (\ServerGroup' {serverList} -> serverList) (\s@ServerGroup' {} a -> s {serverList = a} :: ServerGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ServerGroup where
  parseJSON =
    Data.withObject
      "ServerGroup"
      ( \x ->
          ServerGroup'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "serverGroupId")
            Prelude.<*> (x Data..:? "serverList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ServerGroup where
  hashWithSalt _salt ServerGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverGroupId
      `Prelude.hashWithSalt` serverList

instance Prelude.NFData ServerGroup where
  rnf ServerGroup' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverGroupId
      `Prelude.seq` Prelude.rnf serverList

instance Data.ToJSON ServerGroup where
  toJSON ServerGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("serverGroupId" Data..=) Prelude.<$> serverGroupId,
            ("serverList" Data..=) Prelude.<$> serverList
          ]
      )
