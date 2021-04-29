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
-- Module      : Network.AWS.SMS.Types.ServerGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.Server

-- | Logical grouping of servers.
--
-- /See:/ 'newServerGroup' smart constructor.
data ServerGroup = ServerGroup'
  { -- | The ID of a server group.
    serverGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of a server group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The servers that belong to a server group.
    serverList :: Prelude.Maybe [Server]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { serverGroupId = Prelude.Nothing,
      name = Prelude.Nothing,
      serverList = Prelude.Nothing
    }

-- | The ID of a server group.
serverGroup_serverGroupId :: Lens.Lens' ServerGroup (Prelude.Maybe Prelude.Text)
serverGroup_serverGroupId = Lens.lens (\ServerGroup' {serverGroupId} -> serverGroupId) (\s@ServerGroup' {} a -> s {serverGroupId = a} :: ServerGroup)

-- | The name of a server group.
serverGroup_name :: Lens.Lens' ServerGroup (Prelude.Maybe Prelude.Text)
serverGroup_name = Lens.lens (\ServerGroup' {name} -> name) (\s@ServerGroup' {} a -> s {name = a} :: ServerGroup)

-- | The servers that belong to a server group.
serverGroup_serverList :: Lens.Lens' ServerGroup (Prelude.Maybe [Server])
serverGroup_serverList = Lens.lens (\ServerGroup' {serverList} -> serverList) (\s@ServerGroup' {} a -> s {serverList = a} :: ServerGroup) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ServerGroup where
  parseJSON =
    Prelude.withObject
      "ServerGroup"
      ( \x ->
          ServerGroup'
            Prelude.<$> (x Prelude..:? "serverGroupId")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> ( x Prelude..:? "serverList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ServerGroup

instance Prelude.NFData ServerGroup

instance Prelude.ToJSON ServerGroup where
  toJSON ServerGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("serverGroupId" Prelude..=)
              Prelude.<$> serverGroupId,
            ("name" Prelude..=) Prelude.<$> name,
            ("serverList" Prelude..=) Prelude.<$> serverList
          ]
      )
