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
-- Module      : Amazonka.EC2.Types.LoadPermissionRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LoadPermissionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PermissionGroup
import qualified Amazonka.Prelude as Prelude

-- | Describes a load permission.
--
-- /See:/ 'newLoadPermissionRequest' smart constructor.
data LoadPermissionRequest = LoadPermissionRequest'
  { -- | The Amazon Web Services account ID.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    group' :: Prelude.Maybe PermissionGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadPermissionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'loadPermissionRequest_userId' - The Amazon Web Services account ID.
--
-- 'group'', 'loadPermissionRequest_group' - The name of the group.
newLoadPermissionRequest ::
  LoadPermissionRequest
newLoadPermissionRequest =
  LoadPermissionRequest'
    { userId = Prelude.Nothing,
      group' = Prelude.Nothing
    }

-- | The Amazon Web Services account ID.
loadPermissionRequest_userId :: Lens.Lens' LoadPermissionRequest (Prelude.Maybe Prelude.Text)
loadPermissionRequest_userId = Lens.lens (\LoadPermissionRequest' {userId} -> userId) (\s@LoadPermissionRequest' {} a -> s {userId = a} :: LoadPermissionRequest)

-- | The name of the group.
loadPermissionRequest_group :: Lens.Lens' LoadPermissionRequest (Prelude.Maybe PermissionGroup)
loadPermissionRequest_group = Lens.lens (\LoadPermissionRequest' {group'} -> group') (\s@LoadPermissionRequest' {} a -> s {group' = a} :: LoadPermissionRequest)

instance Prelude.Hashable LoadPermissionRequest where
  hashWithSalt _salt LoadPermissionRequest' {..} =
    _salt `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` group'

instance Prelude.NFData LoadPermissionRequest where
  rnf LoadPermissionRequest' {..} =
    Prelude.rnf userId `Prelude.seq` Prelude.rnf group'

instance Data.ToQuery LoadPermissionRequest where
  toQuery LoadPermissionRequest' {..} =
    Prelude.mconcat
      ["UserId" Data.=: userId, "Group" Data.=: group']
