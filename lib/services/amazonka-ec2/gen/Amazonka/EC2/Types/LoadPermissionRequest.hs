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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The name of the group.
    group' :: Prelude.Maybe PermissionGroup,
    -- | The Amazon Web Services account ID.
    userId :: Prelude.Maybe Prelude.Text
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
-- 'group'', 'loadPermissionRequest_group' - The name of the group.
--
-- 'userId', 'loadPermissionRequest_userId' - The Amazon Web Services account ID.
newLoadPermissionRequest ::
  LoadPermissionRequest
newLoadPermissionRequest =
  LoadPermissionRequest'
    { group' = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The name of the group.
loadPermissionRequest_group :: Lens.Lens' LoadPermissionRequest (Prelude.Maybe PermissionGroup)
loadPermissionRequest_group = Lens.lens (\LoadPermissionRequest' {group'} -> group') (\s@LoadPermissionRequest' {} a -> s {group' = a} :: LoadPermissionRequest)

-- | The Amazon Web Services account ID.
loadPermissionRequest_userId :: Lens.Lens' LoadPermissionRequest (Prelude.Maybe Prelude.Text)
loadPermissionRequest_userId = Lens.lens (\LoadPermissionRequest' {userId} -> userId) (\s@LoadPermissionRequest' {} a -> s {userId = a} :: LoadPermissionRequest)

instance Prelude.Hashable LoadPermissionRequest where
  hashWithSalt _salt LoadPermissionRequest' {..} =
    _salt
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` userId

instance Prelude.NFData LoadPermissionRequest where
  rnf LoadPermissionRequest' {..} =
    Prelude.rnf group' `Prelude.seq` Prelude.rnf userId

instance Data.ToQuery LoadPermissionRequest where
  toQuery LoadPermissionRequest' {..} =
    Prelude.mconcat
      ["Group" Data.=: group', "UserId" Data.=: userId]
