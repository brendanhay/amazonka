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
-- Module      : Network.AWS.EC2.Types.LoadPermissionRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a load permission.
--
-- /See:/ 'newLoadPermissionRequest' smart constructor.
data LoadPermissionRequest = LoadPermissionRequest'
  { -- | The name of the group.
    group' :: Prelude.Maybe PermissionGroup,
    -- | The AWS account ID.
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
-- 'userId', 'loadPermissionRequest_userId' - The AWS account ID.
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

-- | The AWS account ID.
loadPermissionRequest_userId :: Lens.Lens' LoadPermissionRequest (Prelude.Maybe Prelude.Text)
loadPermissionRequest_userId = Lens.lens (\LoadPermissionRequest' {userId} -> userId) (\s@LoadPermissionRequest' {} a -> s {userId = a} :: LoadPermissionRequest)

instance Prelude.Hashable LoadPermissionRequest

instance Prelude.NFData LoadPermissionRequest

instance Core.ToQuery LoadPermissionRequest where
  toQuery LoadPermissionRequest' {..} =
    Prelude.mconcat
      ["Group" Core.=: group', "UserId" Core.=: userId]
