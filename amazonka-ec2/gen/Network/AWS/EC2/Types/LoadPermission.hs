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
-- Module      : Network.AWS.EC2.Types.LoadPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a load permission.
--
-- /See:/ 'newLoadPermission' smart constructor.
data LoadPermission = LoadPermission'
  { -- | The name of the group.
    group' :: Prelude.Maybe PermissionGroup,
    -- | The AWS account ID.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LoadPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'loadPermission_group' - The name of the group.
--
-- 'userId', 'loadPermission_userId' - The AWS account ID.
newLoadPermission ::
  LoadPermission
newLoadPermission =
  LoadPermission'
    { group' = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The name of the group.
loadPermission_group :: Lens.Lens' LoadPermission (Prelude.Maybe PermissionGroup)
loadPermission_group = Lens.lens (\LoadPermission' {group'} -> group') (\s@LoadPermission' {} a -> s {group' = a} :: LoadPermission)

-- | The AWS account ID.
loadPermission_userId :: Lens.Lens' LoadPermission (Prelude.Maybe Prelude.Text)
loadPermission_userId = Lens.lens (\LoadPermission' {userId} -> userId) (\s@LoadPermission' {} a -> s {userId = a} :: LoadPermission)

instance Prelude.FromXML LoadPermission where
  parseXML x =
    LoadPermission'
      Prelude.<$> (x Prelude..@? "group")
      Prelude.<*> (x Prelude..@? "userId")

instance Prelude.Hashable LoadPermission

instance Prelude.NFData LoadPermission
