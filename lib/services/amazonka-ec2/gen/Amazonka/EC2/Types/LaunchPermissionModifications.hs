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
-- Module      : Amazonka.EC2.Types.LaunchPermissionModifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchPermissionModifications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchPermission
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch permission modification.
--
-- /See:/ 'newLaunchPermissionModifications' smart constructor.
data LaunchPermissionModifications = LaunchPermissionModifications'
  { -- | The Amazon Web Services account ID, organization ARN, or OU ARN to add
    -- to the list of launch permissions for the AMI.
    add :: Prelude.Maybe [LaunchPermission],
    -- | The Amazon Web Services account ID, organization ARN, or OU ARN to
    -- remove from the list of launch permissions for the AMI.
    remove :: Prelude.Maybe [LaunchPermission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchPermissionModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'add', 'launchPermissionModifications_add' - The Amazon Web Services account ID, organization ARN, or OU ARN to add
-- to the list of launch permissions for the AMI.
--
-- 'remove', 'launchPermissionModifications_remove' - The Amazon Web Services account ID, organization ARN, or OU ARN to
-- remove from the list of launch permissions for the AMI.
newLaunchPermissionModifications ::
  LaunchPermissionModifications
newLaunchPermissionModifications =
  LaunchPermissionModifications'
    { add =
        Prelude.Nothing,
      remove = Prelude.Nothing
    }

-- | The Amazon Web Services account ID, organization ARN, or OU ARN to add
-- to the list of launch permissions for the AMI.
launchPermissionModifications_add :: Lens.Lens' LaunchPermissionModifications (Prelude.Maybe [LaunchPermission])
launchPermissionModifications_add = Lens.lens (\LaunchPermissionModifications' {add} -> add) (\s@LaunchPermissionModifications' {} a -> s {add = a} :: LaunchPermissionModifications) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID, organization ARN, or OU ARN to
-- remove from the list of launch permissions for the AMI.
launchPermissionModifications_remove :: Lens.Lens' LaunchPermissionModifications (Prelude.Maybe [LaunchPermission])
launchPermissionModifications_remove = Lens.lens (\LaunchPermissionModifications' {remove} -> remove) (\s@LaunchPermissionModifications' {} a -> s {remove = a} :: LaunchPermissionModifications) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    LaunchPermissionModifications
  where
  hashWithSalt _salt LaunchPermissionModifications' {..} =
    _salt `Prelude.hashWithSalt` add
      `Prelude.hashWithSalt` remove

instance Prelude.NFData LaunchPermissionModifications where
  rnf LaunchPermissionModifications' {..} =
    Prelude.rnf add `Prelude.seq` Prelude.rnf remove

instance Data.ToQuery LaunchPermissionModifications where
  toQuery LaunchPermissionModifications' {..} =
    Prelude.mconcat
      [ Data.toQuery
          (Data.toQueryList "Add" Prelude.<$> add),
        Data.toQuery
          (Data.toQueryList "Remove" Prelude.<$> remove)
      ]
