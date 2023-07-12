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
-- Module      : Amazonka.QuickSight.Types.LinkSharingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LinkSharingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResourcePermission

-- | A structure that contains the configuration of a shareable link to the
-- dashboard.
--
-- /See:/ 'newLinkSharingConfiguration' smart constructor.
data LinkSharingConfiguration = LinkSharingConfiguration'
  { -- | A structure that contains the permissions of a shareable link.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LinkSharingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'linkSharingConfiguration_permissions' - A structure that contains the permissions of a shareable link.
newLinkSharingConfiguration ::
  LinkSharingConfiguration
newLinkSharingConfiguration =
  LinkSharingConfiguration'
    { permissions =
        Prelude.Nothing
    }

-- | A structure that contains the permissions of a shareable link.
linkSharingConfiguration_permissions :: Lens.Lens' LinkSharingConfiguration (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
linkSharingConfiguration_permissions = Lens.lens (\LinkSharingConfiguration' {permissions} -> permissions) (\s@LinkSharingConfiguration' {} a -> s {permissions = a} :: LinkSharingConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LinkSharingConfiguration where
  parseJSON =
    Data.withObject
      "LinkSharingConfiguration"
      ( \x ->
          LinkSharingConfiguration'
            Prelude.<$> (x Data..:? "Permissions")
      )

instance Prelude.Hashable LinkSharingConfiguration where
  hashWithSalt _salt LinkSharingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` permissions

instance Prelude.NFData LinkSharingConfiguration where
  rnf LinkSharingConfiguration' {..} =
    Prelude.rnf permissions
