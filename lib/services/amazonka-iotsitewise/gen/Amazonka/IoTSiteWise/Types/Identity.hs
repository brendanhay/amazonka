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
-- Module      : Amazonka.IoTSiteWise.Types.Identity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Identity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.GroupIdentity
import Amazonka.IoTSiteWise.Types.IAMRoleIdentity
import Amazonka.IoTSiteWise.Types.IAMUserIdentity
import Amazonka.IoTSiteWise.Types.UserIdentity
import qualified Amazonka.Prelude as Prelude

-- | Contains an identity that can access an IoT SiteWise Monitor resource.
--
-- Currently, you can\'t use Amazon Web Services APIs to retrieve IAM
-- Identity Center identity IDs. You can find the IAM Identity Center
-- identity IDs in the URL of user and group pages in the
-- <https://console.aws.amazon.com/singlesignon IAM Identity Center console>.
--
-- /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | An IAM Identity Center group identity.
    group' :: Prelude.Maybe GroupIdentity,
    -- | An IAM role identity.
    iamRole :: Prelude.Maybe IAMRoleIdentity,
    -- | An IAM user identity.
    iamUser :: Prelude.Maybe IAMUserIdentity,
    -- | An IAM Identity Center user identity.
    user :: Prelude.Maybe UserIdentity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Identity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'identity_group' - An IAM Identity Center group identity.
--
-- 'iamRole', 'identity_iamRole' - An IAM role identity.
--
-- 'iamUser', 'identity_iamUser' - An IAM user identity.
--
-- 'user', 'identity_user' - An IAM Identity Center user identity.
newIdentity ::
  Identity
newIdentity =
  Identity'
    { group' = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      iamUser = Prelude.Nothing,
      user = Prelude.Nothing
    }

-- | An IAM Identity Center group identity.
identity_group :: Lens.Lens' Identity (Prelude.Maybe GroupIdentity)
identity_group = Lens.lens (\Identity' {group'} -> group') (\s@Identity' {} a -> s {group' = a} :: Identity)

-- | An IAM role identity.
identity_iamRole :: Lens.Lens' Identity (Prelude.Maybe IAMRoleIdentity)
identity_iamRole = Lens.lens (\Identity' {iamRole} -> iamRole) (\s@Identity' {} a -> s {iamRole = a} :: Identity)

-- | An IAM user identity.
identity_iamUser :: Lens.Lens' Identity (Prelude.Maybe IAMUserIdentity)
identity_iamUser = Lens.lens (\Identity' {iamUser} -> iamUser) (\s@Identity' {} a -> s {iamUser = a} :: Identity)

-- | An IAM Identity Center user identity.
identity_user :: Lens.Lens' Identity (Prelude.Maybe UserIdentity)
identity_user = Lens.lens (\Identity' {user} -> user) (\s@Identity' {} a -> s {user = a} :: Identity)

instance Data.FromJSON Identity where
  parseJSON =
    Data.withObject
      "Identity"
      ( \x ->
          Identity'
            Prelude.<$> (x Data..:? "group")
            Prelude.<*> (x Data..:? "iamRole")
            Prelude.<*> (x Data..:? "iamUser")
            Prelude.<*> (x Data..:? "user")
      )

instance Prelude.Hashable Identity where
  hashWithSalt _salt Identity' {..} =
    _salt
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` iamUser
      `Prelude.hashWithSalt` user

instance Prelude.NFData Identity where
  rnf Identity' {..} =
    Prelude.rnf group'
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf iamUser
      `Prelude.seq` Prelude.rnf user

instance Data.ToJSON Identity where
  toJSON Identity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("group" Data..=) Prelude.<$> group',
            ("iamRole" Data..=) Prelude.<$> iamRole,
            ("iamUser" Data..=) Prelude.<$> iamUser,
            ("user" Data..=) Prelude.<$> user
          ]
      )
