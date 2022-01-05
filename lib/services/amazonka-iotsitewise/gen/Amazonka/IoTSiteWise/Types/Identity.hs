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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Identity where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types.GroupIdentity
import Amazonka.IoTSiteWise.Types.IAMRoleIdentity
import Amazonka.IoTSiteWise.Types.IAMUserIdentity
import Amazonka.IoTSiteWise.Types.UserIdentity
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an identity that can access an IoT SiteWise Monitor resource.
--
-- Currently, you can\'t use Amazon Web Services APIs to retrieve Amazon
-- Web Services SSO identity IDs. You can find the Amazon Web Services SSO
-- identity IDs in the URL of user and group pages in the
-- <https://console.aws.amazon.com/singlesignon Amazon Web Services SSO console>.
--
-- /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | An IAM user identity.
    iamUser :: Prelude.Maybe IAMUserIdentity,
    -- | An Amazon Web Services SSO group identity.
    group' :: Prelude.Maybe GroupIdentity,
    -- | An Amazon Web Services SSO user identity.
    user :: Prelude.Maybe UserIdentity,
    -- | An IAM role identity.
    iamRole :: Prelude.Maybe IAMRoleIdentity
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
-- 'iamUser', 'identity_iamUser' - An IAM user identity.
--
-- 'group'', 'identity_group' - An Amazon Web Services SSO group identity.
--
-- 'user', 'identity_user' - An Amazon Web Services SSO user identity.
--
-- 'iamRole', 'identity_iamRole' - An IAM role identity.
newIdentity ::
  Identity
newIdentity =
  Identity'
    { iamUser = Prelude.Nothing,
      group' = Prelude.Nothing,
      user = Prelude.Nothing,
      iamRole = Prelude.Nothing
    }

-- | An IAM user identity.
identity_iamUser :: Lens.Lens' Identity (Prelude.Maybe IAMUserIdentity)
identity_iamUser = Lens.lens (\Identity' {iamUser} -> iamUser) (\s@Identity' {} a -> s {iamUser = a} :: Identity)

-- | An Amazon Web Services SSO group identity.
identity_group :: Lens.Lens' Identity (Prelude.Maybe GroupIdentity)
identity_group = Lens.lens (\Identity' {group'} -> group') (\s@Identity' {} a -> s {group' = a} :: Identity)

-- | An Amazon Web Services SSO user identity.
identity_user :: Lens.Lens' Identity (Prelude.Maybe UserIdentity)
identity_user = Lens.lens (\Identity' {user} -> user) (\s@Identity' {} a -> s {user = a} :: Identity)

-- | An IAM role identity.
identity_iamRole :: Lens.Lens' Identity (Prelude.Maybe IAMRoleIdentity)
identity_iamRole = Lens.lens (\Identity' {iamRole} -> iamRole) (\s@Identity' {} a -> s {iamRole = a} :: Identity)

instance Core.FromJSON Identity where
  parseJSON =
    Core.withObject
      "Identity"
      ( \x ->
          Identity'
            Prelude.<$> (x Core..:? "iamUser")
            Prelude.<*> (x Core..:? "group")
            Prelude.<*> (x Core..:? "user")
            Prelude.<*> (x Core..:? "iamRole")
      )

instance Prelude.Hashable Identity where
  hashWithSalt _salt Identity' {..} =
    _salt `Prelude.hashWithSalt` iamUser
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` iamRole

instance Prelude.NFData Identity where
  rnf Identity' {..} =
    Prelude.rnf iamUser
      `Prelude.seq` Prelude.rnf group'
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf iamRole

instance Core.ToJSON Identity where
  toJSON Identity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("iamUser" Core..=) Prelude.<$> iamUser,
            ("group" Core..=) Prelude.<$> group',
            ("user" Core..=) Prelude.<$> user,
            ("iamRole" Core..=) Prelude.<$> iamRole
          ]
      )
