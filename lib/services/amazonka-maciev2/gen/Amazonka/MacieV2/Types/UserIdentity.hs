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
-- Module      : Amazonka.MacieV2.Types.UserIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UserIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.AssumedRole
import Amazonka.MacieV2.Types.AwsAccount
import Amazonka.MacieV2.Types.AwsService
import Amazonka.MacieV2.Types.FederatedUser
import Amazonka.MacieV2.Types.IamUser
import Amazonka.MacieV2.Types.UserIdentityRoot
import Amazonka.MacieV2.Types.UserIdentityType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the type and other characteristics of an
-- entity that performed an action on an affected resource.
--
-- /See:/ 'newUserIdentity' smart constructor.
data UserIdentity = UserIdentity'
  { -- | If the action was performed using the credentials for an Identity and
    -- Access Management (IAM) user, the name and other details about the user.
    iamUser :: Prelude.Maybe IamUser,
    -- | If the action was performed using the credentials for your Amazon Web
    -- Services account, the details of your account.
    root :: Prelude.Maybe UserIdentityRoot,
    -- | If the action was performed using the credentials for another Amazon Web
    -- Services account, the details of that account.
    awsAccount :: Prelude.Maybe AwsAccount,
    -- | If the action was performed with temporary security credentials that
    -- were obtained using the AssumeRole operation of the Security Token
    -- Service (STS) API, the identifiers, session context, and other details
    -- about the identity.
    assumedRole :: Prelude.Maybe AssumedRole,
    -- | If the action was performed with temporary security credentials that
    -- were obtained using the GetFederationToken operation of the Security
    -- Token Service (STS) API, the identifiers, session context, and other
    -- details about the identity.
    federatedUser :: Prelude.Maybe FederatedUser,
    -- | If the action was performed by an Amazon Web Services account that
    -- belongs to an Amazon Web Service, the name of the service.
    awsService :: Prelude.Maybe AwsService,
    -- | The type of entity that performed the action.
    type' :: Prelude.Maybe UserIdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUser', 'userIdentity_iamUser' - If the action was performed using the credentials for an Identity and
-- Access Management (IAM) user, the name and other details about the user.
--
-- 'root', 'userIdentity_root' - If the action was performed using the credentials for your Amazon Web
-- Services account, the details of your account.
--
-- 'awsAccount', 'userIdentity_awsAccount' - If the action was performed using the credentials for another Amazon Web
-- Services account, the details of that account.
--
-- 'assumedRole', 'userIdentity_assumedRole' - If the action was performed with temporary security credentials that
-- were obtained using the AssumeRole operation of the Security Token
-- Service (STS) API, the identifiers, session context, and other details
-- about the identity.
--
-- 'federatedUser', 'userIdentity_federatedUser' - If the action was performed with temporary security credentials that
-- were obtained using the GetFederationToken operation of the Security
-- Token Service (STS) API, the identifiers, session context, and other
-- details about the identity.
--
-- 'awsService', 'userIdentity_awsService' - If the action was performed by an Amazon Web Services account that
-- belongs to an Amazon Web Service, the name of the service.
--
-- 'type'', 'userIdentity_type' - The type of entity that performed the action.
newUserIdentity ::
  UserIdentity
newUserIdentity =
  UserIdentity'
    { iamUser = Prelude.Nothing,
      root = Prelude.Nothing,
      awsAccount = Prelude.Nothing,
      assumedRole = Prelude.Nothing,
      federatedUser = Prelude.Nothing,
      awsService = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | If the action was performed using the credentials for an Identity and
-- Access Management (IAM) user, the name and other details about the user.
userIdentity_iamUser :: Lens.Lens' UserIdentity (Prelude.Maybe IamUser)
userIdentity_iamUser = Lens.lens (\UserIdentity' {iamUser} -> iamUser) (\s@UserIdentity' {} a -> s {iamUser = a} :: UserIdentity)

-- | If the action was performed using the credentials for your Amazon Web
-- Services account, the details of your account.
userIdentity_root :: Lens.Lens' UserIdentity (Prelude.Maybe UserIdentityRoot)
userIdentity_root = Lens.lens (\UserIdentity' {root} -> root) (\s@UserIdentity' {} a -> s {root = a} :: UserIdentity)

-- | If the action was performed using the credentials for another Amazon Web
-- Services account, the details of that account.
userIdentity_awsAccount :: Lens.Lens' UserIdentity (Prelude.Maybe AwsAccount)
userIdentity_awsAccount = Lens.lens (\UserIdentity' {awsAccount} -> awsAccount) (\s@UserIdentity' {} a -> s {awsAccount = a} :: UserIdentity)

-- | If the action was performed with temporary security credentials that
-- were obtained using the AssumeRole operation of the Security Token
-- Service (STS) API, the identifiers, session context, and other details
-- about the identity.
userIdentity_assumedRole :: Lens.Lens' UserIdentity (Prelude.Maybe AssumedRole)
userIdentity_assumedRole = Lens.lens (\UserIdentity' {assumedRole} -> assumedRole) (\s@UserIdentity' {} a -> s {assumedRole = a} :: UserIdentity)

-- | If the action was performed with temporary security credentials that
-- were obtained using the GetFederationToken operation of the Security
-- Token Service (STS) API, the identifiers, session context, and other
-- details about the identity.
userIdentity_federatedUser :: Lens.Lens' UserIdentity (Prelude.Maybe FederatedUser)
userIdentity_federatedUser = Lens.lens (\UserIdentity' {federatedUser} -> federatedUser) (\s@UserIdentity' {} a -> s {federatedUser = a} :: UserIdentity)

-- | If the action was performed by an Amazon Web Services account that
-- belongs to an Amazon Web Service, the name of the service.
userIdentity_awsService :: Lens.Lens' UserIdentity (Prelude.Maybe AwsService)
userIdentity_awsService = Lens.lens (\UserIdentity' {awsService} -> awsService) (\s@UserIdentity' {} a -> s {awsService = a} :: UserIdentity)

-- | The type of entity that performed the action.
userIdentity_type :: Lens.Lens' UserIdentity (Prelude.Maybe UserIdentityType)
userIdentity_type = Lens.lens (\UserIdentity' {type'} -> type') (\s@UserIdentity' {} a -> s {type' = a} :: UserIdentity)

instance Core.FromJSON UserIdentity where
  parseJSON =
    Core.withObject
      "UserIdentity"
      ( \x ->
          UserIdentity'
            Prelude.<$> (x Core..:? "iamUser")
            Prelude.<*> (x Core..:? "root")
            Prelude.<*> (x Core..:? "awsAccount")
            Prelude.<*> (x Core..:? "assumedRole")
            Prelude.<*> (x Core..:? "federatedUser")
            Prelude.<*> (x Core..:? "awsService")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable UserIdentity where
  hashWithSalt salt' UserIdentity' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` awsService
      `Prelude.hashWithSalt` federatedUser
      `Prelude.hashWithSalt` assumedRole
      `Prelude.hashWithSalt` awsAccount
      `Prelude.hashWithSalt` root
      `Prelude.hashWithSalt` iamUser

instance Prelude.NFData UserIdentity where
  rnf UserIdentity' {..} =
    Prelude.rnf iamUser `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf awsService
      `Prelude.seq` Prelude.rnf federatedUser
      `Prelude.seq` Prelude.rnf assumedRole
      `Prelude.seq` Prelude.rnf awsAccount
      `Prelude.seq` Prelude.rnf root
