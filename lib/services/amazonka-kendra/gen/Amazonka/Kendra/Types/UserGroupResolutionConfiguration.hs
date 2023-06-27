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
-- Module      : Amazonka.Kendra.Types.UserGroupResolutionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.UserGroupResolutionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.UserGroupResolutionMode
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to get users and groups from an
-- IAM Identity Center (successor to Single Sign-On) identity source. This
-- is useful for user context filtering, where search results are filtered
-- based on the user or their group access to documents. You can also use
-- the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_PutPrincipalMapping.html PutPrincipalMapping>
-- API to map users to their groups so that you only need to provide the
-- user ID when you issue the query.
--
-- To set up an IAM Identity Center identity source in the console to use
-- with Amazon Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/getting-started-aws-sso.html Getting started with an IAM Identity Center identity source>.
-- You must also grant the required permissions to use IAM Identity Center
-- with Amazon Kendra. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html#iam-roles-aws-sso IAM roles for IAM Identity Center>.
--
-- Amazon Kendra currently does not support using
-- @UserGroupResolutionConfiguration@ with an Amazon Web Services
-- organization member account for your IAM Identity Center identify
-- source. You must create your index in the management account for the
-- organization in order to use @UserGroupResolutionConfiguration@.
--
-- /See:/ 'newUserGroupResolutionConfiguration' smart constructor.
data UserGroupResolutionConfiguration = UserGroupResolutionConfiguration'
  { -- | The identity store provider (mode) you want to use to get users and
    -- groups. IAM Identity Center (successor to Single Sign-On) is currently
    -- the only available mode. Your users and groups must exist in an IAM
    -- Identity Center identity source in order to use this mode.
    userGroupResolutionMode :: UserGroupResolutionMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserGroupResolutionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userGroupResolutionMode', 'userGroupResolutionConfiguration_userGroupResolutionMode' - The identity store provider (mode) you want to use to get users and
-- groups. IAM Identity Center (successor to Single Sign-On) is currently
-- the only available mode. Your users and groups must exist in an IAM
-- Identity Center identity source in order to use this mode.
newUserGroupResolutionConfiguration ::
  -- | 'userGroupResolutionMode'
  UserGroupResolutionMode ->
  UserGroupResolutionConfiguration
newUserGroupResolutionConfiguration
  pUserGroupResolutionMode_ =
    UserGroupResolutionConfiguration'
      { userGroupResolutionMode =
          pUserGroupResolutionMode_
      }

-- | The identity store provider (mode) you want to use to get users and
-- groups. IAM Identity Center (successor to Single Sign-On) is currently
-- the only available mode. Your users and groups must exist in an IAM
-- Identity Center identity source in order to use this mode.
userGroupResolutionConfiguration_userGroupResolutionMode :: Lens.Lens' UserGroupResolutionConfiguration UserGroupResolutionMode
userGroupResolutionConfiguration_userGroupResolutionMode = Lens.lens (\UserGroupResolutionConfiguration' {userGroupResolutionMode} -> userGroupResolutionMode) (\s@UserGroupResolutionConfiguration' {} a -> s {userGroupResolutionMode = a} :: UserGroupResolutionConfiguration)

instance
  Data.FromJSON
    UserGroupResolutionConfiguration
  where
  parseJSON =
    Data.withObject
      "UserGroupResolutionConfiguration"
      ( \x ->
          UserGroupResolutionConfiguration'
            Prelude.<$> (x Data..: "UserGroupResolutionMode")
      )

instance
  Prelude.Hashable
    UserGroupResolutionConfiguration
  where
  hashWithSalt
    _salt
    UserGroupResolutionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` userGroupResolutionMode

instance
  Prelude.NFData
    UserGroupResolutionConfiguration
  where
  rnf UserGroupResolutionConfiguration' {..} =
    Prelude.rnf userGroupResolutionMode

instance Data.ToJSON UserGroupResolutionConfiguration where
  toJSON UserGroupResolutionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UserGroupResolutionMode"
                  Data..= userGroupResolutionMode
              )
          ]
      )
