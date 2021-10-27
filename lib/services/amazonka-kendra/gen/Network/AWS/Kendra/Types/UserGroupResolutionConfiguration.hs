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
-- Module      : Network.AWS.Kendra.Types.UserGroupResolutionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.UserGroupResolutionConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.UserGroupResolutionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the configuration information to fetch access levels of groups
-- and users from an AWS Single Sign-On identity source. This is useful for
-- setting up user context filtering, where Amazon Kendra filters search
-- results for different users based on their group\'s access to documents.
-- You can also map your users to their groups for user context filtering
-- using the
-- <https://docs.aws.amazon.com/latest/dg/API_PutPrincipalMapping.html PutPrincipalMapping operation>.
--
-- To set up an AWS SSO identity source in the console to use with Amazon
-- Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/getting-started-aws-sso.html Getting started with an AWS SSO identity source>.
-- You must also grant the required permissions to use AWS SSO with Amazon
-- Kendra. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html#iam-roles-aws-sso IAM roles for AWS Single Sign-On>.
--
-- /See:/ 'newUserGroupResolutionConfiguration' smart constructor.
data UserGroupResolutionConfiguration = UserGroupResolutionConfiguration'
  { -- | The identity store provider (mode) you want to use to fetch access
    -- levels of groups and users. AWS Single Sign-On is currently the only
    -- available mode. Your users and groups must exist in an AWS SSO identity
    -- source in order to use this mode.
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
-- 'userGroupResolutionMode', 'userGroupResolutionConfiguration_userGroupResolutionMode' - The identity store provider (mode) you want to use to fetch access
-- levels of groups and users. AWS Single Sign-On is currently the only
-- available mode. Your users and groups must exist in an AWS SSO identity
-- source in order to use this mode.
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

-- | The identity store provider (mode) you want to use to fetch access
-- levels of groups and users. AWS Single Sign-On is currently the only
-- available mode. Your users and groups must exist in an AWS SSO identity
-- source in order to use this mode.
userGroupResolutionConfiguration_userGroupResolutionMode :: Lens.Lens' UserGroupResolutionConfiguration UserGroupResolutionMode
userGroupResolutionConfiguration_userGroupResolutionMode = Lens.lens (\UserGroupResolutionConfiguration' {userGroupResolutionMode} -> userGroupResolutionMode) (\s@UserGroupResolutionConfiguration' {} a -> s {userGroupResolutionMode = a} :: UserGroupResolutionConfiguration)

instance
  Core.FromJSON
    UserGroupResolutionConfiguration
  where
  parseJSON =
    Core.withObject
      "UserGroupResolutionConfiguration"
      ( \x ->
          UserGroupResolutionConfiguration'
            Prelude.<$> (x Core..: "UserGroupResolutionMode")
      )

instance
  Prelude.Hashable
    UserGroupResolutionConfiguration

instance
  Prelude.NFData
    UserGroupResolutionConfiguration

instance Core.ToJSON UserGroupResolutionConfiguration where
  toJSON UserGroupResolutionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UserGroupResolutionMode"
                  Core..= userGroupResolutionMode
              )
          ]
      )
