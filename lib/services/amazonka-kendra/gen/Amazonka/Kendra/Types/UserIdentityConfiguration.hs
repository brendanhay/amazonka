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
-- Module      : Amazonka.Kendra.Types.UserIdentityConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.UserIdentityConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for the identifiers of your
-- users.
--
-- /See:/ 'newUserIdentityConfiguration' smart constructor.
data UserIdentityConfiguration = UserIdentityConfiguration'
  { -- | The IAM Identity Center field name that contains the identifiers of your
    -- users, such as their emails. This is used for
    -- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html user context filtering>
    -- and for granting access to your Amazon Kendra experience. You must set
    -- up IAM Identity Center with Amazon Kendra. You must include your users
    -- and groups in your Access Control List when you ingest documents into
    -- your index. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/getting-started-aws-sso.html Getting started with an IAM Identity Center identity source>.
    identityAttributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserIdentityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityAttributeName', 'userIdentityConfiguration_identityAttributeName' - The IAM Identity Center field name that contains the identifiers of your
-- users, such as their emails. This is used for
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html user context filtering>
-- and for granting access to your Amazon Kendra experience. You must set
-- up IAM Identity Center with Amazon Kendra. You must include your users
-- and groups in your Access Control List when you ingest documents into
-- your index. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/getting-started-aws-sso.html Getting started with an IAM Identity Center identity source>.
newUserIdentityConfiguration ::
  UserIdentityConfiguration
newUserIdentityConfiguration =
  UserIdentityConfiguration'
    { identityAttributeName =
        Prelude.Nothing
    }

-- | The IAM Identity Center field name that contains the identifiers of your
-- users, such as their emails. This is used for
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html user context filtering>
-- and for granting access to your Amazon Kendra experience. You must set
-- up IAM Identity Center with Amazon Kendra. You must include your users
-- and groups in your Access Control List when you ingest documents into
-- your index. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/getting-started-aws-sso.html Getting started with an IAM Identity Center identity source>.
userIdentityConfiguration_identityAttributeName :: Lens.Lens' UserIdentityConfiguration (Prelude.Maybe Prelude.Text)
userIdentityConfiguration_identityAttributeName = Lens.lens (\UserIdentityConfiguration' {identityAttributeName} -> identityAttributeName) (\s@UserIdentityConfiguration' {} a -> s {identityAttributeName = a} :: UserIdentityConfiguration)

instance Data.FromJSON UserIdentityConfiguration where
  parseJSON =
    Data.withObject
      "UserIdentityConfiguration"
      ( \x ->
          UserIdentityConfiguration'
            Prelude.<$> (x Data..:? "IdentityAttributeName")
      )

instance Prelude.Hashable UserIdentityConfiguration where
  hashWithSalt _salt UserIdentityConfiguration' {..} =
    _salt `Prelude.hashWithSalt` identityAttributeName

instance Prelude.NFData UserIdentityConfiguration where
  rnf UserIdentityConfiguration' {..} =
    Prelude.rnf identityAttributeName

instance Data.ToJSON UserIdentityConfiguration where
  toJSON UserIdentityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdentityAttributeName" Data..=)
              Prelude.<$> identityAttributeName
          ]
      )
