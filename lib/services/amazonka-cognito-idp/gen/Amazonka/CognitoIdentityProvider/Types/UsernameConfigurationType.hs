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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UsernameConfigurationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UsernameConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The username configuration type.
--
-- /See:/ 'newUsernameConfigurationType' smart constructor.
data UsernameConfigurationType = UsernameConfigurationType'
  { -- | Specifies whether user name case sensitivity will be applied for all
    -- users in the user pool through Amazon Cognito APIs.
    --
    -- Valid values include:
    --
    -- [True]
    --     Enables case sensitivity for all username input. When this option is
    --     set to @True@, users must sign in using the exact capitalization of
    --     their given username, such as “UserName”. This is the default value.
    --
    -- [False]
    --     Enables case insensitivity for all username input. For example, when
    --     this option is set to @False@, users can sign in using either
    --     \"username\" or \"Username\". This option also enables both
    --     @preferred_username@ and @email@ alias to be case insensitive, in
    --     addition to the @username@ attribute.
    caseSensitive :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsernameConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseSensitive', 'usernameConfigurationType_caseSensitive' - Specifies whether user name case sensitivity will be applied for all
-- users in the user pool through Amazon Cognito APIs.
--
-- Valid values include:
--
-- [True]
--     Enables case sensitivity for all username input. When this option is
--     set to @True@, users must sign in using the exact capitalization of
--     their given username, such as “UserName”. This is the default value.
--
-- [False]
--     Enables case insensitivity for all username input. For example, when
--     this option is set to @False@, users can sign in using either
--     \"username\" or \"Username\". This option also enables both
--     @preferred_username@ and @email@ alias to be case insensitive, in
--     addition to the @username@ attribute.
newUsernameConfigurationType ::
  -- | 'caseSensitive'
  Prelude.Bool ->
  UsernameConfigurationType
newUsernameConfigurationType pCaseSensitive_ =
  UsernameConfigurationType'
    { caseSensitive =
        pCaseSensitive_
    }

-- | Specifies whether user name case sensitivity will be applied for all
-- users in the user pool through Amazon Cognito APIs.
--
-- Valid values include:
--
-- [True]
--     Enables case sensitivity for all username input. When this option is
--     set to @True@, users must sign in using the exact capitalization of
--     their given username, such as “UserName”. This is the default value.
--
-- [False]
--     Enables case insensitivity for all username input. For example, when
--     this option is set to @False@, users can sign in using either
--     \"username\" or \"Username\". This option also enables both
--     @preferred_username@ and @email@ alias to be case insensitive, in
--     addition to the @username@ attribute.
usernameConfigurationType_caseSensitive :: Lens.Lens' UsernameConfigurationType Prelude.Bool
usernameConfigurationType_caseSensitive = Lens.lens (\UsernameConfigurationType' {caseSensitive} -> caseSensitive) (\s@UsernameConfigurationType' {} a -> s {caseSensitive = a} :: UsernameConfigurationType)

instance Data.FromJSON UsernameConfigurationType where
  parseJSON =
    Data.withObject
      "UsernameConfigurationType"
      ( \x ->
          UsernameConfigurationType'
            Prelude.<$> (x Data..: "CaseSensitive")
      )

instance Prelude.Hashable UsernameConfigurationType where
  hashWithSalt _salt UsernameConfigurationType' {..} =
    _salt `Prelude.hashWithSalt` caseSensitive

instance Prelude.NFData UsernameConfigurationType where
  rnf UsernameConfigurationType' {..} =
    Prelude.rnf caseSensitive

instance Data.ToJSON UsernameConfigurationType where
  toJSON UsernameConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CaseSensitive" Data..= caseSensitive)
          ]
      )
