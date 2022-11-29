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
-- Module      : Amazonka.ElastiCache.Types.AuthenticationMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.AuthenticationMode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types.InputAuthenticationType
import qualified Amazonka.Prelude as Prelude

-- | Specifies the authentication mode to use.
--
-- /See:/ 'newAuthenticationMode' smart constructor.
data AuthenticationMode = AuthenticationMode'
  { -- | Specifies the passwords to use for authentication if @Type@ is set to
    -- @password@.
    passwords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the authentication type. Possible options are IAM
    -- authentication, password and no password.
    type' :: Prelude.Maybe InputAuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationMode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwords', 'authenticationMode_passwords' - Specifies the passwords to use for authentication if @Type@ is set to
-- @password@.
--
-- 'type'', 'authenticationMode_type' - Specifies the authentication type. Possible options are IAM
-- authentication, password and no password.
newAuthenticationMode ::
  AuthenticationMode
newAuthenticationMode =
  AuthenticationMode'
    { passwords = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Specifies the passwords to use for authentication if @Type@ is set to
-- @password@.
authenticationMode_passwords :: Lens.Lens' AuthenticationMode (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
authenticationMode_passwords = Lens.lens (\AuthenticationMode' {passwords} -> passwords) (\s@AuthenticationMode' {} a -> s {passwords = a} :: AuthenticationMode) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the authentication type. Possible options are IAM
-- authentication, password and no password.
authenticationMode_type :: Lens.Lens' AuthenticationMode (Prelude.Maybe InputAuthenticationType)
authenticationMode_type = Lens.lens (\AuthenticationMode' {type'} -> type') (\s@AuthenticationMode' {} a -> s {type' = a} :: AuthenticationMode)

instance Prelude.Hashable AuthenticationMode where
  hashWithSalt _salt AuthenticationMode' {..} =
    _salt `Prelude.hashWithSalt` passwords
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AuthenticationMode where
  rnf AuthenticationMode' {..} =
    Prelude.rnf passwords
      `Prelude.seq` Prelude.rnf type'

instance Core.ToQuery AuthenticationMode where
  toQuery AuthenticationMode' {..} =
    Prelude.mconcat
      [ "Passwords"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> passwords),
        "Type" Core.=: type'
      ]
