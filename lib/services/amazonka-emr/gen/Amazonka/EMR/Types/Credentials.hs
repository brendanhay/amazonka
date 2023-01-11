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
-- Module      : Amazonka.EMR.Types.Credentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Credentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.UsernamePassword
import qualified Amazonka.Prelude as Prelude

-- | The credentials that you can use to connect to cluster endpoints.
-- Credentials consist of a username and a password.
--
-- /See:/ 'newCredentials' smart constructor.
data Credentials = Credentials'
  { -- | The username and password that you use to connect to cluster endpoints.
    usernamePassword :: Prelude.Maybe (Data.Sensitive UsernamePassword)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Credentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usernamePassword', 'credentials_usernamePassword' - The username and password that you use to connect to cluster endpoints.
newCredentials ::
  Credentials
newCredentials =
  Credentials' {usernamePassword = Prelude.Nothing}

-- | The username and password that you use to connect to cluster endpoints.
credentials_usernamePassword :: Lens.Lens' Credentials (Prelude.Maybe UsernamePassword)
credentials_usernamePassword = Lens.lens (\Credentials' {usernamePassword} -> usernamePassword) (\s@Credentials' {} a -> s {usernamePassword = a} :: Credentials) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Credentials where
  parseJSON =
    Data.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Prelude.<$> (x Data..:? "UsernamePassword")
      )

instance Prelude.Hashable Credentials where
  hashWithSalt _salt Credentials' {..} =
    _salt `Prelude.hashWithSalt` usernamePassword

instance Prelude.NFData Credentials where
  rnf Credentials' {..} = Prelude.rnf usernamePassword
