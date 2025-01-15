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
-- Module      : Amazonka.QuickSight.Types.CredentialPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CredentialPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSourceParameters

-- | The combination of user name and password that are used as credentials.
--
-- /See:/ 'newCredentialPair' smart constructor.
data CredentialPair = CredentialPair'
  { -- | A set of alternate data source parameters that you want to share for
    -- these credentials. The credentials are applied in tandem with the data
    -- source parameters when you copy a data source by using a create or
    -- update request. The API operation compares the @DataSourceParameters@
    -- structure that\'s in the request with the structures in the
    -- @AlternateDataSourceParameters@ allow list. If the structures are an
    -- exact match, the request is allowed to use the new data source with the
    -- existing credentials. If the @AlternateDataSourceParameters@ list is
    -- null, the @DataSourceParameters@ originally used with these
    -- @Credentials@ is automatically allowed.
    alternateDataSourceParameters :: Prelude.Maybe (Prelude.NonEmpty DataSourceParameters),
    -- | User name.
    username :: Prelude.Text,
    -- | Password.
    password :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CredentialPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alternateDataSourceParameters', 'credentialPair_alternateDataSourceParameters' - A set of alternate data source parameters that you want to share for
-- these credentials. The credentials are applied in tandem with the data
-- source parameters when you copy a data source by using a create or
-- update request. The API operation compares the @DataSourceParameters@
-- structure that\'s in the request with the structures in the
-- @AlternateDataSourceParameters@ allow list. If the structures are an
-- exact match, the request is allowed to use the new data source with the
-- existing credentials. If the @AlternateDataSourceParameters@ list is
-- null, the @DataSourceParameters@ originally used with these
-- @Credentials@ is automatically allowed.
--
-- 'username', 'credentialPair_username' - User name.
--
-- 'password', 'credentialPair_password' - Password.
newCredentialPair ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CredentialPair
newCredentialPair pUsername_ pPassword_ =
  CredentialPair'
    { alternateDataSourceParameters =
        Prelude.Nothing,
      username = pUsername_,
      password = pPassword_
    }

-- | A set of alternate data source parameters that you want to share for
-- these credentials. The credentials are applied in tandem with the data
-- source parameters when you copy a data source by using a create or
-- update request. The API operation compares the @DataSourceParameters@
-- structure that\'s in the request with the structures in the
-- @AlternateDataSourceParameters@ allow list. If the structures are an
-- exact match, the request is allowed to use the new data source with the
-- existing credentials. If the @AlternateDataSourceParameters@ list is
-- null, the @DataSourceParameters@ originally used with these
-- @Credentials@ is automatically allowed.
credentialPair_alternateDataSourceParameters :: Lens.Lens' CredentialPair (Prelude.Maybe (Prelude.NonEmpty DataSourceParameters))
credentialPair_alternateDataSourceParameters = Lens.lens (\CredentialPair' {alternateDataSourceParameters} -> alternateDataSourceParameters) (\s@CredentialPair' {} a -> s {alternateDataSourceParameters = a} :: CredentialPair) Prelude.. Lens.mapping Lens.coerced

-- | User name.
credentialPair_username :: Lens.Lens' CredentialPair Prelude.Text
credentialPair_username = Lens.lens (\CredentialPair' {username} -> username) (\s@CredentialPair' {} a -> s {username = a} :: CredentialPair)

-- | Password.
credentialPair_password :: Lens.Lens' CredentialPair Prelude.Text
credentialPair_password = Lens.lens (\CredentialPair' {password} -> password) (\s@CredentialPair' {} a -> s {password = a} :: CredentialPair)

instance Prelude.Hashable CredentialPair where
  hashWithSalt _salt CredentialPair' {..} =
    _salt
      `Prelude.hashWithSalt` alternateDataSourceParameters
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData CredentialPair where
  rnf CredentialPair' {..} =
    Prelude.rnf alternateDataSourceParameters `Prelude.seq`
      Prelude.rnf username `Prelude.seq`
        Prelude.rnf password

instance Data.ToJSON CredentialPair where
  toJSON CredentialPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlternateDataSourceParameters" Data..=)
              Prelude.<$> alternateDataSourceParameters,
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("Password" Data..= password)
          ]
      )
