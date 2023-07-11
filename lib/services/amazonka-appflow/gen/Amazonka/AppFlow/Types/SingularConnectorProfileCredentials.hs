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
-- Module      : Amazonka.AppFlow.Types.SingularConnectorProfileCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SingularConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using Singular.
--
-- /See:/ 'newSingularConnectorProfileCredentials' smart constructor.
data SingularConnectorProfileCredentials = SingularConnectorProfileCredentials'
  { -- | A unique alphanumeric identifier used to authenticate a user, developer,
    -- or calling program to your API.
    apiKey :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingularConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'singularConnectorProfileCredentials_apiKey' - A unique alphanumeric identifier used to authenticate a user, developer,
-- or calling program to your API.
newSingularConnectorProfileCredentials ::
  -- | 'apiKey'
  Prelude.Text ->
  SingularConnectorProfileCredentials
newSingularConnectorProfileCredentials pApiKey_ =
  SingularConnectorProfileCredentials'
    { apiKey =
        Data._Sensitive Lens.# pApiKey_
    }

-- | A unique alphanumeric identifier used to authenticate a user, developer,
-- or calling program to your API.
singularConnectorProfileCredentials_apiKey :: Lens.Lens' SingularConnectorProfileCredentials Prelude.Text
singularConnectorProfileCredentials_apiKey = Lens.lens (\SingularConnectorProfileCredentials' {apiKey} -> apiKey) (\s@SingularConnectorProfileCredentials' {} a -> s {apiKey = a} :: SingularConnectorProfileCredentials) Prelude.. Data._Sensitive

instance
  Prelude.Hashable
    SingularConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    SingularConnectorProfileCredentials' {..} =
      _salt `Prelude.hashWithSalt` apiKey

instance
  Prelude.NFData
    SingularConnectorProfileCredentials
  where
  rnf SingularConnectorProfileCredentials' {..} =
    Prelude.rnf apiKey

instance
  Data.ToJSON
    SingularConnectorProfileCredentials
  where
  toJSON SingularConnectorProfileCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("apiKey" Data..= apiKey)]
      )
