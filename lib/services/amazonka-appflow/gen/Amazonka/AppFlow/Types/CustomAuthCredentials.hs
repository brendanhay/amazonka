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
-- Module      : Amazonka.AppFlow.Types.CustomAuthCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.CustomAuthCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The custom credentials required for custom authentication.
--
-- /See:/ 'newCustomAuthCredentials' smart constructor.
data CustomAuthCredentials = CustomAuthCredentials'
  { -- | A map that holds custom authentication credentials.
    credentialsMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The custom authentication type that the connector uses.
    customAuthenticationType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomAuthCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialsMap', 'customAuthCredentials_credentialsMap' - A map that holds custom authentication credentials.
--
-- 'customAuthenticationType', 'customAuthCredentials_customAuthenticationType' - The custom authentication type that the connector uses.
newCustomAuthCredentials ::
  -- | 'customAuthenticationType'
  Prelude.Text ->
  CustomAuthCredentials
newCustomAuthCredentials pCustomAuthenticationType_ =
  CustomAuthCredentials'
    { credentialsMap =
        Prelude.Nothing,
      customAuthenticationType =
        pCustomAuthenticationType_
    }

-- | A map that holds custom authentication credentials.
customAuthCredentials_credentialsMap :: Lens.Lens' CustomAuthCredentials (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
customAuthCredentials_credentialsMap = Lens.lens (\CustomAuthCredentials' {credentialsMap} -> credentialsMap) (\s@CustomAuthCredentials' {} a -> s {credentialsMap = a} :: CustomAuthCredentials) Prelude.. Lens.mapping Lens.coerced

-- | The custom authentication type that the connector uses.
customAuthCredentials_customAuthenticationType :: Lens.Lens' CustomAuthCredentials Prelude.Text
customAuthCredentials_customAuthenticationType = Lens.lens (\CustomAuthCredentials' {customAuthenticationType} -> customAuthenticationType) (\s@CustomAuthCredentials' {} a -> s {customAuthenticationType = a} :: CustomAuthCredentials)

instance Prelude.Hashable CustomAuthCredentials where
  hashWithSalt _salt CustomAuthCredentials' {..} =
    _salt `Prelude.hashWithSalt` credentialsMap
      `Prelude.hashWithSalt` customAuthenticationType

instance Prelude.NFData CustomAuthCredentials where
  rnf CustomAuthCredentials' {..} =
    Prelude.rnf credentialsMap
      `Prelude.seq` Prelude.rnf customAuthenticationType

instance Data.ToJSON CustomAuthCredentials where
  toJSON CustomAuthCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("credentialsMap" Data..=)
              Prelude.<$> credentialsMap,
            Prelude.Just
              ( "customAuthenticationType"
                  Data..= customAuthenticationType
              )
          ]
      )
