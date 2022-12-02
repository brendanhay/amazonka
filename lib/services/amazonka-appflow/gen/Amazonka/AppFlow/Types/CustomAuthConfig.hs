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
-- Module      : Amazonka.AppFlow.Types.CustomAuthConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.CustomAuthConfig where

import Amazonka.AppFlow.Types.AuthParameter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information required for custom authentication.
--
-- /See:/ 'newCustomAuthConfig' smart constructor.
data CustomAuthConfig = CustomAuthConfig'
  { -- | Information about authentication parameters required for authentication.
    authParameters :: Prelude.Maybe [AuthParameter],
    -- | The authentication type that the custom connector uses.
    customAuthenticationType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomAuthConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authParameters', 'customAuthConfig_authParameters' - Information about authentication parameters required for authentication.
--
-- 'customAuthenticationType', 'customAuthConfig_customAuthenticationType' - The authentication type that the custom connector uses.
newCustomAuthConfig ::
  CustomAuthConfig
newCustomAuthConfig =
  CustomAuthConfig'
    { authParameters = Prelude.Nothing,
      customAuthenticationType = Prelude.Nothing
    }

-- | Information about authentication parameters required for authentication.
customAuthConfig_authParameters :: Lens.Lens' CustomAuthConfig (Prelude.Maybe [AuthParameter])
customAuthConfig_authParameters = Lens.lens (\CustomAuthConfig' {authParameters} -> authParameters) (\s@CustomAuthConfig' {} a -> s {authParameters = a} :: CustomAuthConfig) Prelude.. Lens.mapping Lens.coerced

-- | The authentication type that the custom connector uses.
customAuthConfig_customAuthenticationType :: Lens.Lens' CustomAuthConfig (Prelude.Maybe Prelude.Text)
customAuthConfig_customAuthenticationType = Lens.lens (\CustomAuthConfig' {customAuthenticationType} -> customAuthenticationType) (\s@CustomAuthConfig' {} a -> s {customAuthenticationType = a} :: CustomAuthConfig)

instance Data.FromJSON CustomAuthConfig where
  parseJSON =
    Data.withObject
      "CustomAuthConfig"
      ( \x ->
          CustomAuthConfig'
            Prelude.<$> (x Data..:? "authParameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "customAuthenticationType")
      )

instance Prelude.Hashable CustomAuthConfig where
  hashWithSalt _salt CustomAuthConfig' {..} =
    _salt `Prelude.hashWithSalt` authParameters
      `Prelude.hashWithSalt` customAuthenticationType

instance Prelude.NFData CustomAuthConfig where
  rnf CustomAuthConfig' {..} =
    Prelude.rnf authParameters
      `Prelude.seq` Prelude.rnf customAuthenticationType
