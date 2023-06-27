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
-- Module      : Amazonka.Kendra.Types.AuthenticationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AuthenticationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.BasicAuthenticationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to websites that
-- require user authentication.
--
-- /See:/ 'newAuthenticationConfiguration' smart constructor.
data AuthenticationConfiguration = AuthenticationConfiguration'
  { -- | The list of configuration information that\'s required to connect to and
    -- crawl a website host using basic authentication credentials.
    --
    -- The list includes the name and port number of the website host.
    basicAuthentication :: Prelude.Maybe [BasicAuthenticationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basicAuthentication', 'authenticationConfiguration_basicAuthentication' - The list of configuration information that\'s required to connect to and
-- crawl a website host using basic authentication credentials.
--
-- The list includes the name and port number of the website host.
newAuthenticationConfiguration ::
  AuthenticationConfiguration
newAuthenticationConfiguration =
  AuthenticationConfiguration'
    { basicAuthentication =
        Prelude.Nothing
    }

-- | The list of configuration information that\'s required to connect to and
-- crawl a website host using basic authentication credentials.
--
-- The list includes the name and port number of the website host.
authenticationConfiguration_basicAuthentication :: Lens.Lens' AuthenticationConfiguration (Prelude.Maybe [BasicAuthenticationConfiguration])
authenticationConfiguration_basicAuthentication = Lens.lens (\AuthenticationConfiguration' {basicAuthentication} -> basicAuthentication) (\s@AuthenticationConfiguration' {} a -> s {basicAuthentication = a} :: AuthenticationConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AuthenticationConfiguration where
  parseJSON =
    Data.withObject
      "AuthenticationConfiguration"
      ( \x ->
          AuthenticationConfiguration'
            Prelude.<$> ( x
                            Data..:? "BasicAuthentication"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AuthenticationConfiguration where
  hashWithSalt _salt AuthenticationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` basicAuthentication

instance Prelude.NFData AuthenticationConfiguration where
  rnf AuthenticationConfiguration' {..} =
    Prelude.rnf basicAuthentication

instance Data.ToJSON AuthenticationConfiguration where
  toJSON AuthenticationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BasicAuthentication" Data..=)
              Prelude.<$> basicAuthentication
          ]
      )
