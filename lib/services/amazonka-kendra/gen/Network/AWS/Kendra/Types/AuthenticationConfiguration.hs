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
-- Module      : Network.AWS.Kendra.Types.AuthenticationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.AuthenticationConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.BasicAuthenticationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON AuthenticationConfiguration where
  parseJSON =
    Core.withObject
      "AuthenticationConfiguration"
      ( \x ->
          AuthenticationConfiguration'
            Prelude.<$> ( x Core..:? "BasicAuthentication"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AuthenticationConfiguration

instance Prelude.NFData AuthenticationConfiguration

instance Core.ToJSON AuthenticationConfiguration where
  toJSON AuthenticationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BasicAuthentication" Core..=)
              Prelude.<$> basicAuthentication
          ]
      )
