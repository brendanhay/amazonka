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
-- Module      : Amazonka.Kendra.Types.UserTokenConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.UserTokenConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.JsonTokenTypeConfiguration
import Amazonka.Kendra.Types.JwtTokenTypeConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for a token.
--
-- /See:/ 'newUserTokenConfiguration' smart constructor.
data UserTokenConfiguration = UserTokenConfiguration'
  { -- | Information about the JWT token type configuration.
    jwtTokenTypeConfiguration :: Prelude.Maybe JwtTokenTypeConfiguration,
    -- | Information about the JSON token type configuration.
    jsonTokenTypeConfiguration :: Prelude.Maybe JsonTokenTypeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserTokenConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jwtTokenTypeConfiguration', 'userTokenConfiguration_jwtTokenTypeConfiguration' - Information about the JWT token type configuration.
--
-- 'jsonTokenTypeConfiguration', 'userTokenConfiguration_jsonTokenTypeConfiguration' - Information about the JSON token type configuration.
newUserTokenConfiguration ::
  UserTokenConfiguration
newUserTokenConfiguration =
  UserTokenConfiguration'
    { jwtTokenTypeConfiguration =
        Prelude.Nothing,
      jsonTokenTypeConfiguration = Prelude.Nothing
    }

-- | Information about the JWT token type configuration.
userTokenConfiguration_jwtTokenTypeConfiguration :: Lens.Lens' UserTokenConfiguration (Prelude.Maybe JwtTokenTypeConfiguration)
userTokenConfiguration_jwtTokenTypeConfiguration = Lens.lens (\UserTokenConfiguration' {jwtTokenTypeConfiguration} -> jwtTokenTypeConfiguration) (\s@UserTokenConfiguration' {} a -> s {jwtTokenTypeConfiguration = a} :: UserTokenConfiguration)

-- | Information about the JSON token type configuration.
userTokenConfiguration_jsonTokenTypeConfiguration :: Lens.Lens' UserTokenConfiguration (Prelude.Maybe JsonTokenTypeConfiguration)
userTokenConfiguration_jsonTokenTypeConfiguration = Lens.lens (\UserTokenConfiguration' {jsonTokenTypeConfiguration} -> jsonTokenTypeConfiguration) (\s@UserTokenConfiguration' {} a -> s {jsonTokenTypeConfiguration = a} :: UserTokenConfiguration)

instance Core.FromJSON UserTokenConfiguration where
  parseJSON =
    Core.withObject
      "UserTokenConfiguration"
      ( \x ->
          UserTokenConfiguration'
            Prelude.<$> (x Core..:? "JwtTokenTypeConfiguration")
            Prelude.<*> (x Core..:? "JsonTokenTypeConfiguration")
      )

instance Prelude.Hashable UserTokenConfiguration where
  hashWithSalt _salt UserTokenConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` jwtTokenTypeConfiguration
      `Prelude.hashWithSalt` jsonTokenTypeConfiguration

instance Prelude.NFData UserTokenConfiguration where
  rnf UserTokenConfiguration' {..} =
    Prelude.rnf jwtTokenTypeConfiguration
      `Prelude.seq` Prelude.rnf jsonTokenTypeConfiguration

instance Core.ToJSON UserTokenConfiguration where
  toJSON UserTokenConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("JwtTokenTypeConfiguration" Core..=)
              Prelude.<$> jwtTokenTypeConfiguration,
            ("JsonTokenTypeConfiguration" Core..=)
              Prelude.<$> jsonTokenTypeConfiguration
          ]
      )
