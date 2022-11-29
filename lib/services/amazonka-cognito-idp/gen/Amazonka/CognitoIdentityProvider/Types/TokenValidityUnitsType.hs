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
-- Module      : Amazonka.CognitoIdentityProvider.Types.TokenValidityUnitsType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.TokenValidityUnitsType where

import Amazonka.CognitoIdentityProvider.Types.TimeUnitsType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The data type TokenValidityUnits specifies the time units you use when
-- you set the duration of ID, access, and refresh tokens.
--
-- /See:/ 'newTokenValidityUnitsType' smart constructor.
data TokenValidityUnitsType = TokenValidityUnitsType'
  { -- | A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
    -- that you set in the @AccessTokenValidity@ parameter. The default
    -- @AccessTokenValidity@ time unit is hours.
    accessToken :: Prelude.Maybe TimeUnitsType,
    -- | A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
    -- that you set in the @IdTokenValidity@ parameter. The default
    -- @IdTokenValidity@ time unit is hours.
    idToken :: Prelude.Maybe TimeUnitsType,
    -- | A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
    -- that you set in the @RefreshTokenValidity@ parameter. The default
    -- @RefreshTokenValidity@ time unit is days.
    refreshToken :: Prelude.Maybe TimeUnitsType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TokenValidityUnitsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'tokenValidityUnitsType_accessToken' - A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
-- that you set in the @AccessTokenValidity@ parameter. The default
-- @AccessTokenValidity@ time unit is hours.
--
-- 'idToken', 'tokenValidityUnitsType_idToken' - A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
-- that you set in the @IdTokenValidity@ parameter. The default
-- @IdTokenValidity@ time unit is hours.
--
-- 'refreshToken', 'tokenValidityUnitsType_refreshToken' - A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
-- that you set in the @RefreshTokenValidity@ parameter. The default
-- @RefreshTokenValidity@ time unit is days.
newTokenValidityUnitsType ::
  TokenValidityUnitsType
newTokenValidityUnitsType =
  TokenValidityUnitsType'
    { accessToken =
        Prelude.Nothing,
      idToken = Prelude.Nothing,
      refreshToken = Prelude.Nothing
    }

-- | A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
-- that you set in the @AccessTokenValidity@ parameter. The default
-- @AccessTokenValidity@ time unit is hours.
tokenValidityUnitsType_accessToken :: Lens.Lens' TokenValidityUnitsType (Prelude.Maybe TimeUnitsType)
tokenValidityUnitsType_accessToken = Lens.lens (\TokenValidityUnitsType' {accessToken} -> accessToken) (\s@TokenValidityUnitsType' {} a -> s {accessToken = a} :: TokenValidityUnitsType)

-- | A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
-- that you set in the @IdTokenValidity@ parameter. The default
-- @IdTokenValidity@ time unit is hours.
tokenValidityUnitsType_idToken :: Lens.Lens' TokenValidityUnitsType (Prelude.Maybe TimeUnitsType)
tokenValidityUnitsType_idToken = Lens.lens (\TokenValidityUnitsType' {idToken} -> idToken) (\s@TokenValidityUnitsType' {} a -> s {idToken = a} :: TokenValidityUnitsType)

-- | A time unit of @seconds@, @minutes@, @hours@, or @days@ for the value
-- that you set in the @RefreshTokenValidity@ parameter. The default
-- @RefreshTokenValidity@ time unit is days.
tokenValidityUnitsType_refreshToken :: Lens.Lens' TokenValidityUnitsType (Prelude.Maybe TimeUnitsType)
tokenValidityUnitsType_refreshToken = Lens.lens (\TokenValidityUnitsType' {refreshToken} -> refreshToken) (\s@TokenValidityUnitsType' {} a -> s {refreshToken = a} :: TokenValidityUnitsType)

instance Core.FromJSON TokenValidityUnitsType where
  parseJSON =
    Core.withObject
      "TokenValidityUnitsType"
      ( \x ->
          TokenValidityUnitsType'
            Prelude.<$> (x Core..:? "AccessToken")
            Prelude.<*> (x Core..:? "IdToken")
            Prelude.<*> (x Core..:? "RefreshToken")
      )

instance Prelude.Hashable TokenValidityUnitsType where
  hashWithSalt _salt TokenValidityUnitsType' {..} =
    _salt `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` idToken
      `Prelude.hashWithSalt` refreshToken

instance Prelude.NFData TokenValidityUnitsType where
  rnf TokenValidityUnitsType' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf idToken
      `Prelude.seq` Prelude.rnf refreshToken

instance Core.ToJSON TokenValidityUnitsType where
  toJSON TokenValidityUnitsType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccessToken" Core..=) Prelude.<$> accessToken,
            ("IdToken" Core..=) Prelude.<$> idToken,
            ("RefreshToken" Core..=) Prelude.<$> refreshToken
          ]
      )
