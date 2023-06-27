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
-- Module      : Amazonka.AmplifyUiBuilder.Types.RefreshTokenRequestBody
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.RefreshTokenRequestBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a refresh token.
--
-- /See:/ 'newRefreshTokenRequestBody' smart constructor.
data RefreshTokenRequestBody = RefreshTokenRequestBody'
  { -- | The ID of the client to request the token from.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The token to use to refresh a previously issued access token that might
    -- have expired.
    token :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshTokenRequestBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'refreshTokenRequestBody_clientId' - The ID of the client to request the token from.
--
-- 'token', 'refreshTokenRequestBody_token' - The token to use to refresh a previously issued access token that might
-- have expired.
newRefreshTokenRequestBody ::
  -- | 'token'
  Prelude.Text ->
  RefreshTokenRequestBody
newRefreshTokenRequestBody pToken_ =
  RefreshTokenRequestBody'
    { clientId =
        Prelude.Nothing,
      token = Data._Sensitive Lens.# pToken_
    }

-- | The ID of the client to request the token from.
refreshTokenRequestBody_clientId :: Lens.Lens' RefreshTokenRequestBody (Prelude.Maybe Prelude.Text)
refreshTokenRequestBody_clientId = Lens.lens (\RefreshTokenRequestBody' {clientId} -> clientId) (\s@RefreshTokenRequestBody' {} a -> s {clientId = a} :: RefreshTokenRequestBody) Prelude.. Lens.mapping Data._Sensitive

-- | The token to use to refresh a previously issued access token that might
-- have expired.
refreshTokenRequestBody_token :: Lens.Lens' RefreshTokenRequestBody Prelude.Text
refreshTokenRequestBody_token = Lens.lens (\RefreshTokenRequestBody' {token} -> token) (\s@RefreshTokenRequestBody' {} a -> s {token = a} :: RefreshTokenRequestBody) Prelude.. Data._Sensitive

instance Prelude.Hashable RefreshTokenRequestBody where
  hashWithSalt _salt RefreshTokenRequestBody' {..} =
    _salt
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` token

instance Prelude.NFData RefreshTokenRequestBody where
  rnf RefreshTokenRequestBody' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf token

instance Data.ToJSON RefreshTokenRequestBody where
  toJSON RefreshTokenRequestBody' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientId" Data..=) Prelude.<$> clientId,
            Prelude.Just ("token" Data..= token)
          ]
      )
