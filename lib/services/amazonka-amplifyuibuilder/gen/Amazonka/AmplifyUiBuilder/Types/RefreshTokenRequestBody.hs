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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.RefreshTokenRequestBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a refresh token.
--
-- /See:/ 'newRefreshTokenRequestBody' smart constructor.
data RefreshTokenRequestBody = RefreshTokenRequestBody'
  { -- | The token to use to refresh a previously issued access token that might
    -- have expired.
    token :: Core.Sensitive Prelude.Text
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
-- 'token', 'refreshTokenRequestBody_token' - The token to use to refresh a previously issued access token that might
-- have expired.
newRefreshTokenRequestBody ::
  -- | 'token'
  Prelude.Text ->
  RefreshTokenRequestBody
newRefreshTokenRequestBody pToken_ =
  RefreshTokenRequestBody'
    { token =
        Core._Sensitive Lens.# pToken_
    }

-- | The token to use to refresh a previously issued access token that might
-- have expired.
refreshTokenRequestBody_token :: Lens.Lens' RefreshTokenRequestBody Prelude.Text
refreshTokenRequestBody_token = Lens.lens (\RefreshTokenRequestBody' {token} -> token) (\s@RefreshTokenRequestBody' {} a -> s {token = a} :: RefreshTokenRequestBody) Prelude.. Core._Sensitive

instance Prelude.Hashable RefreshTokenRequestBody where
  hashWithSalt _salt RefreshTokenRequestBody' {..} =
    _salt `Prelude.hashWithSalt` token

instance Prelude.NFData RefreshTokenRequestBody where
  rnf RefreshTokenRequestBody' {..} = Prelude.rnf token

instance Core.ToJSON RefreshTokenRequestBody where
  toJSON RefreshTokenRequestBody' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("token" Core..= token)]
      )
