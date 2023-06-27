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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ExchangeCodeForTokenRequestBody
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ExchangeCodeForTokenRequestBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a request to exchange an access code for
-- a token.
--
-- /See:/ 'newExchangeCodeForTokenRequestBody' smart constructor.
data ExchangeCodeForTokenRequestBody = ExchangeCodeForTokenRequestBody'
  { -- | The ID of the client to request the token from.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The access code to send in the request.
    code :: Data.Sensitive Prelude.Text,
    -- | The location of the application that will receive the access code.
    redirectUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExchangeCodeForTokenRequestBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'exchangeCodeForTokenRequestBody_clientId' - The ID of the client to request the token from.
--
-- 'code', 'exchangeCodeForTokenRequestBody_code' - The access code to send in the request.
--
-- 'redirectUri', 'exchangeCodeForTokenRequestBody_redirectUri' - The location of the application that will receive the access code.
newExchangeCodeForTokenRequestBody ::
  -- | 'code'
  Prelude.Text ->
  -- | 'redirectUri'
  Prelude.Text ->
  ExchangeCodeForTokenRequestBody
newExchangeCodeForTokenRequestBody
  pCode_
  pRedirectUri_ =
    ExchangeCodeForTokenRequestBody'
      { clientId =
          Prelude.Nothing,
        code = Data._Sensitive Lens.# pCode_,
        redirectUri = pRedirectUri_
      }

-- | The ID of the client to request the token from.
exchangeCodeForTokenRequestBody_clientId :: Lens.Lens' ExchangeCodeForTokenRequestBody (Prelude.Maybe Prelude.Text)
exchangeCodeForTokenRequestBody_clientId = Lens.lens (\ExchangeCodeForTokenRequestBody' {clientId} -> clientId) (\s@ExchangeCodeForTokenRequestBody' {} a -> s {clientId = a} :: ExchangeCodeForTokenRequestBody) Prelude.. Lens.mapping Data._Sensitive

-- | The access code to send in the request.
exchangeCodeForTokenRequestBody_code :: Lens.Lens' ExchangeCodeForTokenRequestBody Prelude.Text
exchangeCodeForTokenRequestBody_code = Lens.lens (\ExchangeCodeForTokenRequestBody' {code} -> code) (\s@ExchangeCodeForTokenRequestBody' {} a -> s {code = a} :: ExchangeCodeForTokenRequestBody) Prelude.. Data._Sensitive

-- | The location of the application that will receive the access code.
exchangeCodeForTokenRequestBody_redirectUri :: Lens.Lens' ExchangeCodeForTokenRequestBody Prelude.Text
exchangeCodeForTokenRequestBody_redirectUri = Lens.lens (\ExchangeCodeForTokenRequestBody' {redirectUri} -> redirectUri) (\s@ExchangeCodeForTokenRequestBody' {} a -> s {redirectUri = a} :: ExchangeCodeForTokenRequestBody)

instance
  Prelude.Hashable
    ExchangeCodeForTokenRequestBody
  where
  hashWithSalt
    _salt
    ExchangeCodeForTokenRequestBody' {..} =
      _salt
        `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` redirectUri

instance
  Prelude.NFData
    ExchangeCodeForTokenRequestBody
  where
  rnf ExchangeCodeForTokenRequestBody' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf redirectUri

instance Data.ToJSON ExchangeCodeForTokenRequestBody where
  toJSON ExchangeCodeForTokenRequestBody' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientId" Data..=) Prelude.<$> clientId,
            Prelude.Just ("code" Data..= code),
            Prelude.Just ("redirectUri" Data..= redirectUri)
          ]
      )
