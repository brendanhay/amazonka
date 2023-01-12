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
-- Module      : Amazonka.AppFlow.Types.OAuth2Properties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OAuth2Properties where

import Amazonka.AppFlow.Types.OAuth2GrantType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The OAuth 2.0 properties required for OAuth 2.0 authentication.
--
-- /See:/ 'newOAuth2Properties' smart constructor.
data OAuth2Properties = OAuth2Properties'
  { -- | Associates your token URL with a map of properties that you define. Use
    -- this parameter to provide any additional details that the connector
    -- requires to authenticate your request.
    tokenUrlCustomProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The token URL required for OAuth 2.0 authentication.
    tokenUrl :: Prelude.Text,
    -- | The OAuth 2.0 grant type used by connector for OAuth 2.0 authentication.
    oAuth2GrantType :: OAuth2GrantType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OAuth2Properties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenUrlCustomProperties', 'oAuth2Properties_tokenUrlCustomProperties' - Associates your token URL with a map of properties that you define. Use
-- this parameter to provide any additional details that the connector
-- requires to authenticate your request.
--
-- 'tokenUrl', 'oAuth2Properties_tokenUrl' - The token URL required for OAuth 2.0 authentication.
--
-- 'oAuth2GrantType', 'oAuth2Properties_oAuth2GrantType' - The OAuth 2.0 grant type used by connector for OAuth 2.0 authentication.
newOAuth2Properties ::
  -- | 'tokenUrl'
  Prelude.Text ->
  -- | 'oAuth2GrantType'
  OAuth2GrantType ->
  OAuth2Properties
newOAuth2Properties pTokenUrl_ pOAuth2GrantType_ =
  OAuth2Properties'
    { tokenUrlCustomProperties =
        Prelude.Nothing,
      tokenUrl = pTokenUrl_,
      oAuth2GrantType = pOAuth2GrantType_
    }

-- | Associates your token URL with a map of properties that you define. Use
-- this parameter to provide any additional details that the connector
-- requires to authenticate your request.
oAuth2Properties_tokenUrlCustomProperties :: Lens.Lens' OAuth2Properties (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
oAuth2Properties_tokenUrlCustomProperties = Lens.lens (\OAuth2Properties' {tokenUrlCustomProperties} -> tokenUrlCustomProperties) (\s@OAuth2Properties' {} a -> s {tokenUrlCustomProperties = a} :: OAuth2Properties) Prelude.. Lens.mapping Lens.coerced

-- | The token URL required for OAuth 2.0 authentication.
oAuth2Properties_tokenUrl :: Lens.Lens' OAuth2Properties Prelude.Text
oAuth2Properties_tokenUrl = Lens.lens (\OAuth2Properties' {tokenUrl} -> tokenUrl) (\s@OAuth2Properties' {} a -> s {tokenUrl = a} :: OAuth2Properties)

-- | The OAuth 2.0 grant type used by connector for OAuth 2.0 authentication.
oAuth2Properties_oAuth2GrantType :: Lens.Lens' OAuth2Properties OAuth2GrantType
oAuth2Properties_oAuth2GrantType = Lens.lens (\OAuth2Properties' {oAuth2GrantType} -> oAuth2GrantType) (\s@OAuth2Properties' {} a -> s {oAuth2GrantType = a} :: OAuth2Properties)

instance Data.FromJSON OAuth2Properties where
  parseJSON =
    Data.withObject
      "OAuth2Properties"
      ( \x ->
          OAuth2Properties'
            Prelude.<$> ( x Data..:? "tokenUrlCustomProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "tokenUrl")
            Prelude.<*> (x Data..: "oAuth2GrantType")
      )

instance Prelude.Hashable OAuth2Properties where
  hashWithSalt _salt OAuth2Properties' {..} =
    _salt
      `Prelude.hashWithSalt` tokenUrlCustomProperties
      `Prelude.hashWithSalt` tokenUrl
      `Prelude.hashWithSalt` oAuth2GrantType

instance Prelude.NFData OAuth2Properties where
  rnf OAuth2Properties' {..} =
    Prelude.rnf tokenUrlCustomProperties
      `Prelude.seq` Prelude.rnf tokenUrl
      `Prelude.seq` Prelude.rnf oAuth2GrantType

instance Data.ToJSON OAuth2Properties where
  toJSON OAuth2Properties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tokenUrlCustomProperties" Data..=)
              Prelude.<$> tokenUrlCustomProperties,
            Prelude.Just ("tokenUrl" Data..= tokenUrl),
            Prelude.Just
              ("oAuth2GrantType" Data..= oAuth2GrantType)
          ]
      )
