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
-- Module      : Amazonka.CognitoIdentityProvider.Types.HttpHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.HttpHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The HTTP header.
--
-- /See:/ 'newHttpHeader' smart constructor.
data HttpHeader = HttpHeader'
  { -- | The header name.
    headerName :: Prelude.Maybe Prelude.Text,
    -- | The header value.
    headerValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerName', 'httpHeader_headerName' - The header name.
--
-- 'headerValue', 'httpHeader_headerValue' - The header value.
newHttpHeader ::
  HttpHeader
newHttpHeader =
  HttpHeader'
    { headerName = Prelude.Nothing,
      headerValue = Prelude.Nothing
    }

-- | The header name.
httpHeader_headerName :: Lens.Lens' HttpHeader (Prelude.Maybe Prelude.Text)
httpHeader_headerName = Lens.lens (\HttpHeader' {headerName} -> headerName) (\s@HttpHeader' {} a -> s {headerName = a} :: HttpHeader)

-- | The header value.
httpHeader_headerValue :: Lens.Lens' HttpHeader (Prelude.Maybe Prelude.Text)
httpHeader_headerValue = Lens.lens (\HttpHeader' {headerValue} -> headerValue) (\s@HttpHeader' {} a -> s {headerValue = a} :: HttpHeader)

instance Prelude.Hashable HttpHeader where
  hashWithSalt _salt HttpHeader' {..} =
    _salt
      `Prelude.hashWithSalt` headerName
      `Prelude.hashWithSalt` headerValue

instance Prelude.NFData HttpHeader where
  rnf HttpHeader' {..} =
    Prelude.rnf headerName
      `Prelude.seq` Prelude.rnf headerValue

instance Data.ToJSON HttpHeader where
  toJSON HttpHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("headerName" Data..=) Prelude.<$> headerName,
            ("headerValue" Data..=) Prelude.<$> headerValue
          ]
      )
