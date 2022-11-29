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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.HttpHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The HTTP header.
--
-- /See:/ 'newHttpHeader' smart constructor.
data HttpHeader = HttpHeader'
  { -- | The header value.
    headerValue :: Prelude.Maybe Prelude.Text,
    -- | The header name.
    headerName :: Prelude.Maybe Prelude.Text
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
-- 'headerValue', 'httpHeader_headerValue' - The header value.
--
-- 'headerName', 'httpHeader_headerName' - The header name.
newHttpHeader ::
  HttpHeader
newHttpHeader =
  HttpHeader'
    { headerValue = Prelude.Nothing,
      headerName = Prelude.Nothing
    }

-- | The header value.
httpHeader_headerValue :: Lens.Lens' HttpHeader (Prelude.Maybe Prelude.Text)
httpHeader_headerValue = Lens.lens (\HttpHeader' {headerValue} -> headerValue) (\s@HttpHeader' {} a -> s {headerValue = a} :: HttpHeader)

-- | The header name.
httpHeader_headerName :: Lens.Lens' HttpHeader (Prelude.Maybe Prelude.Text)
httpHeader_headerName = Lens.lens (\HttpHeader' {headerName} -> headerName) (\s@HttpHeader' {} a -> s {headerName = a} :: HttpHeader)

instance Prelude.Hashable HttpHeader where
  hashWithSalt _salt HttpHeader' {..} =
    _salt `Prelude.hashWithSalt` headerValue
      `Prelude.hashWithSalt` headerName

instance Prelude.NFData HttpHeader where
  rnf HttpHeader' {..} =
    Prelude.rnf headerValue
      `Prelude.seq` Prelude.rnf headerName

instance Core.ToJSON HttpHeader where
  toJSON HttpHeader' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("headerValue" Core..=) Prelude.<$> headerValue,
            ("headerName" Core..=) Prelude.<$> headerName
          ]
      )
