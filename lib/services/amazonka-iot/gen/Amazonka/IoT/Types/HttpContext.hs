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
-- Module      : Amazonka.IoT.Types.HttpContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.HttpContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the HTTP context to use for the test authorizer request.
--
-- /See:/ 'newHttpContext' smart constructor.
data HttpContext = HttpContext'
  { -- | The header keys and values in an HTTP authorization request.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The query string keys and values in an HTTP authorization request.
    queryString :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'httpContext_headers' - The header keys and values in an HTTP authorization request.
--
-- 'queryString', 'httpContext_queryString' - The query string keys and values in an HTTP authorization request.
newHttpContext ::
  HttpContext
newHttpContext =
  HttpContext'
    { headers = Prelude.Nothing,
      queryString = Prelude.Nothing
    }

-- | The header keys and values in an HTTP authorization request.
httpContext_headers :: Lens.Lens' HttpContext (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
httpContext_headers = Lens.lens (\HttpContext' {headers} -> headers) (\s@HttpContext' {} a -> s {headers = a} :: HttpContext) Prelude.. Lens.mapping Lens.coerced

-- | The query string keys and values in an HTTP authorization request.
httpContext_queryString :: Lens.Lens' HttpContext (Prelude.Maybe Prelude.Text)
httpContext_queryString = Lens.lens (\HttpContext' {queryString} -> queryString) (\s@HttpContext' {} a -> s {queryString = a} :: HttpContext)

instance Prelude.Hashable HttpContext where
  hashWithSalt _salt HttpContext' {..} =
    _salt `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData HttpContext where
  rnf HttpContext' {..} =
    Prelude.rnf headers
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToJSON HttpContext where
  toJSON HttpContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("headers" Data..=) Prelude.<$> headers,
            ("queryString" Data..=) Prelude.<$> queryString
          ]
      )
