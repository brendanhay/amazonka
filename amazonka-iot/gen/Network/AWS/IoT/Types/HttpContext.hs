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
-- Module      : Network.AWS.IoT.Types.HttpContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HttpContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the HTTP context to use for the test authorizer request.
--
-- /See:/ 'newHttpContext' smart constructor.
data HttpContext = HttpContext'
  { -- | The query string keys and values in an HTTP authorization request.
    queryString :: Core.Maybe Core.Text,
    -- | The header keys and values in an HTTP authorization request.
    headers :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryString', 'httpContext_queryString' - The query string keys and values in an HTTP authorization request.
--
-- 'headers', 'httpContext_headers' - The header keys and values in an HTTP authorization request.
newHttpContext ::
  HttpContext
newHttpContext =
  HttpContext'
    { queryString = Core.Nothing,
      headers = Core.Nothing
    }

-- | The query string keys and values in an HTTP authorization request.
httpContext_queryString :: Lens.Lens' HttpContext (Core.Maybe Core.Text)
httpContext_queryString = Lens.lens (\HttpContext' {queryString} -> queryString) (\s@HttpContext' {} a -> s {queryString = a} :: HttpContext)

-- | The header keys and values in an HTTP authorization request.
httpContext_headers :: Lens.Lens' HttpContext (Core.Maybe (Core.HashMap Core.Text Core.Text))
httpContext_headers = Lens.lens (\HttpContext' {headers} -> headers) (\s@HttpContext' {} a -> s {headers = a} :: HttpContext) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable HttpContext

instance Core.NFData HttpContext

instance Core.ToJSON HttpContext where
  toJSON HttpContext' {..} =
    Core.object
      ( Core.catMaybes
          [ ("queryString" Core..=) Core.<$> queryString,
            ("headers" Core..=) Core.<$> headers
          ]
      )
