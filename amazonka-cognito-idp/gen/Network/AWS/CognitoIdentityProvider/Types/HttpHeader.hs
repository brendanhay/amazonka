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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.HttpHeader
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.HttpHeader where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The HTTP header.
--
-- /See:/ 'newHttpHeader' smart constructor.
data HttpHeader = HttpHeader'
  { -- | The header name
    headerName :: Core.Maybe Core.Text,
    -- | The header value.
    headerValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerName', 'httpHeader_headerName' - The header name
--
-- 'headerValue', 'httpHeader_headerValue' - The header value.
newHttpHeader ::
  HttpHeader
newHttpHeader =
  HttpHeader'
    { headerName = Core.Nothing,
      headerValue = Core.Nothing
    }

-- | The header name
httpHeader_headerName :: Lens.Lens' HttpHeader (Core.Maybe Core.Text)
httpHeader_headerName = Lens.lens (\HttpHeader' {headerName} -> headerName) (\s@HttpHeader' {} a -> s {headerName = a} :: HttpHeader)

-- | The header value.
httpHeader_headerValue :: Lens.Lens' HttpHeader (Core.Maybe Core.Text)
httpHeader_headerValue = Lens.lens (\HttpHeader' {headerValue} -> headerValue) (\s@HttpHeader' {} a -> s {headerValue = a} :: HttpHeader)

instance Core.Hashable HttpHeader

instance Core.NFData HttpHeader

instance Core.ToJSON HttpHeader where
  toJSON HttpHeader' {..} =
    Core.object
      ( Core.catMaybes
          [ ("headerName" Core..=) Core.<$> headerName,
            ("headerValue" Core..=) Core.<$> headerValue
          ]
      )
