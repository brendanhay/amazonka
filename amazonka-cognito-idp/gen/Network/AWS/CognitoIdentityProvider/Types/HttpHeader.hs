{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The HTTP header.
--
-- /See:/ 'newHttpHeader' smart constructor.
data HttpHeader = HttpHeader'
  { -- | The header name
    headerName :: Prelude.Maybe Prelude.Text,
    -- | The header value.
    headerValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { headerName = Prelude.Nothing,
      headerValue = Prelude.Nothing
    }

-- | The header name
httpHeader_headerName :: Lens.Lens' HttpHeader (Prelude.Maybe Prelude.Text)
httpHeader_headerName = Lens.lens (\HttpHeader' {headerName} -> headerName) (\s@HttpHeader' {} a -> s {headerName = a} :: HttpHeader)

-- | The header value.
httpHeader_headerValue :: Lens.Lens' HttpHeader (Prelude.Maybe Prelude.Text)
httpHeader_headerValue = Lens.lens (\HttpHeader' {headerValue} -> headerValue) (\s@HttpHeader' {} a -> s {headerValue = a} :: HttpHeader)

instance Prelude.Hashable HttpHeader

instance Prelude.NFData HttpHeader

instance Prelude.ToJSON HttpHeader where
  toJSON HttpHeader' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("headerName" Prelude..=) Prelude.<$> headerName,
            ("headerValue" Prelude..=) Prelude.<$> headerValue
          ]
      )
