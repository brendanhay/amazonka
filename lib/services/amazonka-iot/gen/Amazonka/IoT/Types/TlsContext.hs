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
-- Module      : Amazonka.IoT.Types.TlsContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TlsContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the TLS context to use for the test authorizer request.
--
-- /See:/ 'newTlsContext' smart constructor.
data TlsContext = TlsContext'
  { -- | The value of the @serverName@ key in a TLS authorization request.
    serverName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverName', 'tlsContext_serverName' - The value of the @serverName@ key in a TLS authorization request.
newTlsContext ::
  TlsContext
newTlsContext =
  TlsContext' {serverName = Prelude.Nothing}

-- | The value of the @serverName@ key in a TLS authorization request.
tlsContext_serverName :: Lens.Lens' TlsContext (Prelude.Maybe Prelude.Text)
tlsContext_serverName = Lens.lens (\TlsContext' {serverName} -> serverName) (\s@TlsContext' {} a -> s {serverName = a} :: TlsContext)

instance Prelude.Hashable TlsContext where
  hashWithSalt _salt TlsContext' {..} =
    _salt `Prelude.hashWithSalt` serverName

instance Prelude.NFData TlsContext where
  rnf TlsContext' {..} = Prelude.rnf serverName

instance Data.ToJSON TlsContext where
  toJSON TlsContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [("serverName" Data..=) Prelude.<$> serverName]
      )
