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
-- Module      : Network.AWS.IoT.Types.TlsContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TlsContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the TLS context to use for the test authorizer request.
--
-- /See:/ 'newTlsContext' smart constructor.
data TlsContext = TlsContext'
  { -- | The value of the @serverName@ key in a TLS authorization request.
    serverName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  TlsContext' {serverName = Core.Nothing}

-- | The value of the @serverName@ key in a TLS authorization request.
tlsContext_serverName :: Lens.Lens' TlsContext (Core.Maybe Core.Text)
tlsContext_serverName = Lens.lens (\TlsContext' {serverName} -> serverName) (\s@TlsContext' {} a -> s {serverName = a} :: TlsContext)

instance Core.Hashable TlsContext

instance Core.NFData TlsContext

instance Core.ToJSON TlsContext where
  toJSON TlsContext' {..} =
    Core.object
      ( Core.catMaybes
          [("serverName" Core..=) Core.<$> serverName]
      )
