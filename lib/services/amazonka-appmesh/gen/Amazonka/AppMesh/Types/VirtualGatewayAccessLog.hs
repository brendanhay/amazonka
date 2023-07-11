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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayAccessLog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayAccessLog where

import Amazonka.AppMesh.Types.VirtualGatewayFileAccessLog
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The access log configuration for a virtual gateway.
--
-- /See:/ 'newVirtualGatewayAccessLog' smart constructor.
data VirtualGatewayAccessLog = VirtualGatewayAccessLog'
  { -- | The file object to send virtual gateway access logs to.
    file :: Prelude.Maybe VirtualGatewayFileAccessLog
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayAccessLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'file', 'virtualGatewayAccessLog_file' - The file object to send virtual gateway access logs to.
newVirtualGatewayAccessLog ::
  VirtualGatewayAccessLog
newVirtualGatewayAccessLog =
  VirtualGatewayAccessLog' {file = Prelude.Nothing}

-- | The file object to send virtual gateway access logs to.
virtualGatewayAccessLog_file :: Lens.Lens' VirtualGatewayAccessLog (Prelude.Maybe VirtualGatewayFileAccessLog)
virtualGatewayAccessLog_file = Lens.lens (\VirtualGatewayAccessLog' {file} -> file) (\s@VirtualGatewayAccessLog' {} a -> s {file = a} :: VirtualGatewayAccessLog)

instance Data.FromJSON VirtualGatewayAccessLog where
  parseJSON =
    Data.withObject
      "VirtualGatewayAccessLog"
      ( \x ->
          VirtualGatewayAccessLog'
            Prelude.<$> (x Data..:? "file")
      )

instance Prelude.Hashable VirtualGatewayAccessLog where
  hashWithSalt _salt VirtualGatewayAccessLog' {..} =
    _salt `Prelude.hashWithSalt` file

instance Prelude.NFData VirtualGatewayAccessLog where
  rnf VirtualGatewayAccessLog' {..} = Prelude.rnf file

instance Data.ToJSON VirtualGatewayAccessLog where
  toJSON VirtualGatewayAccessLog' {..} =
    Data.object
      ( Prelude.catMaybes
          [("file" Data..=) Prelude.<$> file]
      )
