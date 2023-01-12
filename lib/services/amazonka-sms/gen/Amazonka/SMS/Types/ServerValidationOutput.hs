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
-- Module      : Amazonka.SMS.Types.ServerValidationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerValidationOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.Server

-- | Contains output from validating an instance.
--
-- /See:/ 'newServerValidationOutput' smart constructor.
data ServerValidationOutput = ServerValidationOutput'
  { server :: Prelude.Maybe Server
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerValidationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'serverValidationOutput_server' - Undocumented member.
newServerValidationOutput ::
  ServerValidationOutput
newServerValidationOutput =
  ServerValidationOutput' {server = Prelude.Nothing}

-- | Undocumented member.
serverValidationOutput_server :: Lens.Lens' ServerValidationOutput (Prelude.Maybe Server)
serverValidationOutput_server = Lens.lens (\ServerValidationOutput' {server} -> server) (\s@ServerValidationOutput' {} a -> s {server = a} :: ServerValidationOutput)

instance Data.FromJSON ServerValidationOutput where
  parseJSON =
    Data.withObject
      "ServerValidationOutput"
      ( \x ->
          ServerValidationOutput'
            Prelude.<$> (x Data..:? "server")
      )

instance Prelude.Hashable ServerValidationOutput where
  hashWithSalt _salt ServerValidationOutput' {..} =
    _salt `Prelude.hashWithSalt` server

instance Prelude.NFData ServerValidationOutput where
  rnf ServerValidationOutput' {..} = Prelude.rnf server
