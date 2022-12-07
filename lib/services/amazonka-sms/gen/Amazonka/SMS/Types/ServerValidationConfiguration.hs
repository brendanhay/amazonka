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
-- Module      : Amazonka.SMS.Types.ServerValidationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerValidationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.Server
import Amazonka.SMS.Types.ServerValidationStrategy
import Amazonka.SMS.Types.UserDataValidationParameters

-- | Configuration for validating an instance.
--
-- /See:/ 'newServerValidationConfiguration' smart constructor.
data ServerValidationConfiguration = ServerValidationConfiguration'
  { -- | The name of the configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The validation parameters.
    userDataValidationParameters :: Prelude.Maybe UserDataValidationParameters,
    -- | The ID of the validation.
    validationId :: Prelude.Maybe Prelude.Text,
    -- | The validation strategy.
    serverValidationStrategy :: Prelude.Maybe ServerValidationStrategy,
    server :: Prelude.Maybe Server
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'serverValidationConfiguration_name' - The name of the configuration.
--
-- 'userDataValidationParameters', 'serverValidationConfiguration_userDataValidationParameters' - The validation parameters.
--
-- 'validationId', 'serverValidationConfiguration_validationId' - The ID of the validation.
--
-- 'serverValidationStrategy', 'serverValidationConfiguration_serverValidationStrategy' - The validation strategy.
--
-- 'server', 'serverValidationConfiguration_server' - Undocumented member.
newServerValidationConfiguration ::
  ServerValidationConfiguration
newServerValidationConfiguration =
  ServerValidationConfiguration'
    { name =
        Prelude.Nothing,
      userDataValidationParameters =
        Prelude.Nothing,
      validationId = Prelude.Nothing,
      serverValidationStrategy = Prelude.Nothing,
      server = Prelude.Nothing
    }

-- | The name of the configuration.
serverValidationConfiguration_name :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe Prelude.Text)
serverValidationConfiguration_name = Lens.lens (\ServerValidationConfiguration' {name} -> name) (\s@ServerValidationConfiguration' {} a -> s {name = a} :: ServerValidationConfiguration)

-- | The validation parameters.
serverValidationConfiguration_userDataValidationParameters :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe UserDataValidationParameters)
serverValidationConfiguration_userDataValidationParameters = Lens.lens (\ServerValidationConfiguration' {userDataValidationParameters} -> userDataValidationParameters) (\s@ServerValidationConfiguration' {} a -> s {userDataValidationParameters = a} :: ServerValidationConfiguration)

-- | The ID of the validation.
serverValidationConfiguration_validationId :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe Prelude.Text)
serverValidationConfiguration_validationId = Lens.lens (\ServerValidationConfiguration' {validationId} -> validationId) (\s@ServerValidationConfiguration' {} a -> s {validationId = a} :: ServerValidationConfiguration)

-- | The validation strategy.
serverValidationConfiguration_serverValidationStrategy :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe ServerValidationStrategy)
serverValidationConfiguration_serverValidationStrategy = Lens.lens (\ServerValidationConfiguration' {serverValidationStrategy} -> serverValidationStrategy) (\s@ServerValidationConfiguration' {} a -> s {serverValidationStrategy = a} :: ServerValidationConfiguration)

-- | Undocumented member.
serverValidationConfiguration_server :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe Server)
serverValidationConfiguration_server = Lens.lens (\ServerValidationConfiguration' {server} -> server) (\s@ServerValidationConfiguration' {} a -> s {server = a} :: ServerValidationConfiguration)

instance Data.FromJSON ServerValidationConfiguration where
  parseJSON =
    Data.withObject
      "ServerValidationConfiguration"
      ( \x ->
          ServerValidationConfiguration'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "userDataValidationParameters")
            Prelude.<*> (x Data..:? "validationId")
            Prelude.<*> (x Data..:? "serverValidationStrategy")
            Prelude.<*> (x Data..:? "server")
      )

instance
  Prelude.Hashable
    ServerValidationConfiguration
  where
  hashWithSalt _salt ServerValidationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` userDataValidationParameters
      `Prelude.hashWithSalt` validationId
      `Prelude.hashWithSalt` serverValidationStrategy
      `Prelude.hashWithSalt` server

instance Prelude.NFData ServerValidationConfiguration where
  rnf ServerValidationConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf userDataValidationParameters
      `Prelude.seq` Prelude.rnf validationId
      `Prelude.seq` Prelude.rnf serverValidationStrategy
      `Prelude.seq` Prelude.rnf server

instance Data.ToJSON ServerValidationConfiguration where
  toJSON ServerValidationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("userDataValidationParameters" Data..=)
              Prelude.<$> userDataValidationParameters,
            ("validationId" Data..=) Prelude.<$> validationId,
            ("serverValidationStrategy" Data..=)
              Prelude.<$> serverValidationStrategy,
            ("server" Data..=) Prelude.<$> server
          ]
      )
