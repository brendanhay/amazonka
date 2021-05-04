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
-- Module      : Network.AWS.SMS.Types.ServerValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerValidationStrategy
import Network.AWS.SMS.Types.UserDataValidationParameters

-- | Configuration for validating an instance.
--
-- /See:/ 'newServerValidationConfiguration' smart constructor.
data ServerValidationConfiguration = ServerValidationConfiguration'
  { -- | The ID of the validation.
    validationId :: Prelude.Maybe Prelude.Text,
    -- | The validation parameters.
    userDataValidationParameters :: Prelude.Maybe UserDataValidationParameters,
    server :: Prelude.Maybe Server,
    -- | The name of the configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The validation strategy.
    serverValidationStrategy :: Prelude.Maybe ServerValidationStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServerValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationId', 'serverValidationConfiguration_validationId' - The ID of the validation.
--
-- 'userDataValidationParameters', 'serverValidationConfiguration_userDataValidationParameters' - The validation parameters.
--
-- 'server', 'serverValidationConfiguration_server' - Undocumented member.
--
-- 'name', 'serverValidationConfiguration_name' - The name of the configuration.
--
-- 'serverValidationStrategy', 'serverValidationConfiguration_serverValidationStrategy' - The validation strategy.
newServerValidationConfiguration ::
  ServerValidationConfiguration
newServerValidationConfiguration =
  ServerValidationConfiguration'
    { validationId =
        Prelude.Nothing,
      userDataValidationParameters =
        Prelude.Nothing,
      server = Prelude.Nothing,
      name = Prelude.Nothing,
      serverValidationStrategy = Prelude.Nothing
    }

-- | The ID of the validation.
serverValidationConfiguration_validationId :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe Prelude.Text)
serverValidationConfiguration_validationId = Lens.lens (\ServerValidationConfiguration' {validationId} -> validationId) (\s@ServerValidationConfiguration' {} a -> s {validationId = a} :: ServerValidationConfiguration)

-- | The validation parameters.
serverValidationConfiguration_userDataValidationParameters :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe UserDataValidationParameters)
serverValidationConfiguration_userDataValidationParameters = Lens.lens (\ServerValidationConfiguration' {userDataValidationParameters} -> userDataValidationParameters) (\s@ServerValidationConfiguration' {} a -> s {userDataValidationParameters = a} :: ServerValidationConfiguration)

-- | Undocumented member.
serverValidationConfiguration_server :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe Server)
serverValidationConfiguration_server = Lens.lens (\ServerValidationConfiguration' {server} -> server) (\s@ServerValidationConfiguration' {} a -> s {server = a} :: ServerValidationConfiguration)

-- | The name of the configuration.
serverValidationConfiguration_name :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe Prelude.Text)
serverValidationConfiguration_name = Lens.lens (\ServerValidationConfiguration' {name} -> name) (\s@ServerValidationConfiguration' {} a -> s {name = a} :: ServerValidationConfiguration)

-- | The validation strategy.
serverValidationConfiguration_serverValidationStrategy :: Lens.Lens' ServerValidationConfiguration (Prelude.Maybe ServerValidationStrategy)
serverValidationConfiguration_serverValidationStrategy = Lens.lens (\ServerValidationConfiguration' {serverValidationStrategy} -> serverValidationStrategy) (\s@ServerValidationConfiguration' {} a -> s {serverValidationStrategy = a} :: ServerValidationConfiguration)

instance
  Prelude.FromJSON
    ServerValidationConfiguration
  where
  parseJSON =
    Prelude.withObject
      "ServerValidationConfiguration"
      ( \x ->
          ServerValidationConfiguration'
            Prelude.<$> (x Prelude..:? "validationId")
            Prelude.<*> (x Prelude..:? "userDataValidationParameters")
            Prelude.<*> (x Prelude..:? "server")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "serverValidationStrategy")
      )

instance
  Prelude.Hashable
    ServerValidationConfiguration

instance Prelude.NFData ServerValidationConfiguration

instance Prelude.ToJSON ServerValidationConfiguration where
  toJSON ServerValidationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("validationId" Prelude..=)
              Prelude.<$> validationId,
            ("userDataValidationParameters" Prelude..=)
              Prelude.<$> userDataValidationParameters,
            ("server" Prelude..=) Prelude.<$> server,
            ("name" Prelude..=) Prelude.<$> name,
            ("serverValidationStrategy" Prelude..=)
              Prelude.<$> serverValidationStrategy
          ]
      )
