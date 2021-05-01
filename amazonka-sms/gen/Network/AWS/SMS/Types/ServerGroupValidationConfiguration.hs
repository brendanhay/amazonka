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
-- Module      : Network.AWS.SMS.Types.ServerGroupValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupValidationConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.ServerValidationConfiguration

-- | Configuration for validating an instance.
--
-- /See:/ 'newServerGroupValidationConfiguration' smart constructor.
data ServerGroupValidationConfiguration = ServerGroupValidationConfiguration'
  { -- | The ID of the server group.
    serverGroupId :: Prelude.Maybe Prelude.Text,
    -- | The validation configuration.
    serverValidationConfigurations :: Prelude.Maybe [ServerValidationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServerGroupValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupId', 'serverGroupValidationConfiguration_serverGroupId' - The ID of the server group.
--
-- 'serverValidationConfigurations', 'serverGroupValidationConfiguration_serverValidationConfigurations' - The validation configuration.
newServerGroupValidationConfiguration ::
  ServerGroupValidationConfiguration
newServerGroupValidationConfiguration =
  ServerGroupValidationConfiguration'
    { serverGroupId =
        Prelude.Nothing,
      serverValidationConfigurations =
        Prelude.Nothing
    }

-- | The ID of the server group.
serverGroupValidationConfiguration_serverGroupId :: Lens.Lens' ServerGroupValidationConfiguration (Prelude.Maybe Prelude.Text)
serverGroupValidationConfiguration_serverGroupId = Lens.lens (\ServerGroupValidationConfiguration' {serverGroupId} -> serverGroupId) (\s@ServerGroupValidationConfiguration' {} a -> s {serverGroupId = a} :: ServerGroupValidationConfiguration)

-- | The validation configuration.
serverGroupValidationConfiguration_serverValidationConfigurations :: Lens.Lens' ServerGroupValidationConfiguration (Prelude.Maybe [ServerValidationConfiguration])
serverGroupValidationConfiguration_serverValidationConfigurations = Lens.lens (\ServerGroupValidationConfiguration' {serverValidationConfigurations} -> serverValidationConfigurations) (\s@ServerGroupValidationConfiguration' {} a -> s {serverValidationConfigurations = a} :: ServerGroupValidationConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    ServerGroupValidationConfiguration
  where
  parseJSON =
    Prelude.withObject
      "ServerGroupValidationConfiguration"
      ( \x ->
          ServerGroupValidationConfiguration'
            Prelude.<$> (x Prelude..:? "serverGroupId")
            Prelude.<*> ( x Prelude..:? "serverValidationConfigurations"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ServerGroupValidationConfiguration

instance
  Prelude.NFData
    ServerGroupValidationConfiguration

instance
  Prelude.ToJSON
    ServerGroupValidationConfiguration
  where
  toJSON ServerGroupValidationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("serverGroupId" Prelude..=)
              Prelude.<$> serverGroupId,
            ("serverValidationConfigurations" Prelude..=)
              Prelude.<$> serverValidationConfigurations
          ]
      )
