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
-- Module      : Amazonka.AppFlow.Types.ConnectorProvisioningConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProvisioningConfig where

import Amazonka.AppFlow.Types.LambdaConnectorProvisioningConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the configuration of the connector being
-- registered.
--
-- /See:/ 'newConnectorProvisioningConfig' smart constructor.
data ConnectorProvisioningConfig = ConnectorProvisioningConfig'
  { -- | Contains information about the configuration of the lambda which is
    -- being registered as the connector.
    lambda :: Prelude.Maybe LambdaConnectorProvisioningConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorProvisioningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambda', 'connectorProvisioningConfig_lambda' - Contains information about the configuration of the lambda which is
-- being registered as the connector.
newConnectorProvisioningConfig ::
  ConnectorProvisioningConfig
newConnectorProvisioningConfig =
  ConnectorProvisioningConfig'
    { lambda =
        Prelude.Nothing
    }

-- | Contains information about the configuration of the lambda which is
-- being registered as the connector.
connectorProvisioningConfig_lambda :: Lens.Lens' ConnectorProvisioningConfig (Prelude.Maybe LambdaConnectorProvisioningConfig)
connectorProvisioningConfig_lambda = Lens.lens (\ConnectorProvisioningConfig' {lambda} -> lambda) (\s@ConnectorProvisioningConfig' {} a -> s {lambda = a} :: ConnectorProvisioningConfig)

instance Core.FromJSON ConnectorProvisioningConfig where
  parseJSON =
    Core.withObject
      "ConnectorProvisioningConfig"
      ( \x ->
          ConnectorProvisioningConfig'
            Prelude.<$> (x Core..:? "lambda")
      )

instance Prelude.Hashable ConnectorProvisioningConfig where
  hashWithSalt _salt ConnectorProvisioningConfig' {..} =
    _salt `Prelude.hashWithSalt` lambda

instance Prelude.NFData ConnectorProvisioningConfig where
  rnf ConnectorProvisioningConfig' {..} =
    Prelude.rnf lambda

instance Core.ToJSON ConnectorProvisioningConfig where
  toJSON ConnectorProvisioningConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [("lambda" Core..=) Prelude.<$> lambda]
      )
