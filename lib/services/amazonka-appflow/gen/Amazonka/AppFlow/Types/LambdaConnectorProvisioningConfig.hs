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
-- Module      : Amazonka.AppFlow.Types.LambdaConnectorProvisioningConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.LambdaConnectorProvisioningConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the configuration of the lambda which is
-- being registered as the connector.
--
-- /See:/ 'newLambdaConnectorProvisioningConfig' smart constructor.
data LambdaConnectorProvisioningConfig = LambdaConnectorProvisioningConfig'
  { -- | Lambda ARN of the connector being registered.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaConnectorProvisioningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaArn', 'lambdaConnectorProvisioningConfig_lambdaArn' - Lambda ARN of the connector being registered.
newLambdaConnectorProvisioningConfig ::
  -- | 'lambdaArn'
  Prelude.Text ->
  LambdaConnectorProvisioningConfig
newLambdaConnectorProvisioningConfig pLambdaArn_ =
  LambdaConnectorProvisioningConfig'
    { lambdaArn =
        pLambdaArn_
    }

-- | Lambda ARN of the connector being registered.
lambdaConnectorProvisioningConfig_lambdaArn :: Lens.Lens' LambdaConnectorProvisioningConfig Prelude.Text
lambdaConnectorProvisioningConfig_lambdaArn = Lens.lens (\LambdaConnectorProvisioningConfig' {lambdaArn} -> lambdaArn) (\s@LambdaConnectorProvisioningConfig' {} a -> s {lambdaArn = a} :: LambdaConnectorProvisioningConfig)

instance
  Data.FromJSON
    LambdaConnectorProvisioningConfig
  where
  parseJSON =
    Data.withObject
      "LambdaConnectorProvisioningConfig"
      ( \x ->
          LambdaConnectorProvisioningConfig'
            Prelude.<$> (x Data..: "lambdaArn")
      )

instance
  Prelude.Hashable
    LambdaConnectorProvisioningConfig
  where
  hashWithSalt
    _salt
    LambdaConnectorProvisioningConfig' {..} =
      _salt `Prelude.hashWithSalt` lambdaArn

instance
  Prelude.NFData
    LambdaConnectorProvisioningConfig
  where
  rnf LambdaConnectorProvisioningConfig' {..} =
    Prelude.rnf lambdaArn

instance
  Data.ToJSON
    LambdaConnectorProvisioningConfig
  where
  toJSON LambdaConnectorProvisioningConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("lambdaArn" Data..= lambdaArn)]
      )
