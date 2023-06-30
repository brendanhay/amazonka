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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowLambdaParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowLambdaParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for a @LAMBDA@ task type.
--
-- For information about specifying and updating task parameters, see
-- RegisterTaskWithMaintenanceWindow and UpdateMaintenanceWindowTask.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- For Lambda tasks, Systems Manager ignores any values specified for
-- TaskParameters and LoggingInfo.
--
-- /See:/ 'newMaintenanceWindowLambdaParameters' smart constructor.
data MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters'
  { -- | Pass client-specific information to the Lambda function that you are
    -- invoking. You can then process the client information in your Lambda
    -- function as you choose through the context variable.
    clientContext :: Prelude.Maybe Prelude.Text,
    -- | JSON to provide to your Lambda function as input.
    payload :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | (Optional) Specify an Lambda function version or alias name. If you
    -- specify a function version, the operation uses the qualified function
    -- Amazon Resource Name (ARN) to invoke a specific Lambda function. If you
    -- specify an alias name, the operation uses the alias ARN to invoke the
    -- Lambda function version to which the alias points.
    qualifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowLambdaParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientContext', 'maintenanceWindowLambdaParameters_clientContext' - Pass client-specific information to the Lambda function that you are
-- invoking. You can then process the client information in your Lambda
-- function as you choose through the context variable.
--
-- 'payload', 'maintenanceWindowLambdaParameters_payload' - JSON to provide to your Lambda function as input.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'qualifier', 'maintenanceWindowLambdaParameters_qualifier' - (Optional) Specify an Lambda function version or alias name. If you
-- specify a function version, the operation uses the qualified function
-- Amazon Resource Name (ARN) to invoke a specific Lambda function. If you
-- specify an alias name, the operation uses the alias ARN to invoke the
-- Lambda function version to which the alias points.
newMaintenanceWindowLambdaParameters ::
  MaintenanceWindowLambdaParameters
newMaintenanceWindowLambdaParameters =
  MaintenanceWindowLambdaParameters'
    { clientContext =
        Prelude.Nothing,
      payload = Prelude.Nothing,
      qualifier = Prelude.Nothing
    }

-- | Pass client-specific information to the Lambda function that you are
-- invoking. You can then process the client information in your Lambda
-- function as you choose through the context variable.
maintenanceWindowLambdaParameters_clientContext :: Lens.Lens' MaintenanceWindowLambdaParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowLambdaParameters_clientContext = Lens.lens (\MaintenanceWindowLambdaParameters' {clientContext} -> clientContext) (\s@MaintenanceWindowLambdaParameters' {} a -> s {clientContext = a} :: MaintenanceWindowLambdaParameters)

-- | JSON to provide to your Lambda function as input.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
maintenanceWindowLambdaParameters_payload :: Lens.Lens' MaintenanceWindowLambdaParameters (Prelude.Maybe Prelude.ByteString)
maintenanceWindowLambdaParameters_payload = Lens.lens (\MaintenanceWindowLambdaParameters' {payload} -> payload) (\s@MaintenanceWindowLambdaParameters' {} a -> s {payload = a} :: MaintenanceWindowLambdaParameters) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | (Optional) Specify an Lambda function version or alias name. If you
-- specify a function version, the operation uses the qualified function
-- Amazon Resource Name (ARN) to invoke a specific Lambda function. If you
-- specify an alias name, the operation uses the alias ARN to invoke the
-- Lambda function version to which the alias points.
maintenanceWindowLambdaParameters_qualifier :: Lens.Lens' MaintenanceWindowLambdaParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowLambdaParameters_qualifier = Lens.lens (\MaintenanceWindowLambdaParameters' {qualifier} -> qualifier) (\s@MaintenanceWindowLambdaParameters' {} a -> s {qualifier = a} :: MaintenanceWindowLambdaParameters)

instance
  Data.FromJSON
    MaintenanceWindowLambdaParameters
  where
  parseJSON =
    Data.withObject
      "MaintenanceWindowLambdaParameters"
      ( \x ->
          MaintenanceWindowLambdaParameters'
            Prelude.<$> (x Data..:? "ClientContext")
            Prelude.<*> (x Data..:? "Payload")
            Prelude.<*> (x Data..:? "Qualifier")
      )

instance
  Prelude.Hashable
    MaintenanceWindowLambdaParameters
  where
  hashWithSalt
    _salt
    MaintenanceWindowLambdaParameters' {..} =
      _salt
        `Prelude.hashWithSalt` clientContext
        `Prelude.hashWithSalt` payload
        `Prelude.hashWithSalt` qualifier

instance
  Prelude.NFData
    MaintenanceWindowLambdaParameters
  where
  rnf MaintenanceWindowLambdaParameters' {..} =
    Prelude.rnf clientContext
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf qualifier

instance
  Data.ToJSON
    MaintenanceWindowLambdaParameters
  where
  toJSON MaintenanceWindowLambdaParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientContext" Data..=) Prelude.<$> clientContext,
            ("Payload" Data..=) Prelude.<$> payload,
            ("Qualifier" Data..=) Prelude.<$> qualifier
          ]
      )
