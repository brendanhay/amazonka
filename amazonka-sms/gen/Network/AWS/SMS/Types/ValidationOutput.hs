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
-- Module      : Network.AWS.SMS.Types.ValidationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ValidationOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.AppValidationOutput
import Network.AWS.SMS.Types.ServerValidationOutput
import Network.AWS.SMS.Types.ValidationStatus

-- | Contains validation output.
--
-- /See:/ 'newValidationOutput' smart constructor.
data ValidationOutput = ValidationOutput'
  { -- | The status message.
    statusMessage :: Core.Maybe Core.Text,
    -- | The status of the validation.
    status :: Core.Maybe ValidationStatus,
    -- | The ID of the validation.
    validationId :: Core.Maybe Core.Text,
    -- | The output from validating an application.
    appValidationOutput :: Core.Maybe AppValidationOutput,
    -- | The name of the validation.
    name :: Core.Maybe Core.Text,
    -- | The output from validation an instance.
    serverValidationOutput :: Core.Maybe ServerValidationOutput,
    -- | The latest time that the validation was performed.
    latestValidationTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'validationOutput_statusMessage' - The status message.
--
-- 'status', 'validationOutput_status' - The status of the validation.
--
-- 'validationId', 'validationOutput_validationId' - The ID of the validation.
--
-- 'appValidationOutput', 'validationOutput_appValidationOutput' - The output from validating an application.
--
-- 'name', 'validationOutput_name' - The name of the validation.
--
-- 'serverValidationOutput', 'validationOutput_serverValidationOutput' - The output from validation an instance.
--
-- 'latestValidationTime', 'validationOutput_latestValidationTime' - The latest time that the validation was performed.
newValidationOutput ::
  ValidationOutput
newValidationOutput =
  ValidationOutput'
    { statusMessage = Core.Nothing,
      status = Core.Nothing,
      validationId = Core.Nothing,
      appValidationOutput = Core.Nothing,
      name = Core.Nothing,
      serverValidationOutput = Core.Nothing,
      latestValidationTime = Core.Nothing
    }

-- | The status message.
validationOutput_statusMessage :: Lens.Lens' ValidationOutput (Core.Maybe Core.Text)
validationOutput_statusMessage = Lens.lens (\ValidationOutput' {statusMessage} -> statusMessage) (\s@ValidationOutput' {} a -> s {statusMessage = a} :: ValidationOutput)

-- | The status of the validation.
validationOutput_status :: Lens.Lens' ValidationOutput (Core.Maybe ValidationStatus)
validationOutput_status = Lens.lens (\ValidationOutput' {status} -> status) (\s@ValidationOutput' {} a -> s {status = a} :: ValidationOutput)

-- | The ID of the validation.
validationOutput_validationId :: Lens.Lens' ValidationOutput (Core.Maybe Core.Text)
validationOutput_validationId = Lens.lens (\ValidationOutput' {validationId} -> validationId) (\s@ValidationOutput' {} a -> s {validationId = a} :: ValidationOutput)

-- | The output from validating an application.
validationOutput_appValidationOutput :: Lens.Lens' ValidationOutput (Core.Maybe AppValidationOutput)
validationOutput_appValidationOutput = Lens.lens (\ValidationOutput' {appValidationOutput} -> appValidationOutput) (\s@ValidationOutput' {} a -> s {appValidationOutput = a} :: ValidationOutput)

-- | The name of the validation.
validationOutput_name :: Lens.Lens' ValidationOutput (Core.Maybe Core.Text)
validationOutput_name = Lens.lens (\ValidationOutput' {name} -> name) (\s@ValidationOutput' {} a -> s {name = a} :: ValidationOutput)

-- | The output from validation an instance.
validationOutput_serverValidationOutput :: Lens.Lens' ValidationOutput (Core.Maybe ServerValidationOutput)
validationOutput_serverValidationOutput = Lens.lens (\ValidationOutput' {serverValidationOutput} -> serverValidationOutput) (\s@ValidationOutput' {} a -> s {serverValidationOutput = a} :: ValidationOutput)

-- | The latest time that the validation was performed.
validationOutput_latestValidationTime :: Lens.Lens' ValidationOutput (Core.Maybe Core.UTCTime)
validationOutput_latestValidationTime = Lens.lens (\ValidationOutput' {latestValidationTime} -> latestValidationTime) (\s@ValidationOutput' {} a -> s {latestValidationTime = a} :: ValidationOutput) Core.. Lens.mapping Core._Time

instance Core.FromJSON ValidationOutput where
  parseJSON =
    Core.withObject
      "ValidationOutput"
      ( \x ->
          ValidationOutput'
            Core.<$> (x Core..:? "statusMessage")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "validationId")
            Core.<*> (x Core..:? "appValidationOutput")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "serverValidationOutput")
            Core.<*> (x Core..:? "latestValidationTime")
      )

instance Core.Hashable ValidationOutput

instance Core.NFData ValidationOutput
