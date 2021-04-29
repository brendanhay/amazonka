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
-- Module      : Network.AWS.SMS.Types.ValidationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ValidationOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.AppValidationOutput
import Network.AWS.SMS.Types.ServerValidationOutput
import Network.AWS.SMS.Types.ValidationStatus

-- | Contains validation output.
--
-- /See:/ 'newValidationOutput' smart constructor.
data ValidationOutput = ValidationOutput'
  { -- | The status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the validation.
    status :: Prelude.Maybe ValidationStatus,
    -- | The ID of the validation.
    validationId :: Prelude.Maybe Prelude.Text,
    -- | The output from validating an application.
    appValidationOutput :: Prelude.Maybe AppValidationOutput,
    -- | The name of the validation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The output from validation an instance.
    serverValidationOutput :: Prelude.Maybe ServerValidationOutput,
    -- | The latest time that the validation was performed.
    latestValidationTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { statusMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      validationId = Prelude.Nothing,
      appValidationOutput = Prelude.Nothing,
      name = Prelude.Nothing,
      serverValidationOutput = Prelude.Nothing,
      latestValidationTime = Prelude.Nothing
    }

-- | The status message.
validationOutput_statusMessage :: Lens.Lens' ValidationOutput (Prelude.Maybe Prelude.Text)
validationOutput_statusMessage = Lens.lens (\ValidationOutput' {statusMessage} -> statusMessage) (\s@ValidationOutput' {} a -> s {statusMessage = a} :: ValidationOutput)

-- | The status of the validation.
validationOutput_status :: Lens.Lens' ValidationOutput (Prelude.Maybe ValidationStatus)
validationOutput_status = Lens.lens (\ValidationOutput' {status} -> status) (\s@ValidationOutput' {} a -> s {status = a} :: ValidationOutput)

-- | The ID of the validation.
validationOutput_validationId :: Lens.Lens' ValidationOutput (Prelude.Maybe Prelude.Text)
validationOutput_validationId = Lens.lens (\ValidationOutput' {validationId} -> validationId) (\s@ValidationOutput' {} a -> s {validationId = a} :: ValidationOutput)

-- | The output from validating an application.
validationOutput_appValidationOutput :: Lens.Lens' ValidationOutput (Prelude.Maybe AppValidationOutput)
validationOutput_appValidationOutput = Lens.lens (\ValidationOutput' {appValidationOutput} -> appValidationOutput) (\s@ValidationOutput' {} a -> s {appValidationOutput = a} :: ValidationOutput)

-- | The name of the validation.
validationOutput_name :: Lens.Lens' ValidationOutput (Prelude.Maybe Prelude.Text)
validationOutput_name = Lens.lens (\ValidationOutput' {name} -> name) (\s@ValidationOutput' {} a -> s {name = a} :: ValidationOutput)

-- | The output from validation an instance.
validationOutput_serverValidationOutput :: Lens.Lens' ValidationOutput (Prelude.Maybe ServerValidationOutput)
validationOutput_serverValidationOutput = Lens.lens (\ValidationOutput' {serverValidationOutput} -> serverValidationOutput) (\s@ValidationOutput' {} a -> s {serverValidationOutput = a} :: ValidationOutput)

-- | The latest time that the validation was performed.
validationOutput_latestValidationTime :: Lens.Lens' ValidationOutput (Prelude.Maybe Prelude.UTCTime)
validationOutput_latestValidationTime = Lens.lens (\ValidationOutput' {latestValidationTime} -> latestValidationTime) (\s@ValidationOutput' {} a -> s {latestValidationTime = a} :: ValidationOutput) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ValidationOutput where
  parseJSON =
    Prelude.withObject
      "ValidationOutput"
      ( \x ->
          ValidationOutput'
            Prelude.<$> (x Prelude..:? "statusMessage")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "validationId")
            Prelude.<*> (x Prelude..:? "appValidationOutput")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "serverValidationOutput")
            Prelude.<*> (x Prelude..:? "latestValidationTime")
      )

instance Prelude.Hashable ValidationOutput

instance Prelude.NFData ValidationOutput
