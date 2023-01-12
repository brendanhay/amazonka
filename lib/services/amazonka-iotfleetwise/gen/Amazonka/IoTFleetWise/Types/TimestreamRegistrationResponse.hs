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
-- Module      : Amazonka.IoTFleetWise.Types.TimestreamRegistrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.TimestreamRegistrationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.RegistrationStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the registered Amazon Timestream resources or errors,
-- if any.
--
-- /See:/ 'newTimestreamRegistrationResponse' smart constructor.
data TimestreamRegistrationResponse = TimestreamRegistrationResponse'
  { -- | A message associated with a registration error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Timestream database.
    timestreamDatabaseArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Timestream database table.
    timestreamTableArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Timestream database.
    timestreamDatabaseName :: Prelude.Text,
    -- | The name of the Timestream database table.
    timestreamTableName :: Prelude.Text,
    -- | The status of registering your Amazon Timestream resources. The status
    -- can be one of @REGISTRATION_SUCCESS@, @REGISTRATION_PENDING@,
    -- @REGISTRATION_FAILURE@.
    registrationStatus :: RegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestreamRegistrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'timestreamRegistrationResponse_errorMessage' - A message associated with a registration error.
--
-- 'timestreamDatabaseArn', 'timestreamRegistrationResponse_timestreamDatabaseArn' - The Amazon Resource Name (ARN) of the Timestream database.
--
-- 'timestreamTableArn', 'timestreamRegistrationResponse_timestreamTableArn' - The ARN of the Timestream database table.
--
-- 'timestreamDatabaseName', 'timestreamRegistrationResponse_timestreamDatabaseName' - The name of the Timestream database.
--
-- 'timestreamTableName', 'timestreamRegistrationResponse_timestreamTableName' - The name of the Timestream database table.
--
-- 'registrationStatus', 'timestreamRegistrationResponse_registrationStatus' - The status of registering your Amazon Timestream resources. The status
-- can be one of @REGISTRATION_SUCCESS@, @REGISTRATION_PENDING@,
-- @REGISTRATION_FAILURE@.
newTimestreamRegistrationResponse ::
  -- | 'timestreamDatabaseName'
  Prelude.Text ->
  -- | 'timestreamTableName'
  Prelude.Text ->
  -- | 'registrationStatus'
  RegistrationStatus ->
  TimestreamRegistrationResponse
newTimestreamRegistrationResponse
  pTimestreamDatabaseName_
  pTimestreamTableName_
  pRegistrationStatus_ =
    TimestreamRegistrationResponse'
      { errorMessage =
          Prelude.Nothing,
        timestreamDatabaseArn = Prelude.Nothing,
        timestreamTableArn = Prelude.Nothing,
        timestreamDatabaseName =
          pTimestreamDatabaseName_,
        timestreamTableName = pTimestreamTableName_,
        registrationStatus = pRegistrationStatus_
      }

-- | A message associated with a registration error.
timestreamRegistrationResponse_errorMessage :: Lens.Lens' TimestreamRegistrationResponse (Prelude.Maybe Prelude.Text)
timestreamRegistrationResponse_errorMessage = Lens.lens (\TimestreamRegistrationResponse' {errorMessage} -> errorMessage) (\s@TimestreamRegistrationResponse' {} a -> s {errorMessage = a} :: TimestreamRegistrationResponse)

-- | The Amazon Resource Name (ARN) of the Timestream database.
timestreamRegistrationResponse_timestreamDatabaseArn :: Lens.Lens' TimestreamRegistrationResponse (Prelude.Maybe Prelude.Text)
timestreamRegistrationResponse_timestreamDatabaseArn = Lens.lens (\TimestreamRegistrationResponse' {timestreamDatabaseArn} -> timestreamDatabaseArn) (\s@TimestreamRegistrationResponse' {} a -> s {timestreamDatabaseArn = a} :: TimestreamRegistrationResponse)

-- | The ARN of the Timestream database table.
timestreamRegistrationResponse_timestreamTableArn :: Lens.Lens' TimestreamRegistrationResponse (Prelude.Maybe Prelude.Text)
timestreamRegistrationResponse_timestreamTableArn = Lens.lens (\TimestreamRegistrationResponse' {timestreamTableArn} -> timestreamTableArn) (\s@TimestreamRegistrationResponse' {} a -> s {timestreamTableArn = a} :: TimestreamRegistrationResponse)

-- | The name of the Timestream database.
timestreamRegistrationResponse_timestreamDatabaseName :: Lens.Lens' TimestreamRegistrationResponse Prelude.Text
timestreamRegistrationResponse_timestreamDatabaseName = Lens.lens (\TimestreamRegistrationResponse' {timestreamDatabaseName} -> timestreamDatabaseName) (\s@TimestreamRegistrationResponse' {} a -> s {timestreamDatabaseName = a} :: TimestreamRegistrationResponse)

-- | The name of the Timestream database table.
timestreamRegistrationResponse_timestreamTableName :: Lens.Lens' TimestreamRegistrationResponse Prelude.Text
timestreamRegistrationResponse_timestreamTableName = Lens.lens (\TimestreamRegistrationResponse' {timestreamTableName} -> timestreamTableName) (\s@TimestreamRegistrationResponse' {} a -> s {timestreamTableName = a} :: TimestreamRegistrationResponse)

-- | The status of registering your Amazon Timestream resources. The status
-- can be one of @REGISTRATION_SUCCESS@, @REGISTRATION_PENDING@,
-- @REGISTRATION_FAILURE@.
timestreamRegistrationResponse_registrationStatus :: Lens.Lens' TimestreamRegistrationResponse RegistrationStatus
timestreamRegistrationResponse_registrationStatus = Lens.lens (\TimestreamRegistrationResponse' {registrationStatus} -> registrationStatus) (\s@TimestreamRegistrationResponse' {} a -> s {registrationStatus = a} :: TimestreamRegistrationResponse)

instance Data.FromJSON TimestreamRegistrationResponse where
  parseJSON =
    Data.withObject
      "TimestreamRegistrationResponse"
      ( \x ->
          TimestreamRegistrationResponse'
            Prelude.<$> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "timestreamDatabaseArn")
            Prelude.<*> (x Data..:? "timestreamTableArn")
            Prelude.<*> (x Data..: "timestreamDatabaseName")
            Prelude.<*> (x Data..: "timestreamTableName")
            Prelude.<*> (x Data..: "registrationStatus")
      )

instance
  Prelude.Hashable
    TimestreamRegistrationResponse
  where
  hashWithSalt
    _salt
    TimestreamRegistrationResponse' {..} =
      _salt `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` timestreamDatabaseArn
        `Prelude.hashWithSalt` timestreamTableArn
        `Prelude.hashWithSalt` timestreamDatabaseName
        `Prelude.hashWithSalt` timestreamTableName
        `Prelude.hashWithSalt` registrationStatus

instance
  Prelude.NFData
    TimestreamRegistrationResponse
  where
  rnf TimestreamRegistrationResponse' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf timestreamDatabaseArn
      `Prelude.seq` Prelude.rnf timestreamTableArn
      `Prelude.seq` Prelude.rnf timestreamDatabaseName
      `Prelude.seq` Prelude.rnf timestreamTableName
      `Prelude.seq` Prelude.rnf registrationStatus
