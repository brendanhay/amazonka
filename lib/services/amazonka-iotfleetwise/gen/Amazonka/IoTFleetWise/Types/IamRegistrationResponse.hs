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
-- Module      : Amazonka.IoTFleetWise.Types.IamRegistrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.IamRegistrationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.RegistrationStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about registering an Identity and Access Management (IAM)
-- resource so Amazon Web Services IoT FleetWise edge agent software can
-- transfer your vehicle data to Amazon Timestream.
--
-- /See:/ 'newIamRegistrationResponse' smart constructor.
data IamRegistrationResponse = IamRegistrationResponse'
  { -- | A message associated with a registration error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to register.
    roleArn :: Prelude.Text,
    -- | The status of registering your IAM resource. The status can be one of
    -- @REGISTRATION_SUCCESS@, @REGISTRATION_PENDING@, @REGISTRATION_FAILURE@.
    registrationStatus :: RegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamRegistrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'iamRegistrationResponse_errorMessage' - A message associated with a registration error.
--
-- 'roleArn', 'iamRegistrationResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role to register.
--
-- 'registrationStatus', 'iamRegistrationResponse_registrationStatus' - The status of registering your IAM resource. The status can be one of
-- @REGISTRATION_SUCCESS@, @REGISTRATION_PENDING@, @REGISTRATION_FAILURE@.
newIamRegistrationResponse ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'registrationStatus'
  RegistrationStatus ->
  IamRegistrationResponse
newIamRegistrationResponse
  pRoleArn_
  pRegistrationStatus_ =
    IamRegistrationResponse'
      { errorMessage =
          Prelude.Nothing,
        roleArn = pRoleArn_,
        registrationStatus = pRegistrationStatus_
      }

-- | A message associated with a registration error.
iamRegistrationResponse_errorMessage :: Lens.Lens' IamRegistrationResponse (Prelude.Maybe Prelude.Text)
iamRegistrationResponse_errorMessage = Lens.lens (\IamRegistrationResponse' {errorMessage} -> errorMessage) (\s@IamRegistrationResponse' {} a -> s {errorMessage = a} :: IamRegistrationResponse)

-- | The Amazon Resource Name (ARN) of the IAM role to register.
iamRegistrationResponse_roleArn :: Lens.Lens' IamRegistrationResponse Prelude.Text
iamRegistrationResponse_roleArn = Lens.lens (\IamRegistrationResponse' {roleArn} -> roleArn) (\s@IamRegistrationResponse' {} a -> s {roleArn = a} :: IamRegistrationResponse)

-- | The status of registering your IAM resource. The status can be one of
-- @REGISTRATION_SUCCESS@, @REGISTRATION_PENDING@, @REGISTRATION_FAILURE@.
iamRegistrationResponse_registrationStatus :: Lens.Lens' IamRegistrationResponse RegistrationStatus
iamRegistrationResponse_registrationStatus = Lens.lens (\IamRegistrationResponse' {registrationStatus} -> registrationStatus) (\s@IamRegistrationResponse' {} a -> s {registrationStatus = a} :: IamRegistrationResponse)

instance Data.FromJSON IamRegistrationResponse where
  parseJSON =
    Data.withObject
      "IamRegistrationResponse"
      ( \x ->
          IamRegistrationResponse'
            Prelude.<$> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "registrationStatus")
      )

instance Prelude.Hashable IamRegistrationResponse where
  hashWithSalt _salt IamRegistrationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` registrationStatus

instance Prelude.NFData IamRegistrationResponse where
  rnf IamRegistrationResponse' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf registrationStatus
