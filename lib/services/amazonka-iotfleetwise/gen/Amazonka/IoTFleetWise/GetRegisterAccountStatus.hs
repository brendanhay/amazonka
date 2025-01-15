{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTFleetWise.GetRegisterAccountStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status of registering your Amazon Web
-- Services account, IAM, and Amazon Timestream resources so that Amazon
-- Web Services IoT FleetWise can transfer your vehicle data to the Amazon
-- Web Services Cloud.
--
-- For more information, including step-by-step procedures, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/setting-up.html Setting up Amazon Web Services IoT FleetWise>.
--
-- This API operation doesn\'t require input parameters.
module Amazonka.IoTFleetWise.GetRegisterAccountStatus
  ( -- * Creating a Request
    GetRegisterAccountStatus (..),
    newGetRegisterAccountStatus,

    -- * Destructuring the Response
    GetRegisterAccountStatusResponse (..),
    newGetRegisterAccountStatusResponse,

    -- * Response Lenses
    getRegisterAccountStatusResponse_httpStatus,
    getRegisterAccountStatusResponse_customerAccountId,
    getRegisterAccountStatusResponse_accountStatus,
    getRegisterAccountStatusResponse_timestreamRegistrationResponse,
    getRegisterAccountStatusResponse_iamRegistrationResponse,
    getRegisterAccountStatusResponse_creationTime,
    getRegisterAccountStatusResponse_lastModificationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRegisterAccountStatus' smart constructor.
data GetRegisterAccountStatus = GetRegisterAccountStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegisterAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetRegisterAccountStatus ::
  GetRegisterAccountStatus
newGetRegisterAccountStatus =
  GetRegisterAccountStatus'

instance Core.AWSRequest GetRegisterAccountStatus where
  type
    AWSResponse GetRegisterAccountStatus =
      GetRegisterAccountStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegisterAccountStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "customerAccountId")
            Prelude.<*> (x Data..:> "accountStatus")
            Prelude.<*> (x Data..:> "timestreamRegistrationResponse")
            Prelude.<*> (x Data..:> "iamRegistrationResponse")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "lastModificationTime")
      )

instance Prelude.Hashable GetRegisterAccountStatus where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetRegisterAccountStatus where
  rnf _ = ()

instance Data.ToHeaders GetRegisterAccountStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.GetRegisterAccountStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRegisterAccountStatus where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetRegisterAccountStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRegisterAccountStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegisterAccountStatusResponse' smart constructor.
data GetRegisterAccountStatusResponse = GetRegisterAccountStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ID of the Amazon Web Services account, provided at account
    -- creation.
    customerAccountId :: Prelude.Text,
    -- | The status of registering your account and resources. The status can be
    -- one of:
    --
    -- -   @REGISTRATION_SUCCESS@ - The Amazon Web Services resource is
    --     successfully registered.
    --
    -- -   @REGISTRATION_PENDING@ - Amazon Web Services IoT FleetWise is
    --     processing the registration request. This process takes
    --     approximately five minutes to complete.
    --
    -- -   @REGISTRATION_FAILURE@ - Amazon Web Services IoT FleetWise can\'t
    --     register the AWS resource. Try again later.
    accountStatus :: RegistrationStatus,
    -- | Information about the registered Amazon Timestream resources or errors,
    -- if any.
    timestreamRegistrationResponse :: TimestreamRegistrationResponse,
    -- | Information about the registered IAM resources or errors, if any.
    iamRegistrationResponse :: IamRegistrationResponse,
    -- | The time the account was registered, in seconds since epoch (January 1,
    -- 1970 at midnight UTC time).
    creationTime :: Data.POSIX,
    -- | The time this registration was last updated, in seconds since epoch
    -- (January 1, 1970 at midnight UTC time).
    lastModificationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegisterAccountStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRegisterAccountStatusResponse_httpStatus' - The response's http status code.
--
-- 'customerAccountId', 'getRegisterAccountStatusResponse_customerAccountId' - The unique ID of the Amazon Web Services account, provided at account
-- creation.
--
-- 'accountStatus', 'getRegisterAccountStatusResponse_accountStatus' - The status of registering your account and resources. The status can be
-- one of:
--
-- -   @REGISTRATION_SUCCESS@ - The Amazon Web Services resource is
--     successfully registered.
--
-- -   @REGISTRATION_PENDING@ - Amazon Web Services IoT FleetWise is
--     processing the registration request. This process takes
--     approximately five minutes to complete.
--
-- -   @REGISTRATION_FAILURE@ - Amazon Web Services IoT FleetWise can\'t
--     register the AWS resource. Try again later.
--
-- 'timestreamRegistrationResponse', 'getRegisterAccountStatusResponse_timestreamRegistrationResponse' - Information about the registered Amazon Timestream resources or errors,
-- if any.
--
-- 'iamRegistrationResponse', 'getRegisterAccountStatusResponse_iamRegistrationResponse' - Information about the registered IAM resources or errors, if any.
--
-- 'creationTime', 'getRegisterAccountStatusResponse_creationTime' - The time the account was registered, in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
--
-- 'lastModificationTime', 'getRegisterAccountStatusResponse_lastModificationTime' - The time this registration was last updated, in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
newGetRegisterAccountStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'customerAccountId'
  Prelude.Text ->
  -- | 'accountStatus'
  RegistrationStatus ->
  -- | 'timestreamRegistrationResponse'
  TimestreamRegistrationResponse ->
  -- | 'iamRegistrationResponse'
  IamRegistrationResponse ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModificationTime'
  Prelude.UTCTime ->
  GetRegisterAccountStatusResponse
newGetRegisterAccountStatusResponse
  pHttpStatus_
  pCustomerAccountId_
  pAccountStatus_
  pTimestreamRegistrationResponse_
  pIamRegistrationResponse_
  pCreationTime_
  pLastModificationTime_ =
    GetRegisterAccountStatusResponse'
      { httpStatus =
          pHttpStatus_,
        customerAccountId = pCustomerAccountId_,
        accountStatus = pAccountStatus_,
        timestreamRegistrationResponse =
          pTimestreamRegistrationResponse_,
        iamRegistrationResponse =
          pIamRegistrationResponse_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        lastModificationTime =
          Data._Time
            Lens.# pLastModificationTime_
      }

-- | The response's http status code.
getRegisterAccountStatusResponse_httpStatus :: Lens.Lens' GetRegisterAccountStatusResponse Prelude.Int
getRegisterAccountStatusResponse_httpStatus = Lens.lens (\GetRegisterAccountStatusResponse' {httpStatus} -> httpStatus) (\s@GetRegisterAccountStatusResponse' {} a -> s {httpStatus = a} :: GetRegisterAccountStatusResponse)

-- | The unique ID of the Amazon Web Services account, provided at account
-- creation.
getRegisterAccountStatusResponse_customerAccountId :: Lens.Lens' GetRegisterAccountStatusResponse Prelude.Text
getRegisterAccountStatusResponse_customerAccountId = Lens.lens (\GetRegisterAccountStatusResponse' {customerAccountId} -> customerAccountId) (\s@GetRegisterAccountStatusResponse' {} a -> s {customerAccountId = a} :: GetRegisterAccountStatusResponse)

-- | The status of registering your account and resources. The status can be
-- one of:
--
-- -   @REGISTRATION_SUCCESS@ - The Amazon Web Services resource is
--     successfully registered.
--
-- -   @REGISTRATION_PENDING@ - Amazon Web Services IoT FleetWise is
--     processing the registration request. This process takes
--     approximately five minutes to complete.
--
-- -   @REGISTRATION_FAILURE@ - Amazon Web Services IoT FleetWise can\'t
--     register the AWS resource. Try again later.
getRegisterAccountStatusResponse_accountStatus :: Lens.Lens' GetRegisterAccountStatusResponse RegistrationStatus
getRegisterAccountStatusResponse_accountStatus = Lens.lens (\GetRegisterAccountStatusResponse' {accountStatus} -> accountStatus) (\s@GetRegisterAccountStatusResponse' {} a -> s {accountStatus = a} :: GetRegisterAccountStatusResponse)

-- | Information about the registered Amazon Timestream resources or errors,
-- if any.
getRegisterAccountStatusResponse_timestreamRegistrationResponse :: Lens.Lens' GetRegisterAccountStatusResponse TimestreamRegistrationResponse
getRegisterAccountStatusResponse_timestreamRegistrationResponse = Lens.lens (\GetRegisterAccountStatusResponse' {timestreamRegistrationResponse} -> timestreamRegistrationResponse) (\s@GetRegisterAccountStatusResponse' {} a -> s {timestreamRegistrationResponse = a} :: GetRegisterAccountStatusResponse)

-- | Information about the registered IAM resources or errors, if any.
getRegisterAccountStatusResponse_iamRegistrationResponse :: Lens.Lens' GetRegisterAccountStatusResponse IamRegistrationResponse
getRegisterAccountStatusResponse_iamRegistrationResponse = Lens.lens (\GetRegisterAccountStatusResponse' {iamRegistrationResponse} -> iamRegistrationResponse) (\s@GetRegisterAccountStatusResponse' {} a -> s {iamRegistrationResponse = a} :: GetRegisterAccountStatusResponse)

-- | The time the account was registered, in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
getRegisterAccountStatusResponse_creationTime :: Lens.Lens' GetRegisterAccountStatusResponse Prelude.UTCTime
getRegisterAccountStatusResponse_creationTime = Lens.lens (\GetRegisterAccountStatusResponse' {creationTime} -> creationTime) (\s@GetRegisterAccountStatusResponse' {} a -> s {creationTime = a} :: GetRegisterAccountStatusResponse) Prelude.. Data._Time

-- | The time this registration was last updated, in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
getRegisterAccountStatusResponse_lastModificationTime :: Lens.Lens' GetRegisterAccountStatusResponse Prelude.UTCTime
getRegisterAccountStatusResponse_lastModificationTime = Lens.lens (\GetRegisterAccountStatusResponse' {lastModificationTime} -> lastModificationTime) (\s@GetRegisterAccountStatusResponse' {} a -> s {lastModificationTime = a} :: GetRegisterAccountStatusResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    GetRegisterAccountStatusResponse
  where
  rnf GetRegisterAccountStatusResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf customerAccountId `Prelude.seq`
        Prelude.rnf accountStatus `Prelude.seq`
          Prelude.rnf timestreamRegistrationResponse `Prelude.seq`
            Prelude.rnf iamRegistrationResponse `Prelude.seq`
              Prelude.rnf creationTime `Prelude.seq`
                Prelude.rnf lastModificationTime
