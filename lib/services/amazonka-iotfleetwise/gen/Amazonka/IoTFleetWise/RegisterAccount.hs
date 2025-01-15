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
-- Module      : Amazonka.IoTFleetWise.RegisterAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers your Amazon Web Services account, IAM, and Amazon Timestream
-- resources so Amazon Web Services IoT FleetWise can transfer your vehicle
-- data to the Amazon Web Services Cloud. For more information, including
-- step-by-step procedures, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/setting-up.html Setting up Amazon Web Services IoT FleetWise>.
--
-- An Amazon Web Services account is __not__ the same thing as a \"user
-- account\". An
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/introduction_identity-management.html#intro-identity-users Amazon Web Services user>
-- is an identity that you create using Identity and Access Management
-- (IAM) and takes the form of either an
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users.html IAM user>
-- or an
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM role, both with credentials>.
-- A single Amazon Web Services account can, and typically does, contain
-- many users and roles.
module Amazonka.IoTFleetWise.RegisterAccount
  ( -- * Creating a Request
    RegisterAccount (..),
    newRegisterAccount,

    -- * Request Lenses
    registerAccount_iamResources,
    registerAccount_timestreamResources,

    -- * Destructuring the Response
    RegisterAccountResponse (..),
    newRegisterAccountResponse,

    -- * Response Lenses
    registerAccountResponse_httpStatus,
    registerAccountResponse_registerAccountStatus,
    registerAccountResponse_timestreamResources,
    registerAccountResponse_iamResources,
    registerAccountResponse_creationTime,
    registerAccountResponse_lastModificationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterAccount' smart constructor.
data RegisterAccount = RegisterAccount'
  { -- | The IAM resource that allows Amazon Web Services IoT FleetWise to send
    -- data to Amazon Timestream.
    iamResources :: Prelude.Maybe IamResources,
    timestreamResources :: TimestreamResources
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamResources', 'registerAccount_iamResources' - The IAM resource that allows Amazon Web Services IoT FleetWise to send
-- data to Amazon Timestream.
--
-- 'timestreamResources', 'registerAccount_timestreamResources' - Undocumented member.
newRegisterAccount ::
  -- | 'timestreamResources'
  TimestreamResources ->
  RegisterAccount
newRegisterAccount pTimestreamResources_ =
  RegisterAccount'
    { iamResources = Prelude.Nothing,
      timestreamResources = pTimestreamResources_
    }

-- | The IAM resource that allows Amazon Web Services IoT FleetWise to send
-- data to Amazon Timestream.
registerAccount_iamResources :: Lens.Lens' RegisterAccount (Prelude.Maybe IamResources)
registerAccount_iamResources = Lens.lens (\RegisterAccount' {iamResources} -> iamResources) (\s@RegisterAccount' {} a -> s {iamResources = a} :: RegisterAccount)

-- | Undocumented member.
registerAccount_timestreamResources :: Lens.Lens' RegisterAccount TimestreamResources
registerAccount_timestreamResources = Lens.lens (\RegisterAccount' {timestreamResources} -> timestreamResources) (\s@RegisterAccount' {} a -> s {timestreamResources = a} :: RegisterAccount)

instance Core.AWSRequest RegisterAccount where
  type
    AWSResponse RegisterAccount =
      RegisterAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "registerAccountStatus")
            Prelude.<*> (x Data..:> "timestreamResources")
            Prelude.<*> (x Data..:> "iamResources")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "lastModificationTime")
      )

instance Prelude.Hashable RegisterAccount where
  hashWithSalt _salt RegisterAccount' {..} =
    _salt
      `Prelude.hashWithSalt` iamResources
      `Prelude.hashWithSalt` timestreamResources

instance Prelude.NFData RegisterAccount where
  rnf RegisterAccount' {..} =
    Prelude.rnf iamResources `Prelude.seq`
      Prelude.rnf timestreamResources

instance Data.ToHeaders RegisterAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.RegisterAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterAccount where
  toJSON RegisterAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iamResources" Data..=) Prelude.<$> iamResources,
            Prelude.Just
              ("timestreamResources" Data..= timestreamResources)
          ]
      )

instance Data.ToPath RegisterAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterAccountResponse' smart constructor.
data RegisterAccountResponse = RegisterAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of registering your Amazon Web Services account, IAM role,
    -- and Timestream resources.
    registerAccountStatus :: RegistrationStatus,
    timestreamResources :: TimestreamResources,
    -- | The registered IAM resource that allows Amazon Web Services IoT
    -- FleetWise to send data to Amazon Timestream.
    iamResources :: IamResources,
    -- | The time the account was registered, in seconds since epoch (January 1,
    -- 1970 at midnight UTC time).
    creationTime :: Data.POSIX,
    -- | The time this registration was last updated, in seconds since epoch
    -- (January 1, 1970 at midnight UTC time).
    lastModificationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerAccountResponse_httpStatus' - The response's http status code.
--
-- 'registerAccountStatus', 'registerAccountResponse_registerAccountStatus' - The status of registering your Amazon Web Services account, IAM role,
-- and Timestream resources.
--
-- 'timestreamResources', 'registerAccountResponse_timestreamResources' - Undocumented member.
--
-- 'iamResources', 'registerAccountResponse_iamResources' - The registered IAM resource that allows Amazon Web Services IoT
-- FleetWise to send data to Amazon Timestream.
--
-- 'creationTime', 'registerAccountResponse_creationTime' - The time the account was registered, in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
--
-- 'lastModificationTime', 'registerAccountResponse_lastModificationTime' - The time this registration was last updated, in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
newRegisterAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'registerAccountStatus'
  RegistrationStatus ->
  -- | 'timestreamResources'
  TimestreamResources ->
  -- | 'iamResources'
  IamResources ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModificationTime'
  Prelude.UTCTime ->
  RegisterAccountResponse
newRegisterAccountResponse
  pHttpStatus_
  pRegisterAccountStatus_
  pTimestreamResources_
  pIamResources_
  pCreationTime_
  pLastModificationTime_ =
    RegisterAccountResponse'
      { httpStatus = pHttpStatus_,
        registerAccountStatus = pRegisterAccountStatus_,
        timestreamResources = pTimestreamResources_,
        iamResources = pIamResources_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModificationTime =
          Data._Time Lens.# pLastModificationTime_
      }

-- | The response's http status code.
registerAccountResponse_httpStatus :: Lens.Lens' RegisterAccountResponse Prelude.Int
registerAccountResponse_httpStatus = Lens.lens (\RegisterAccountResponse' {httpStatus} -> httpStatus) (\s@RegisterAccountResponse' {} a -> s {httpStatus = a} :: RegisterAccountResponse)

-- | The status of registering your Amazon Web Services account, IAM role,
-- and Timestream resources.
registerAccountResponse_registerAccountStatus :: Lens.Lens' RegisterAccountResponse RegistrationStatus
registerAccountResponse_registerAccountStatus = Lens.lens (\RegisterAccountResponse' {registerAccountStatus} -> registerAccountStatus) (\s@RegisterAccountResponse' {} a -> s {registerAccountStatus = a} :: RegisterAccountResponse)

-- | Undocumented member.
registerAccountResponse_timestreamResources :: Lens.Lens' RegisterAccountResponse TimestreamResources
registerAccountResponse_timestreamResources = Lens.lens (\RegisterAccountResponse' {timestreamResources} -> timestreamResources) (\s@RegisterAccountResponse' {} a -> s {timestreamResources = a} :: RegisterAccountResponse)

-- | The registered IAM resource that allows Amazon Web Services IoT
-- FleetWise to send data to Amazon Timestream.
registerAccountResponse_iamResources :: Lens.Lens' RegisterAccountResponse IamResources
registerAccountResponse_iamResources = Lens.lens (\RegisterAccountResponse' {iamResources} -> iamResources) (\s@RegisterAccountResponse' {} a -> s {iamResources = a} :: RegisterAccountResponse)

-- | The time the account was registered, in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
registerAccountResponse_creationTime :: Lens.Lens' RegisterAccountResponse Prelude.UTCTime
registerAccountResponse_creationTime = Lens.lens (\RegisterAccountResponse' {creationTime} -> creationTime) (\s@RegisterAccountResponse' {} a -> s {creationTime = a} :: RegisterAccountResponse) Prelude.. Data._Time

-- | The time this registration was last updated, in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
registerAccountResponse_lastModificationTime :: Lens.Lens' RegisterAccountResponse Prelude.UTCTime
registerAccountResponse_lastModificationTime = Lens.lens (\RegisterAccountResponse' {lastModificationTime} -> lastModificationTime) (\s@RegisterAccountResponse' {} a -> s {lastModificationTime = a} :: RegisterAccountResponse) Prelude.. Data._Time

instance Prelude.NFData RegisterAccountResponse where
  rnf RegisterAccountResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf registerAccountStatus `Prelude.seq`
        Prelude.rnf timestreamResources `Prelude.seq`
          Prelude.rnf iamResources `Prelude.seq`
            Prelude.rnf creationTime `Prelude.seq`
              Prelude.rnf lastModificationTime
