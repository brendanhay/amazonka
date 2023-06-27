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
-- Module      : Amazonka.IAM.GetMFADevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an MFA device for a specified user.
module Amazonka.IAM.GetMFADevice
  ( -- * Creating a Request
    GetMFADevice (..),
    newGetMFADevice,

    -- * Request Lenses
    getMFADevice_userName,
    getMFADevice_serialNumber,

    -- * Destructuring the Response
    GetMFADeviceResponse (..),
    newGetMFADeviceResponse,

    -- * Response Lenses
    getMFADeviceResponse_certifications,
    getMFADeviceResponse_enableDate,
    getMFADeviceResponse_userName,
    getMFADeviceResponse_httpStatus,
    getMFADeviceResponse_serialNumber,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMFADevice' smart constructor.
data GetMFADevice = GetMFADevice'
  { -- | The friendly name identifying the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | Serial number that uniquely identifies the MFA device. For this API, we
    -- only accept FIDO security key
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html ARNs>.
    serialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'getMFADevice_userName' - The friendly name identifying the user.
--
-- 'serialNumber', 'getMFADevice_serialNumber' - Serial number that uniquely identifies the MFA device. For this API, we
-- only accept FIDO security key
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html ARNs>.
newGetMFADevice ::
  -- | 'serialNumber'
  Prelude.Text ->
  GetMFADevice
newGetMFADevice pSerialNumber_ =
  GetMFADevice'
    { userName = Prelude.Nothing,
      serialNumber = pSerialNumber_
    }

-- | The friendly name identifying the user.
getMFADevice_userName :: Lens.Lens' GetMFADevice (Prelude.Maybe Prelude.Text)
getMFADevice_userName = Lens.lens (\GetMFADevice' {userName} -> userName) (\s@GetMFADevice' {} a -> s {userName = a} :: GetMFADevice)

-- | Serial number that uniquely identifies the MFA device. For this API, we
-- only accept FIDO security key
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html ARNs>.
getMFADevice_serialNumber :: Lens.Lens' GetMFADevice Prelude.Text
getMFADevice_serialNumber = Lens.lens (\GetMFADevice' {serialNumber} -> serialNumber) (\s@GetMFADevice' {} a -> s {serialNumber = a} :: GetMFADevice)

instance Core.AWSRequest GetMFADevice where
  type AWSResponse GetMFADevice = GetMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetMFADeviceResult"
      ( \s h x ->
          GetMFADeviceResponse'
            Prelude.<$> ( x
                            Data..@? "Certifications"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (x Data..@? "EnableDate")
            Prelude.<*> (x Data..@? "UserName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "SerialNumber")
      )

instance Prelude.Hashable GetMFADevice where
  hashWithSalt _salt GetMFADevice' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` serialNumber

instance Prelude.NFData GetMFADevice where
  rnf GetMFADevice' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf serialNumber

instance Data.ToHeaders GetMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMFADevice where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMFADevice where
  toQuery GetMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetMFADevice" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "SerialNumber" Data.=: serialNumber
      ]

-- | /See:/ 'newGetMFADeviceResponse' smart constructor.
data GetMFADeviceResponse = GetMFADeviceResponse'
  { -- | The certifications of a specified user\'s MFA device. We currently
    -- provide FIPS-140-2, FIPS-140-3, and FIDO certification levels obtained
    -- from
    -- <https://fidoalliance.org/metadata/ FIDO Alliance Metadata Service (MDS)>.
    certifications :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date that a specified user\'s MFA device was first enabled.
    enableDate :: Prelude.Maybe Data.ISO8601,
    -- | The friendly name identifying the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Serial number that uniquely identifies the MFA device. For this API, we
    -- only accept FIDO security key
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html ARNs>.
    serialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certifications', 'getMFADeviceResponse_certifications' - The certifications of a specified user\'s MFA device. We currently
-- provide FIPS-140-2, FIPS-140-3, and FIDO certification levels obtained
-- from
-- <https://fidoalliance.org/metadata/ FIDO Alliance Metadata Service (MDS)>.
--
-- 'enableDate', 'getMFADeviceResponse_enableDate' - The date that a specified user\'s MFA device was first enabled.
--
-- 'userName', 'getMFADeviceResponse_userName' - The friendly name identifying the user.
--
-- 'httpStatus', 'getMFADeviceResponse_httpStatus' - The response's http status code.
--
-- 'serialNumber', 'getMFADeviceResponse_serialNumber' - Serial number that uniquely identifies the MFA device. For this API, we
-- only accept FIDO security key
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html ARNs>.
newGetMFADeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serialNumber'
  Prelude.Text ->
  GetMFADeviceResponse
newGetMFADeviceResponse pHttpStatus_ pSerialNumber_ =
  GetMFADeviceResponse'
    { certifications =
        Prelude.Nothing,
      enableDate = Prelude.Nothing,
      userName = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      serialNumber = pSerialNumber_
    }

-- | The certifications of a specified user\'s MFA device. We currently
-- provide FIPS-140-2, FIPS-140-3, and FIDO certification levels obtained
-- from
-- <https://fidoalliance.org/metadata/ FIDO Alliance Metadata Service (MDS)>.
getMFADeviceResponse_certifications :: Lens.Lens' GetMFADeviceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getMFADeviceResponse_certifications = Lens.lens (\GetMFADeviceResponse' {certifications} -> certifications) (\s@GetMFADeviceResponse' {} a -> s {certifications = a} :: GetMFADeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date that a specified user\'s MFA device was first enabled.
getMFADeviceResponse_enableDate :: Lens.Lens' GetMFADeviceResponse (Prelude.Maybe Prelude.UTCTime)
getMFADeviceResponse_enableDate = Lens.lens (\GetMFADeviceResponse' {enableDate} -> enableDate) (\s@GetMFADeviceResponse' {} a -> s {enableDate = a} :: GetMFADeviceResponse) Prelude.. Lens.mapping Data._Time

-- | The friendly name identifying the user.
getMFADeviceResponse_userName :: Lens.Lens' GetMFADeviceResponse (Prelude.Maybe Prelude.Text)
getMFADeviceResponse_userName = Lens.lens (\GetMFADeviceResponse' {userName} -> userName) (\s@GetMFADeviceResponse' {} a -> s {userName = a} :: GetMFADeviceResponse)

-- | The response's http status code.
getMFADeviceResponse_httpStatus :: Lens.Lens' GetMFADeviceResponse Prelude.Int
getMFADeviceResponse_httpStatus = Lens.lens (\GetMFADeviceResponse' {httpStatus} -> httpStatus) (\s@GetMFADeviceResponse' {} a -> s {httpStatus = a} :: GetMFADeviceResponse)

-- | Serial number that uniquely identifies the MFA device. For this API, we
-- only accept FIDO security key
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html ARNs>.
getMFADeviceResponse_serialNumber :: Lens.Lens' GetMFADeviceResponse Prelude.Text
getMFADeviceResponse_serialNumber = Lens.lens (\GetMFADeviceResponse' {serialNumber} -> serialNumber) (\s@GetMFADeviceResponse' {} a -> s {serialNumber = a} :: GetMFADeviceResponse)

instance Prelude.NFData GetMFADeviceResponse where
  rnf GetMFADeviceResponse' {..} =
    Prelude.rnf certifications
      `Prelude.seq` Prelude.rnf enableDate
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serialNumber
