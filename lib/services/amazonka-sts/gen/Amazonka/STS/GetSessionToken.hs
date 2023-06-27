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
-- Module      : Amazonka.STS.GetSessionToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary credentials for an Amazon Web Services
-- account or IAM user. The credentials consist of an access key ID, a
-- secret access key, and a security token. Typically, you use
-- @GetSessionToken@ if you want to use MFA to protect programmatic calls
-- to specific Amazon Web Services API operations like Amazon EC2
-- @StopInstances@.
--
-- MFA-enabled IAM users must call @GetSessionToken@ and submit an MFA code
-- that is associated with their MFA device. Using the temporary security
-- credentials that the call returns, IAM users can then make programmatic
-- calls to API operations that require MFA authentication. An incorrect
-- MFA code causes the API to return an access denied error. For a
-- comparison of @GetSessionToken@ with the other API operations that
-- produce temporary credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the Amazon Web Services STS API operations>
-- in the /IAM User Guide/.
--
-- No permissions are required for users to perform this operation. The
-- purpose of the @sts:GetSessionToken@ operation is to authenticate the
-- user using MFA. You cannot use policies to control authentication
-- operations. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_getsessiontoken.html Permissions for GetSessionToken>
-- in the /IAM User Guide/.
--
-- __Session Duration__
--
-- The @GetSessionToken@ operation must be called by using the long-term
-- Amazon Web Services security credentials of an IAM user. Credentials
-- that are created by IAM users are valid for the duration that you
-- specify. This duration can range from 900 seconds (15 minutes) up to a
-- maximum of 129,600 seconds (36 hours), with a default of 43,200 seconds
-- (12 hours). Credentials based on account credentials can range from 900
-- seconds (15 minutes) up to 3,600 seconds (1 hour), with a default of 1
-- hour.
--
-- __Permissions__
--
-- The temporary security credentials created by @GetSessionToken@ can be
-- used to make API calls to any Amazon Web Services service with the
-- following exceptions:
--
-- -   You cannot call any IAM API operations unless MFA authentication
--     information is included in the request.
--
-- -   You cannot call any STS API /except/ @AssumeRole@ or
--     @GetCallerIdentity@.
--
-- The credentials that @GetSessionToken@ returns are based on permissions
-- associated with the IAM user whose credentials were used to call the
-- operation. The temporary credentials have the same permissions as the
-- IAM user.
--
-- Although it is possible to call @GetSessionToken@ using the security
-- credentials of an Amazon Web Services account root user rather than an
-- IAM user, we do not recommend it. If @GetSessionToken@ is called using
-- root user credentials, the temporary credentials have root user
-- permissions. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/best-practices.html#lock-away-credentials Safeguard your root user credentials and don\'t use them for everyday tasks>
-- in the /IAM User Guide/
--
-- For more information about using @GetSessionToken@ to create temporary
-- credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_getsessiontoken Temporary Credentials for Users in Untrusted Environments>
-- in the /IAM User Guide/.
module Amazonka.STS.GetSessionToken
  ( -- * Creating a Request
    GetSessionToken (..),
    newGetSessionToken,

    -- * Request Lenses
    getSessionToken_durationSeconds,
    getSessionToken_serialNumber,
    getSessionToken_tokenCode,

    -- * Destructuring the Response
    GetSessionTokenResponse (..),
    newGetSessionTokenResponse,

    -- * Response Lenses
    getSessionTokenResponse_credentials,
    getSessionTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.STS.Types

-- | /See:/ 'newGetSessionToken' smart constructor.
data GetSessionToken = GetSessionToken'
  { -- | The duration, in seconds, that the credentials should remain valid.
    -- Acceptable durations for IAM user sessions range from 900 seconds (15
    -- minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours)
    -- as the default. Sessions for Amazon Web Services account owners are
    -- restricted to a maximum of 3,600 seconds (one hour). If the duration is
    -- longer than one hour, the session for Amazon Web Services account owners
    -- defaults to one hour.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The identification number of the MFA device that is associated with the
    -- IAM user who is making the @GetSessionToken@ call. Specify this value if
    -- the IAM user has a policy that requires MFA authentication. The value is
    -- either the serial number for a hardware device (such as @GAHT12345678@)
    -- or an Amazon Resource Name (ARN) for a virtual device (such as
    -- @arn:aws:iam::123456789012:mfa\/user@). You can find the device for an
    -- IAM user by going to the Amazon Web Services Management Console and
    -- viewing the user\'s security credentials.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The value provided by the MFA device, if MFA is required. If any policy
    -- requires the IAM user to submit an MFA code, specify this value. If MFA
    -- authentication is required, the user must provide a code when requesting
    -- a set of temporary security credentials. A user who fails to provide the
    -- code receives an \"access denied\" response when requesting resources
    -- that require MFA authentication.
    --
    -- The format for this parameter, as described by its regex pattern, is a
    -- sequence of six numeric digits.
    tokenCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationSeconds', 'getSessionToken_durationSeconds' - The duration, in seconds, that the credentials should remain valid.
-- Acceptable durations for IAM user sessions range from 900 seconds (15
-- minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours)
-- as the default. Sessions for Amazon Web Services account owners are
-- restricted to a maximum of 3,600 seconds (one hour). If the duration is
-- longer than one hour, the session for Amazon Web Services account owners
-- defaults to one hour.
--
-- 'serialNumber', 'getSessionToken_serialNumber' - The identification number of the MFA device that is associated with the
-- IAM user who is making the @GetSessionToken@ call. Specify this value if
-- the IAM user has a policy that requires MFA authentication. The value is
-- either the serial number for a hardware device (such as @GAHT12345678@)
-- or an Amazon Resource Name (ARN) for a virtual device (such as
-- @arn:aws:iam::123456789012:mfa\/user@). You can find the device for an
-- IAM user by going to the Amazon Web Services Management Console and
-- viewing the user\'s security credentials.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
--
-- 'tokenCode', 'getSessionToken_tokenCode' - The value provided by the MFA device, if MFA is required. If any policy
-- requires the IAM user to submit an MFA code, specify this value. If MFA
-- authentication is required, the user must provide a code when requesting
-- a set of temporary security credentials. A user who fails to provide the
-- code receives an \"access denied\" response when requesting resources
-- that require MFA authentication.
--
-- The format for this parameter, as described by its regex pattern, is a
-- sequence of six numeric digits.
newGetSessionToken ::
  GetSessionToken
newGetSessionToken =
  GetSessionToken'
    { durationSeconds = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      tokenCode = Prelude.Nothing
    }

-- | The duration, in seconds, that the credentials should remain valid.
-- Acceptable durations for IAM user sessions range from 900 seconds (15
-- minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours)
-- as the default. Sessions for Amazon Web Services account owners are
-- restricted to a maximum of 3,600 seconds (one hour). If the duration is
-- longer than one hour, the session for Amazon Web Services account owners
-- defaults to one hour.
getSessionToken_durationSeconds :: Lens.Lens' GetSessionToken (Prelude.Maybe Prelude.Natural)
getSessionToken_durationSeconds = Lens.lens (\GetSessionToken' {durationSeconds} -> durationSeconds) (\s@GetSessionToken' {} a -> s {durationSeconds = a} :: GetSessionToken)

-- | The identification number of the MFA device that is associated with the
-- IAM user who is making the @GetSessionToken@ call. Specify this value if
-- the IAM user has a policy that requires MFA authentication. The value is
-- either the serial number for a hardware device (such as @GAHT12345678@)
-- or an Amazon Resource Name (ARN) for a virtual device (such as
-- @arn:aws:iam::123456789012:mfa\/user@). You can find the device for an
-- IAM user by going to the Amazon Web Services Management Console and
-- viewing the user\'s security credentials.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
getSessionToken_serialNumber :: Lens.Lens' GetSessionToken (Prelude.Maybe Prelude.Text)
getSessionToken_serialNumber = Lens.lens (\GetSessionToken' {serialNumber} -> serialNumber) (\s@GetSessionToken' {} a -> s {serialNumber = a} :: GetSessionToken)

-- | The value provided by the MFA device, if MFA is required. If any policy
-- requires the IAM user to submit an MFA code, specify this value. If MFA
-- authentication is required, the user must provide a code when requesting
-- a set of temporary security credentials. A user who fails to provide the
-- code receives an \"access denied\" response when requesting resources
-- that require MFA authentication.
--
-- The format for this parameter, as described by its regex pattern, is a
-- sequence of six numeric digits.
getSessionToken_tokenCode :: Lens.Lens' GetSessionToken (Prelude.Maybe Prelude.Text)
getSessionToken_tokenCode = Lens.lens (\GetSessionToken' {tokenCode} -> tokenCode) (\s@GetSessionToken' {} a -> s {tokenCode = a} :: GetSessionToken)

instance Core.AWSRequest GetSessionToken where
  type
    AWSResponse GetSessionToken =
      GetSessionTokenResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetSessionTokenResult"
      ( \s h x ->
          GetSessionTokenResponse'
            Prelude.<$> (x Data..@? "Credentials")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSessionToken where
  hashWithSalt _salt GetSessionToken' {..} =
    _salt
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` tokenCode

instance Prelude.NFData GetSessionToken where
  rnf GetSessionToken' {..} =
    Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf tokenCode

instance Data.ToHeaders GetSessionToken where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSessionToken where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSessionToken where
  toQuery GetSessionToken' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetSessionToken" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-06-15" :: Prelude.ByteString),
        "DurationSeconds" Data.=: durationSeconds,
        "SerialNumber" Data.=: serialNumber,
        "TokenCode" Data.=: tokenCode
      ]

-- | Contains the response to a successful GetSessionToken request, including
-- temporary Amazon Web Services credentials that can be used to make
-- Amazon Web Services requests.
--
-- /See:/ 'newGetSessionTokenResponse' smart constructor.
data GetSessionTokenResponse = GetSessionTokenResponse'
  { -- | The temporary security credentials, which include an access key ID, a
    -- secret access key, and a security (or session) token.
    --
    -- The size of the security token that STS API operations return is not
    -- fixed. We strongly recommend that you make no assumptions about the
    -- maximum size.
    credentials :: Prelude.Maybe Core.AuthEnv,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'getSessionTokenResponse_credentials' - The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
--
-- 'httpStatus', 'getSessionTokenResponse_httpStatus' - The response's http status code.
newGetSessionTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSessionTokenResponse
newGetSessionTokenResponse pHttpStatus_ =
  GetSessionTokenResponse'
    { credentials =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
getSessionTokenResponse_credentials :: Lens.Lens' GetSessionTokenResponse (Prelude.Maybe Core.AuthEnv)
getSessionTokenResponse_credentials = Lens.lens (\GetSessionTokenResponse' {credentials} -> credentials) (\s@GetSessionTokenResponse' {} a -> s {credentials = a} :: GetSessionTokenResponse)

-- | The response's http status code.
getSessionTokenResponse_httpStatus :: Lens.Lens' GetSessionTokenResponse Prelude.Int
getSessionTokenResponse_httpStatus = Lens.lens (\GetSessionTokenResponse' {httpStatus} -> httpStatus) (\s@GetSessionTokenResponse' {} a -> s {httpStatus = a} :: GetSessionTokenResponse)

instance Prelude.NFData GetSessionTokenResponse where
  rnf GetSessionTokenResponse' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf httpStatus
