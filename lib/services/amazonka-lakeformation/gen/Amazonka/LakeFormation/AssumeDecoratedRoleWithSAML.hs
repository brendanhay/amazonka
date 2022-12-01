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
-- Module      : Amazonka.LakeFormation.AssumeDecoratedRoleWithSAML
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a caller to assume an IAM role decorated as the SAML user
-- specified in the SAML assertion included in the request. This decoration
-- allows Lake Formation to enforce access policies against the SAML users
-- and groups. This API operation requires SAML federation setup in the
-- caller’s account as it can only be called with valid SAML assertions.
-- Lake Formation does not scope down the permission of the assumed role.
-- All permissions attached to the role via the SAML federation setup will
-- be included in the role session.
--
-- This decorated role is expected to access data in Amazon S3 by getting
-- temporary access from Lake Formation which is authorized via the virtual
-- API @GetDataAccess@. Therefore, all SAML roles that can be assumed via
-- @AssumeDecoratedRoleWithSAML@ must at a minimum include
-- @lakeformation:GetDataAccess@ in their role policies. A typical IAM
-- policy attached to such a role would look as follows:
module Amazonka.LakeFormation.AssumeDecoratedRoleWithSAML
  ( -- * Creating a Request
    AssumeDecoratedRoleWithSAML (..),
    newAssumeDecoratedRoleWithSAML,

    -- * Request Lenses
    assumeDecoratedRoleWithSAML_durationSeconds,
    assumeDecoratedRoleWithSAML_sAMLAssertion,
    assumeDecoratedRoleWithSAML_roleArn,
    assumeDecoratedRoleWithSAML_principalArn,

    -- * Destructuring the Response
    AssumeDecoratedRoleWithSAMLResponse (..),
    newAssumeDecoratedRoleWithSAMLResponse,

    -- * Response Lenses
    assumeDecoratedRoleWithSAMLResponse_sessionToken,
    assumeDecoratedRoleWithSAMLResponse_expiration,
    assumeDecoratedRoleWithSAMLResponse_secretAccessKey,
    assumeDecoratedRoleWithSAMLResponse_accessKeyId,
    assumeDecoratedRoleWithSAMLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssumeDecoratedRoleWithSAML' smart constructor.
data AssumeDecoratedRoleWithSAML = AssumeDecoratedRoleWithSAML'
  { -- | The time period, between 900 and 43,200 seconds, for the timeout of the
    -- temporary credentials.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A SAML assertion consisting of an assertion statement for the user who
    -- needs temporary credentials. This must match the SAML assertion that was
    -- issued to IAM. This must be Base64 encoded.
    sAMLAssertion :: Prelude.Text,
    -- | The role that represents an IAM principal whose scope down policy allows
    -- it to call credential vending APIs such as
    -- @GetTemporaryTableCredentials@. The caller must also have iam:PassRole
    -- permission on this role.
    roleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SAML provider in IAM that
    -- describes the IdP.
    principalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeDecoratedRoleWithSAML' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationSeconds', 'assumeDecoratedRoleWithSAML_durationSeconds' - The time period, between 900 and 43,200 seconds, for the timeout of the
-- temporary credentials.
--
-- 'sAMLAssertion', 'assumeDecoratedRoleWithSAML_sAMLAssertion' - A SAML assertion consisting of an assertion statement for the user who
-- needs temporary credentials. This must match the SAML assertion that was
-- issued to IAM. This must be Base64 encoded.
--
-- 'roleArn', 'assumeDecoratedRoleWithSAML_roleArn' - The role that represents an IAM principal whose scope down policy allows
-- it to call credential vending APIs such as
-- @GetTemporaryTableCredentials@. The caller must also have iam:PassRole
-- permission on this role.
--
-- 'principalArn', 'assumeDecoratedRoleWithSAML_principalArn' - The Amazon Resource Name (ARN) of the SAML provider in IAM that
-- describes the IdP.
newAssumeDecoratedRoleWithSAML ::
  -- | 'sAMLAssertion'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'principalArn'
  Prelude.Text ->
  AssumeDecoratedRoleWithSAML
newAssumeDecoratedRoleWithSAML
  pSAMLAssertion_
  pRoleArn_
  pPrincipalArn_ =
    AssumeDecoratedRoleWithSAML'
      { durationSeconds =
          Prelude.Nothing,
        sAMLAssertion = pSAMLAssertion_,
        roleArn = pRoleArn_,
        principalArn = pPrincipalArn_
      }

-- | The time period, between 900 and 43,200 seconds, for the timeout of the
-- temporary credentials.
assumeDecoratedRoleWithSAML_durationSeconds :: Lens.Lens' AssumeDecoratedRoleWithSAML (Prelude.Maybe Prelude.Natural)
assumeDecoratedRoleWithSAML_durationSeconds = Lens.lens (\AssumeDecoratedRoleWithSAML' {durationSeconds} -> durationSeconds) (\s@AssumeDecoratedRoleWithSAML' {} a -> s {durationSeconds = a} :: AssumeDecoratedRoleWithSAML)

-- | A SAML assertion consisting of an assertion statement for the user who
-- needs temporary credentials. This must match the SAML assertion that was
-- issued to IAM. This must be Base64 encoded.
assumeDecoratedRoleWithSAML_sAMLAssertion :: Lens.Lens' AssumeDecoratedRoleWithSAML Prelude.Text
assumeDecoratedRoleWithSAML_sAMLAssertion = Lens.lens (\AssumeDecoratedRoleWithSAML' {sAMLAssertion} -> sAMLAssertion) (\s@AssumeDecoratedRoleWithSAML' {} a -> s {sAMLAssertion = a} :: AssumeDecoratedRoleWithSAML)

-- | The role that represents an IAM principal whose scope down policy allows
-- it to call credential vending APIs such as
-- @GetTemporaryTableCredentials@. The caller must also have iam:PassRole
-- permission on this role.
assumeDecoratedRoleWithSAML_roleArn :: Lens.Lens' AssumeDecoratedRoleWithSAML Prelude.Text
assumeDecoratedRoleWithSAML_roleArn = Lens.lens (\AssumeDecoratedRoleWithSAML' {roleArn} -> roleArn) (\s@AssumeDecoratedRoleWithSAML' {} a -> s {roleArn = a} :: AssumeDecoratedRoleWithSAML)

-- | The Amazon Resource Name (ARN) of the SAML provider in IAM that
-- describes the IdP.
assumeDecoratedRoleWithSAML_principalArn :: Lens.Lens' AssumeDecoratedRoleWithSAML Prelude.Text
assumeDecoratedRoleWithSAML_principalArn = Lens.lens (\AssumeDecoratedRoleWithSAML' {principalArn} -> principalArn) (\s@AssumeDecoratedRoleWithSAML' {} a -> s {principalArn = a} :: AssumeDecoratedRoleWithSAML)

instance Core.AWSRequest AssumeDecoratedRoleWithSAML where
  type
    AWSResponse AssumeDecoratedRoleWithSAML =
      AssumeDecoratedRoleWithSAMLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssumeDecoratedRoleWithSAMLResponse'
            Prelude.<$> (x Core..?> "SessionToken")
            Prelude.<*> (x Core..?> "Expiration")
            Prelude.<*> (x Core..?> "SecretAccessKey")
            Prelude.<*> (x Core..?> "AccessKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssumeDecoratedRoleWithSAML where
  hashWithSalt _salt AssumeDecoratedRoleWithSAML' {..} =
    _salt `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` sAMLAssertion
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` principalArn

instance Prelude.NFData AssumeDecoratedRoleWithSAML where
  rnf AssumeDecoratedRoleWithSAML' {..} =
    Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf sAMLAssertion
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf principalArn

instance Core.ToHeaders AssumeDecoratedRoleWithSAML where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssumeDecoratedRoleWithSAML where
  toJSON AssumeDecoratedRoleWithSAML' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DurationSeconds" Core..=)
              Prelude.<$> durationSeconds,
            Prelude.Just ("SAMLAssertion" Core..= sAMLAssertion),
            Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just ("PrincipalArn" Core..= principalArn)
          ]
      )

instance Core.ToPath AssumeDecoratedRoleWithSAML where
  toPath = Prelude.const "/AssumeDecoratedRoleWithSAML"

instance Core.ToQuery AssumeDecoratedRoleWithSAML where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssumeDecoratedRoleWithSAMLResponse' smart constructor.
data AssumeDecoratedRoleWithSAMLResponse = AssumeDecoratedRoleWithSAMLResponse'
  { -- | The session token for the temporary credentials.
    sessionToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the temporary credentials expire.
    expiration :: Prelude.Maybe Core.POSIX,
    -- | The secret key for the temporary credentials. (The access key consists
    -- of an access key ID and a secret key).
    secretAccessKey :: Prelude.Maybe Prelude.Text,
    -- | The access key ID for the temporary credentials. (The access key
    -- consists of an access key ID and a secret key).
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeDecoratedRoleWithSAMLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionToken', 'assumeDecoratedRoleWithSAMLResponse_sessionToken' - The session token for the temporary credentials.
--
-- 'expiration', 'assumeDecoratedRoleWithSAMLResponse_expiration' - The date and time when the temporary credentials expire.
--
-- 'secretAccessKey', 'assumeDecoratedRoleWithSAMLResponse_secretAccessKey' - The secret key for the temporary credentials. (The access key consists
-- of an access key ID and a secret key).
--
-- 'accessKeyId', 'assumeDecoratedRoleWithSAMLResponse_accessKeyId' - The access key ID for the temporary credentials. (The access key
-- consists of an access key ID and a secret key).
--
-- 'httpStatus', 'assumeDecoratedRoleWithSAMLResponse_httpStatus' - The response's http status code.
newAssumeDecoratedRoleWithSAMLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssumeDecoratedRoleWithSAMLResponse
newAssumeDecoratedRoleWithSAMLResponse pHttpStatus_ =
  AssumeDecoratedRoleWithSAMLResponse'
    { sessionToken =
        Prelude.Nothing,
      expiration = Prelude.Nothing,
      secretAccessKey = Prelude.Nothing,
      accessKeyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session token for the temporary credentials.
assumeDecoratedRoleWithSAMLResponse_sessionToken :: Lens.Lens' AssumeDecoratedRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeDecoratedRoleWithSAMLResponse_sessionToken = Lens.lens (\AssumeDecoratedRoleWithSAMLResponse' {sessionToken} -> sessionToken) (\s@AssumeDecoratedRoleWithSAMLResponse' {} a -> s {sessionToken = a} :: AssumeDecoratedRoleWithSAMLResponse)

-- | The date and time when the temporary credentials expire.
assumeDecoratedRoleWithSAMLResponse_expiration :: Lens.Lens' AssumeDecoratedRoleWithSAMLResponse (Prelude.Maybe Prelude.UTCTime)
assumeDecoratedRoleWithSAMLResponse_expiration = Lens.lens (\AssumeDecoratedRoleWithSAMLResponse' {expiration} -> expiration) (\s@AssumeDecoratedRoleWithSAMLResponse' {} a -> s {expiration = a} :: AssumeDecoratedRoleWithSAMLResponse) Prelude.. Lens.mapping Core._Time

-- | The secret key for the temporary credentials. (The access key consists
-- of an access key ID and a secret key).
assumeDecoratedRoleWithSAMLResponse_secretAccessKey :: Lens.Lens' AssumeDecoratedRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeDecoratedRoleWithSAMLResponse_secretAccessKey = Lens.lens (\AssumeDecoratedRoleWithSAMLResponse' {secretAccessKey} -> secretAccessKey) (\s@AssumeDecoratedRoleWithSAMLResponse' {} a -> s {secretAccessKey = a} :: AssumeDecoratedRoleWithSAMLResponse)

-- | The access key ID for the temporary credentials. (The access key
-- consists of an access key ID and a secret key).
assumeDecoratedRoleWithSAMLResponse_accessKeyId :: Lens.Lens' AssumeDecoratedRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeDecoratedRoleWithSAMLResponse_accessKeyId = Lens.lens (\AssumeDecoratedRoleWithSAMLResponse' {accessKeyId} -> accessKeyId) (\s@AssumeDecoratedRoleWithSAMLResponse' {} a -> s {accessKeyId = a} :: AssumeDecoratedRoleWithSAMLResponse)

-- | The response's http status code.
assumeDecoratedRoleWithSAMLResponse_httpStatus :: Lens.Lens' AssumeDecoratedRoleWithSAMLResponse Prelude.Int
assumeDecoratedRoleWithSAMLResponse_httpStatus = Lens.lens (\AssumeDecoratedRoleWithSAMLResponse' {httpStatus} -> httpStatus) (\s@AssumeDecoratedRoleWithSAMLResponse' {} a -> s {httpStatus = a} :: AssumeDecoratedRoleWithSAMLResponse)

instance
  Prelude.NFData
    AssumeDecoratedRoleWithSAMLResponse
  where
  rnf AssumeDecoratedRoleWithSAMLResponse' {..} =
    Prelude.rnf sessionToken
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf httpStatus
