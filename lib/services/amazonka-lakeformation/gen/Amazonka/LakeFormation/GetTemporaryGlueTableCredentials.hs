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
-- Module      : Amazonka.LakeFormation.GetTemporaryGlueTableCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a caller in a secure environment to assume a role with permission
-- to access Amazon S3. In order to vend such credentials, Lake Formation
-- assumes the role associated with a registered location, for example an
-- Amazon S3 bucket, with a scope down policy which restricts the access to
-- a single prefix.
module Amazonka.LakeFormation.GetTemporaryGlueTableCredentials
  ( -- * Creating a Request
    GetTemporaryGlueTableCredentials (..),
    newGetTemporaryGlueTableCredentials,

    -- * Request Lenses
    getTemporaryGlueTableCredentials_auditContext,
    getTemporaryGlueTableCredentials_permissions,
    getTemporaryGlueTableCredentials_durationSeconds,
    getTemporaryGlueTableCredentials_tableArn,
    getTemporaryGlueTableCredentials_supportedPermissionTypes,

    -- * Destructuring the Response
    GetTemporaryGlueTableCredentialsResponse (..),
    newGetTemporaryGlueTableCredentialsResponse,

    -- * Response Lenses
    getTemporaryGlueTableCredentialsResponse_sessionToken,
    getTemporaryGlueTableCredentialsResponse_expiration,
    getTemporaryGlueTableCredentialsResponse_secretAccessKey,
    getTemporaryGlueTableCredentialsResponse_accessKeyId,
    getTemporaryGlueTableCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTemporaryGlueTableCredentials' smart constructor.
data GetTemporaryGlueTableCredentials = GetTemporaryGlueTableCredentials'
  { -- | A structure representing context to access a resource (column names,
    -- query ID, etc).
    auditContext :: Prelude.Maybe AuditContext,
    -- | Filters the request based on the user having been granted a list of
    -- specified permissions on the requested resource(s).
    permissions :: Prelude.Maybe [Permission],
    -- | The time period, between 900 and 21,600 seconds, for the timeout of the
    -- temporary credentials.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ARN identifying a table in the Data Catalog for the temporary
    -- credentials request.
    tableArn :: Prelude.Text,
    -- | A list of supported permission types for the table. Valid values are
    -- @COLUMN_PERMISSION@ and @CELL_FILTER_PERMISSION@.
    supportedPermissionTypes :: Prelude.NonEmpty PermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemporaryGlueTableCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditContext', 'getTemporaryGlueTableCredentials_auditContext' - A structure representing context to access a resource (column names,
-- query ID, etc).
--
-- 'permissions', 'getTemporaryGlueTableCredentials_permissions' - Filters the request based on the user having been granted a list of
-- specified permissions on the requested resource(s).
--
-- 'durationSeconds', 'getTemporaryGlueTableCredentials_durationSeconds' - The time period, between 900 and 21,600 seconds, for the timeout of the
-- temporary credentials.
--
-- 'tableArn', 'getTemporaryGlueTableCredentials_tableArn' - The ARN identifying a table in the Data Catalog for the temporary
-- credentials request.
--
-- 'supportedPermissionTypes', 'getTemporaryGlueTableCredentials_supportedPermissionTypes' - A list of supported permission types for the table. Valid values are
-- @COLUMN_PERMISSION@ and @CELL_FILTER_PERMISSION@.
newGetTemporaryGlueTableCredentials ::
  -- | 'tableArn'
  Prelude.Text ->
  -- | 'supportedPermissionTypes'
  Prelude.NonEmpty PermissionType ->
  GetTemporaryGlueTableCredentials
newGetTemporaryGlueTableCredentials
  pTableArn_
  pSupportedPermissionTypes_ =
    GetTemporaryGlueTableCredentials'
      { auditContext =
          Prelude.Nothing,
        permissions = Prelude.Nothing,
        durationSeconds = Prelude.Nothing,
        tableArn = pTableArn_,
        supportedPermissionTypes =
          Lens.coerced
            Lens.# pSupportedPermissionTypes_
      }

-- | A structure representing context to access a resource (column names,
-- query ID, etc).
getTemporaryGlueTableCredentials_auditContext :: Lens.Lens' GetTemporaryGlueTableCredentials (Prelude.Maybe AuditContext)
getTemporaryGlueTableCredentials_auditContext = Lens.lens (\GetTemporaryGlueTableCredentials' {auditContext} -> auditContext) (\s@GetTemporaryGlueTableCredentials' {} a -> s {auditContext = a} :: GetTemporaryGlueTableCredentials)

-- | Filters the request based on the user having been granted a list of
-- specified permissions on the requested resource(s).
getTemporaryGlueTableCredentials_permissions :: Lens.Lens' GetTemporaryGlueTableCredentials (Prelude.Maybe [Permission])
getTemporaryGlueTableCredentials_permissions = Lens.lens (\GetTemporaryGlueTableCredentials' {permissions} -> permissions) (\s@GetTemporaryGlueTableCredentials' {} a -> s {permissions = a} :: GetTemporaryGlueTableCredentials) Prelude.. Lens.mapping Lens.coerced

-- | The time period, between 900 and 21,600 seconds, for the timeout of the
-- temporary credentials.
getTemporaryGlueTableCredentials_durationSeconds :: Lens.Lens' GetTemporaryGlueTableCredentials (Prelude.Maybe Prelude.Natural)
getTemporaryGlueTableCredentials_durationSeconds = Lens.lens (\GetTemporaryGlueTableCredentials' {durationSeconds} -> durationSeconds) (\s@GetTemporaryGlueTableCredentials' {} a -> s {durationSeconds = a} :: GetTemporaryGlueTableCredentials)

-- | The ARN identifying a table in the Data Catalog for the temporary
-- credentials request.
getTemporaryGlueTableCredentials_tableArn :: Lens.Lens' GetTemporaryGlueTableCredentials Prelude.Text
getTemporaryGlueTableCredentials_tableArn = Lens.lens (\GetTemporaryGlueTableCredentials' {tableArn} -> tableArn) (\s@GetTemporaryGlueTableCredentials' {} a -> s {tableArn = a} :: GetTemporaryGlueTableCredentials)

-- | A list of supported permission types for the table. Valid values are
-- @COLUMN_PERMISSION@ and @CELL_FILTER_PERMISSION@.
getTemporaryGlueTableCredentials_supportedPermissionTypes :: Lens.Lens' GetTemporaryGlueTableCredentials (Prelude.NonEmpty PermissionType)
getTemporaryGlueTableCredentials_supportedPermissionTypes = Lens.lens (\GetTemporaryGlueTableCredentials' {supportedPermissionTypes} -> supportedPermissionTypes) (\s@GetTemporaryGlueTableCredentials' {} a -> s {supportedPermissionTypes = a} :: GetTemporaryGlueTableCredentials) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetTemporaryGlueTableCredentials
  where
  type
    AWSResponse GetTemporaryGlueTableCredentials =
      GetTemporaryGlueTableCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTemporaryGlueTableCredentialsResponse'
            Prelude.<$> (x Core..?> "SessionToken")
            Prelude.<*> (x Core..?> "Expiration")
            Prelude.<*> (x Core..?> "SecretAccessKey")
            Prelude.<*> (x Core..?> "AccessKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTemporaryGlueTableCredentials
  where
  hashWithSalt
    _salt
    GetTemporaryGlueTableCredentials' {..} =
      _salt `Prelude.hashWithSalt` auditContext
        `Prelude.hashWithSalt` permissions
        `Prelude.hashWithSalt` durationSeconds
        `Prelude.hashWithSalt` tableArn
        `Prelude.hashWithSalt` supportedPermissionTypes

instance
  Prelude.NFData
    GetTemporaryGlueTableCredentials
  where
  rnf GetTemporaryGlueTableCredentials' {..} =
    Prelude.rnf auditContext
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf tableArn
      `Prelude.seq` Prelude.rnf supportedPermissionTypes

instance
  Core.ToHeaders
    GetTemporaryGlueTableCredentials
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTemporaryGlueTableCredentials where
  toJSON GetTemporaryGlueTableCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AuditContext" Core..=) Prelude.<$> auditContext,
            ("Permissions" Core..=) Prelude.<$> permissions,
            ("DurationSeconds" Core..=)
              Prelude.<$> durationSeconds,
            Prelude.Just ("TableArn" Core..= tableArn),
            Prelude.Just
              ( "SupportedPermissionTypes"
                  Core..= supportedPermissionTypes
              )
          ]
      )

instance Core.ToPath GetTemporaryGlueTableCredentials where
  toPath =
    Prelude.const "/GetTemporaryGlueTableCredentials"

instance
  Core.ToQuery
    GetTemporaryGlueTableCredentials
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTemporaryGlueTableCredentialsResponse' smart constructor.
data GetTemporaryGlueTableCredentialsResponse = GetTemporaryGlueTableCredentialsResponse'
  { -- | The session token for the temporary credentials.
    sessionToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the temporary credentials expire.
    expiration :: Prelude.Maybe Core.POSIX,
    -- | The secret key for the temporary credentials.
    secretAccessKey :: Prelude.Maybe Prelude.Text,
    -- | The access key ID for the temporary credentials.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemporaryGlueTableCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionToken', 'getTemporaryGlueTableCredentialsResponse_sessionToken' - The session token for the temporary credentials.
--
-- 'expiration', 'getTemporaryGlueTableCredentialsResponse_expiration' - The date and time when the temporary credentials expire.
--
-- 'secretAccessKey', 'getTemporaryGlueTableCredentialsResponse_secretAccessKey' - The secret key for the temporary credentials.
--
-- 'accessKeyId', 'getTemporaryGlueTableCredentialsResponse_accessKeyId' - The access key ID for the temporary credentials.
--
-- 'httpStatus', 'getTemporaryGlueTableCredentialsResponse_httpStatus' - The response's http status code.
newGetTemporaryGlueTableCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemporaryGlueTableCredentialsResponse
newGetTemporaryGlueTableCredentialsResponse
  pHttpStatus_ =
    GetTemporaryGlueTableCredentialsResponse'
      { sessionToken =
          Prelude.Nothing,
        expiration = Prelude.Nothing,
        secretAccessKey = Prelude.Nothing,
        accessKeyId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The session token for the temporary credentials.
getTemporaryGlueTableCredentialsResponse_sessionToken :: Lens.Lens' GetTemporaryGlueTableCredentialsResponse (Prelude.Maybe Prelude.Text)
getTemporaryGlueTableCredentialsResponse_sessionToken = Lens.lens (\GetTemporaryGlueTableCredentialsResponse' {sessionToken} -> sessionToken) (\s@GetTemporaryGlueTableCredentialsResponse' {} a -> s {sessionToken = a} :: GetTemporaryGlueTableCredentialsResponse)

-- | The date and time when the temporary credentials expire.
getTemporaryGlueTableCredentialsResponse_expiration :: Lens.Lens' GetTemporaryGlueTableCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getTemporaryGlueTableCredentialsResponse_expiration = Lens.lens (\GetTemporaryGlueTableCredentialsResponse' {expiration} -> expiration) (\s@GetTemporaryGlueTableCredentialsResponse' {} a -> s {expiration = a} :: GetTemporaryGlueTableCredentialsResponse) Prelude.. Lens.mapping Core._Time

-- | The secret key for the temporary credentials.
getTemporaryGlueTableCredentialsResponse_secretAccessKey :: Lens.Lens' GetTemporaryGlueTableCredentialsResponse (Prelude.Maybe Prelude.Text)
getTemporaryGlueTableCredentialsResponse_secretAccessKey = Lens.lens (\GetTemporaryGlueTableCredentialsResponse' {secretAccessKey} -> secretAccessKey) (\s@GetTemporaryGlueTableCredentialsResponse' {} a -> s {secretAccessKey = a} :: GetTemporaryGlueTableCredentialsResponse)

-- | The access key ID for the temporary credentials.
getTemporaryGlueTableCredentialsResponse_accessKeyId :: Lens.Lens' GetTemporaryGlueTableCredentialsResponse (Prelude.Maybe Prelude.Text)
getTemporaryGlueTableCredentialsResponse_accessKeyId = Lens.lens (\GetTemporaryGlueTableCredentialsResponse' {accessKeyId} -> accessKeyId) (\s@GetTemporaryGlueTableCredentialsResponse' {} a -> s {accessKeyId = a} :: GetTemporaryGlueTableCredentialsResponse)

-- | The response's http status code.
getTemporaryGlueTableCredentialsResponse_httpStatus :: Lens.Lens' GetTemporaryGlueTableCredentialsResponse Prelude.Int
getTemporaryGlueTableCredentialsResponse_httpStatus = Lens.lens (\GetTemporaryGlueTableCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetTemporaryGlueTableCredentialsResponse' {} a -> s {httpStatus = a} :: GetTemporaryGlueTableCredentialsResponse)

instance
  Prelude.NFData
    GetTemporaryGlueTableCredentialsResponse
  where
  rnf GetTemporaryGlueTableCredentialsResponse' {..} =
    Prelude.rnf sessionToken
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf httpStatus
