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
-- Module      : Amazonka.LakeFormation.GetTemporaryGluePartitionCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is identical to @GetTemporaryTableCredentials@ except that this
-- is used when the target Data Catalog resource is of type Partition. Lake
-- Formation restricts the permission of the vended credentials with the
-- same scope down policy which restricts access to a single Amazon S3
-- prefix.
module Amazonka.LakeFormation.GetTemporaryGluePartitionCredentials
  ( -- * Creating a Request
    GetTemporaryGluePartitionCredentials (..),
    newGetTemporaryGluePartitionCredentials,

    -- * Request Lenses
    getTemporaryGluePartitionCredentials_auditContext,
    getTemporaryGluePartitionCredentials_permissions,
    getTemporaryGluePartitionCredentials_durationSeconds,
    getTemporaryGluePartitionCredentials_tableArn,
    getTemporaryGluePartitionCredentials_partition,
    getTemporaryGluePartitionCredentials_supportedPermissionTypes,

    -- * Destructuring the Response
    GetTemporaryGluePartitionCredentialsResponse (..),
    newGetTemporaryGluePartitionCredentialsResponse,

    -- * Response Lenses
    getTemporaryGluePartitionCredentialsResponse_sessionToken,
    getTemporaryGluePartitionCredentialsResponse_expiration,
    getTemporaryGluePartitionCredentialsResponse_secretAccessKey,
    getTemporaryGluePartitionCredentialsResponse_accessKeyId,
    getTemporaryGluePartitionCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTemporaryGluePartitionCredentials' smart constructor.
data GetTemporaryGluePartitionCredentials = GetTemporaryGluePartitionCredentials'
  { -- | A structure representing context to access a resource (column names,
    -- query ID, etc).
    auditContext :: Prelude.Maybe AuditContext,
    -- | Filters the request based on the user having been granted a list of
    -- specified permissions on the requested resource(s).
    permissions :: Prelude.Maybe [Permission],
    -- | The time period, between 900 and 21,600 seconds, for the timeout of the
    -- temporary credentials.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the partitions\' table.
    tableArn :: Prelude.Text,
    -- | A list of partition values identifying a single partition.
    partition :: PartitionValueList,
    -- | A list of supported permission types for the partition. Valid values are
    -- @COLUMN_PERMISSION@ and @CELL_FILTER_PERMISSION@.
    supportedPermissionTypes :: Prelude.NonEmpty PermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemporaryGluePartitionCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditContext', 'getTemporaryGluePartitionCredentials_auditContext' - A structure representing context to access a resource (column names,
-- query ID, etc).
--
-- 'permissions', 'getTemporaryGluePartitionCredentials_permissions' - Filters the request based on the user having been granted a list of
-- specified permissions on the requested resource(s).
--
-- 'durationSeconds', 'getTemporaryGluePartitionCredentials_durationSeconds' - The time period, between 900 and 21,600 seconds, for the timeout of the
-- temporary credentials.
--
-- 'tableArn', 'getTemporaryGluePartitionCredentials_tableArn' - The ARN of the partitions\' table.
--
-- 'partition', 'getTemporaryGluePartitionCredentials_partition' - A list of partition values identifying a single partition.
--
-- 'supportedPermissionTypes', 'getTemporaryGluePartitionCredentials_supportedPermissionTypes' - A list of supported permission types for the partition. Valid values are
-- @COLUMN_PERMISSION@ and @CELL_FILTER_PERMISSION@.
newGetTemporaryGluePartitionCredentials ::
  -- | 'tableArn'
  Prelude.Text ->
  -- | 'partition'
  PartitionValueList ->
  -- | 'supportedPermissionTypes'
  Prelude.NonEmpty PermissionType ->
  GetTemporaryGluePartitionCredentials
newGetTemporaryGluePartitionCredentials
  pTableArn_
  pPartition_
  pSupportedPermissionTypes_ =
    GetTemporaryGluePartitionCredentials'
      { auditContext =
          Prelude.Nothing,
        permissions = Prelude.Nothing,
        durationSeconds = Prelude.Nothing,
        tableArn = pTableArn_,
        partition = pPartition_,
        supportedPermissionTypes =
          Lens.coerced
            Lens.# pSupportedPermissionTypes_
      }

-- | A structure representing context to access a resource (column names,
-- query ID, etc).
getTemporaryGluePartitionCredentials_auditContext :: Lens.Lens' GetTemporaryGluePartitionCredentials (Prelude.Maybe AuditContext)
getTemporaryGluePartitionCredentials_auditContext = Lens.lens (\GetTemporaryGluePartitionCredentials' {auditContext} -> auditContext) (\s@GetTemporaryGluePartitionCredentials' {} a -> s {auditContext = a} :: GetTemporaryGluePartitionCredentials)

-- | Filters the request based on the user having been granted a list of
-- specified permissions on the requested resource(s).
getTemporaryGluePartitionCredentials_permissions :: Lens.Lens' GetTemporaryGluePartitionCredentials (Prelude.Maybe [Permission])
getTemporaryGluePartitionCredentials_permissions = Lens.lens (\GetTemporaryGluePartitionCredentials' {permissions} -> permissions) (\s@GetTemporaryGluePartitionCredentials' {} a -> s {permissions = a} :: GetTemporaryGluePartitionCredentials) Prelude.. Lens.mapping Lens.coerced

-- | The time period, between 900 and 21,600 seconds, for the timeout of the
-- temporary credentials.
getTemporaryGluePartitionCredentials_durationSeconds :: Lens.Lens' GetTemporaryGluePartitionCredentials (Prelude.Maybe Prelude.Natural)
getTemporaryGluePartitionCredentials_durationSeconds = Lens.lens (\GetTemporaryGluePartitionCredentials' {durationSeconds} -> durationSeconds) (\s@GetTemporaryGluePartitionCredentials' {} a -> s {durationSeconds = a} :: GetTemporaryGluePartitionCredentials)

-- | The ARN of the partitions\' table.
getTemporaryGluePartitionCredentials_tableArn :: Lens.Lens' GetTemporaryGluePartitionCredentials Prelude.Text
getTemporaryGluePartitionCredentials_tableArn = Lens.lens (\GetTemporaryGluePartitionCredentials' {tableArn} -> tableArn) (\s@GetTemporaryGluePartitionCredentials' {} a -> s {tableArn = a} :: GetTemporaryGluePartitionCredentials)

-- | A list of partition values identifying a single partition.
getTemporaryGluePartitionCredentials_partition :: Lens.Lens' GetTemporaryGluePartitionCredentials PartitionValueList
getTemporaryGluePartitionCredentials_partition = Lens.lens (\GetTemporaryGluePartitionCredentials' {partition} -> partition) (\s@GetTemporaryGluePartitionCredentials' {} a -> s {partition = a} :: GetTemporaryGluePartitionCredentials)

-- | A list of supported permission types for the partition. Valid values are
-- @COLUMN_PERMISSION@ and @CELL_FILTER_PERMISSION@.
getTemporaryGluePartitionCredentials_supportedPermissionTypes :: Lens.Lens' GetTemporaryGluePartitionCredentials (Prelude.NonEmpty PermissionType)
getTemporaryGluePartitionCredentials_supportedPermissionTypes = Lens.lens (\GetTemporaryGluePartitionCredentials' {supportedPermissionTypes} -> supportedPermissionTypes) (\s@GetTemporaryGluePartitionCredentials' {} a -> s {supportedPermissionTypes = a} :: GetTemporaryGluePartitionCredentials) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetTemporaryGluePartitionCredentials
  where
  type
    AWSResponse GetTemporaryGluePartitionCredentials =
      GetTemporaryGluePartitionCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTemporaryGluePartitionCredentialsResponse'
            Prelude.<$> (x Data..?> "SessionToken")
              Prelude.<*> (x Data..?> "Expiration")
              Prelude.<*> (x Data..?> "SecretAccessKey")
              Prelude.<*> (x Data..?> "AccessKeyId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTemporaryGluePartitionCredentials
  where
  hashWithSalt
    _salt
    GetTemporaryGluePartitionCredentials' {..} =
      _salt `Prelude.hashWithSalt` auditContext
        `Prelude.hashWithSalt` permissions
        `Prelude.hashWithSalt` durationSeconds
        `Prelude.hashWithSalt` tableArn
        `Prelude.hashWithSalt` partition
        `Prelude.hashWithSalt` supportedPermissionTypes

instance
  Prelude.NFData
    GetTemporaryGluePartitionCredentials
  where
  rnf GetTemporaryGluePartitionCredentials' {..} =
    Prelude.rnf auditContext
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf tableArn
      `Prelude.seq` Prelude.rnf partition
      `Prelude.seq` Prelude.rnf supportedPermissionTypes

instance
  Data.ToHeaders
    GetTemporaryGluePartitionCredentials
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetTemporaryGluePartitionCredentials
  where
  toJSON GetTemporaryGluePartitionCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditContext" Data..=) Prelude.<$> auditContext,
            ("Permissions" Data..=) Prelude.<$> permissions,
            ("DurationSeconds" Data..=)
              Prelude.<$> durationSeconds,
            Prelude.Just ("TableArn" Data..= tableArn),
            Prelude.Just ("Partition" Data..= partition),
            Prelude.Just
              ( "SupportedPermissionTypes"
                  Data..= supportedPermissionTypes
              )
          ]
      )

instance
  Data.ToPath
    GetTemporaryGluePartitionCredentials
  where
  toPath =
    Prelude.const
      "/GetTemporaryGluePartitionCredentials"

instance
  Data.ToQuery
    GetTemporaryGluePartitionCredentials
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTemporaryGluePartitionCredentialsResponse' smart constructor.
data GetTemporaryGluePartitionCredentialsResponse = GetTemporaryGluePartitionCredentialsResponse'
  { -- | The session token for the temporary credentials.
    sessionToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the temporary credentials expire.
    expiration :: Prelude.Maybe Data.POSIX,
    -- | The secret key for the temporary credentials.
    secretAccessKey :: Prelude.Maybe Prelude.Text,
    -- | The access key ID for the temporary credentials.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemporaryGluePartitionCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionToken', 'getTemporaryGluePartitionCredentialsResponse_sessionToken' - The session token for the temporary credentials.
--
-- 'expiration', 'getTemporaryGluePartitionCredentialsResponse_expiration' - The date and time when the temporary credentials expire.
--
-- 'secretAccessKey', 'getTemporaryGluePartitionCredentialsResponse_secretAccessKey' - The secret key for the temporary credentials.
--
-- 'accessKeyId', 'getTemporaryGluePartitionCredentialsResponse_accessKeyId' - The access key ID for the temporary credentials.
--
-- 'httpStatus', 'getTemporaryGluePartitionCredentialsResponse_httpStatus' - The response's http status code.
newGetTemporaryGluePartitionCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemporaryGluePartitionCredentialsResponse
newGetTemporaryGluePartitionCredentialsResponse
  pHttpStatus_ =
    GetTemporaryGluePartitionCredentialsResponse'
      { sessionToken =
          Prelude.Nothing,
        expiration = Prelude.Nothing,
        secretAccessKey =
          Prelude.Nothing,
        accessKeyId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The session token for the temporary credentials.
getTemporaryGluePartitionCredentialsResponse_sessionToken :: Lens.Lens' GetTemporaryGluePartitionCredentialsResponse (Prelude.Maybe Prelude.Text)
getTemporaryGluePartitionCredentialsResponse_sessionToken = Lens.lens (\GetTemporaryGluePartitionCredentialsResponse' {sessionToken} -> sessionToken) (\s@GetTemporaryGluePartitionCredentialsResponse' {} a -> s {sessionToken = a} :: GetTemporaryGluePartitionCredentialsResponse)

-- | The date and time when the temporary credentials expire.
getTemporaryGluePartitionCredentialsResponse_expiration :: Lens.Lens' GetTemporaryGluePartitionCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getTemporaryGluePartitionCredentialsResponse_expiration = Lens.lens (\GetTemporaryGluePartitionCredentialsResponse' {expiration} -> expiration) (\s@GetTemporaryGluePartitionCredentialsResponse' {} a -> s {expiration = a} :: GetTemporaryGluePartitionCredentialsResponse) Prelude.. Lens.mapping Data._Time

-- | The secret key for the temporary credentials.
getTemporaryGluePartitionCredentialsResponse_secretAccessKey :: Lens.Lens' GetTemporaryGluePartitionCredentialsResponse (Prelude.Maybe Prelude.Text)
getTemporaryGluePartitionCredentialsResponse_secretAccessKey = Lens.lens (\GetTemporaryGluePartitionCredentialsResponse' {secretAccessKey} -> secretAccessKey) (\s@GetTemporaryGluePartitionCredentialsResponse' {} a -> s {secretAccessKey = a} :: GetTemporaryGluePartitionCredentialsResponse)

-- | The access key ID for the temporary credentials.
getTemporaryGluePartitionCredentialsResponse_accessKeyId :: Lens.Lens' GetTemporaryGluePartitionCredentialsResponse (Prelude.Maybe Prelude.Text)
getTemporaryGluePartitionCredentialsResponse_accessKeyId = Lens.lens (\GetTemporaryGluePartitionCredentialsResponse' {accessKeyId} -> accessKeyId) (\s@GetTemporaryGluePartitionCredentialsResponse' {} a -> s {accessKeyId = a} :: GetTemporaryGluePartitionCredentialsResponse)

-- | The response's http status code.
getTemporaryGluePartitionCredentialsResponse_httpStatus :: Lens.Lens' GetTemporaryGluePartitionCredentialsResponse Prelude.Int
getTemporaryGluePartitionCredentialsResponse_httpStatus = Lens.lens (\GetTemporaryGluePartitionCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetTemporaryGluePartitionCredentialsResponse' {} a -> s {httpStatus = a} :: GetTemporaryGluePartitionCredentialsResponse)

instance
  Prelude.NFData
    GetTemporaryGluePartitionCredentialsResponse
  where
  rnf GetTemporaryGluePartitionCredentialsResponse' {..} =
    Prelude.rnf sessionToken
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf httpStatus
