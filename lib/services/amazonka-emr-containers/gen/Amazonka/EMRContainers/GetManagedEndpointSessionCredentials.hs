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
-- Module      : Amazonka.EMRContainers.GetManagedEndpointSessionCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generate a session token to connect to a managed endpoint.
module Amazonka.EMRContainers.GetManagedEndpointSessionCredentials
  ( -- * Creating a Request
    GetManagedEndpointSessionCredentials (..),
    newGetManagedEndpointSessionCredentials,

    -- * Request Lenses
    getManagedEndpointSessionCredentials_clientToken,
    getManagedEndpointSessionCredentials_durationInSeconds,
    getManagedEndpointSessionCredentials_logContext,
    getManagedEndpointSessionCredentials_executionRoleArn,
    getManagedEndpointSessionCredentials_credentialType,
    getManagedEndpointSessionCredentials_endpointIdentifier,
    getManagedEndpointSessionCredentials_virtualClusterIdentifier,

    -- * Destructuring the Response
    GetManagedEndpointSessionCredentialsResponse (..),
    newGetManagedEndpointSessionCredentialsResponse,

    -- * Response Lenses
    getManagedEndpointSessionCredentialsResponse_credentials,
    getManagedEndpointSessionCredentialsResponse_expiresAt,
    getManagedEndpointSessionCredentialsResponse_id,
    getManagedEndpointSessionCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetManagedEndpointSessionCredentials' smart constructor.
data GetManagedEndpointSessionCredentials = GetManagedEndpointSessionCredentials'
  { -- | The client idempotency token of the job run request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Duration in seconds for which the session token is valid. The default
    -- duration is 15 minutes and the maximum is 12 hours.
    durationInSeconds :: Prelude.Maybe Prelude.Int,
    -- | String identifier used to separate sections of the execution logs
    -- uploaded to S3.
    logContext :: Prelude.Maybe Prelude.Text,
    -- | The IAM Execution Role ARN that will be used by the job run.
    executionRoleArn :: Prelude.Text,
    -- | Type of the token requested. Currently supported and default value of
    -- this field is “TOKEN.”
    credentialType :: Prelude.Text,
    -- | The ARN of the managed endpoint for which the request is submitted.
    endpointIdentifier :: Prelude.Text,
    -- | The ARN of the Virtual Cluster which the Managed Endpoint belongs to.
    virtualClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedEndpointSessionCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'getManagedEndpointSessionCredentials_clientToken' - The client idempotency token of the job run request.
--
-- 'durationInSeconds', 'getManagedEndpointSessionCredentials_durationInSeconds' - Duration in seconds for which the session token is valid. The default
-- duration is 15 minutes and the maximum is 12 hours.
--
-- 'logContext', 'getManagedEndpointSessionCredentials_logContext' - String identifier used to separate sections of the execution logs
-- uploaded to S3.
--
-- 'executionRoleArn', 'getManagedEndpointSessionCredentials_executionRoleArn' - The IAM Execution Role ARN that will be used by the job run.
--
-- 'credentialType', 'getManagedEndpointSessionCredentials_credentialType' - Type of the token requested. Currently supported and default value of
-- this field is “TOKEN.”
--
-- 'endpointIdentifier', 'getManagedEndpointSessionCredentials_endpointIdentifier' - The ARN of the managed endpoint for which the request is submitted.
--
-- 'virtualClusterIdentifier', 'getManagedEndpointSessionCredentials_virtualClusterIdentifier' - The ARN of the Virtual Cluster which the Managed Endpoint belongs to.
newGetManagedEndpointSessionCredentials ::
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'credentialType'
  Prelude.Text ->
  -- | 'endpointIdentifier'
  Prelude.Text ->
  -- | 'virtualClusterIdentifier'
  Prelude.Text ->
  GetManagedEndpointSessionCredentials
newGetManagedEndpointSessionCredentials
  pExecutionRoleArn_
  pCredentialType_
  pEndpointIdentifier_
  pVirtualClusterIdentifier_ =
    GetManagedEndpointSessionCredentials'
      { clientToken =
          Prelude.Nothing,
        durationInSeconds = Prelude.Nothing,
        logContext = Prelude.Nothing,
        executionRoleArn = pExecutionRoleArn_,
        credentialType = pCredentialType_,
        endpointIdentifier =
          pEndpointIdentifier_,
        virtualClusterIdentifier =
          pVirtualClusterIdentifier_
      }

-- | The client idempotency token of the job run request.
getManagedEndpointSessionCredentials_clientToken :: Lens.Lens' GetManagedEndpointSessionCredentials (Prelude.Maybe Prelude.Text)
getManagedEndpointSessionCredentials_clientToken = Lens.lens (\GetManagedEndpointSessionCredentials' {clientToken} -> clientToken) (\s@GetManagedEndpointSessionCredentials' {} a -> s {clientToken = a} :: GetManagedEndpointSessionCredentials)

-- | Duration in seconds for which the session token is valid. The default
-- duration is 15 minutes and the maximum is 12 hours.
getManagedEndpointSessionCredentials_durationInSeconds :: Lens.Lens' GetManagedEndpointSessionCredentials (Prelude.Maybe Prelude.Int)
getManagedEndpointSessionCredentials_durationInSeconds = Lens.lens (\GetManagedEndpointSessionCredentials' {durationInSeconds} -> durationInSeconds) (\s@GetManagedEndpointSessionCredentials' {} a -> s {durationInSeconds = a} :: GetManagedEndpointSessionCredentials)

-- | String identifier used to separate sections of the execution logs
-- uploaded to S3.
getManagedEndpointSessionCredentials_logContext :: Lens.Lens' GetManagedEndpointSessionCredentials (Prelude.Maybe Prelude.Text)
getManagedEndpointSessionCredentials_logContext = Lens.lens (\GetManagedEndpointSessionCredentials' {logContext} -> logContext) (\s@GetManagedEndpointSessionCredentials' {} a -> s {logContext = a} :: GetManagedEndpointSessionCredentials)

-- | The IAM Execution Role ARN that will be used by the job run.
getManagedEndpointSessionCredentials_executionRoleArn :: Lens.Lens' GetManagedEndpointSessionCredentials Prelude.Text
getManagedEndpointSessionCredentials_executionRoleArn = Lens.lens (\GetManagedEndpointSessionCredentials' {executionRoleArn} -> executionRoleArn) (\s@GetManagedEndpointSessionCredentials' {} a -> s {executionRoleArn = a} :: GetManagedEndpointSessionCredentials)

-- | Type of the token requested. Currently supported and default value of
-- this field is “TOKEN.”
getManagedEndpointSessionCredentials_credentialType :: Lens.Lens' GetManagedEndpointSessionCredentials Prelude.Text
getManagedEndpointSessionCredentials_credentialType = Lens.lens (\GetManagedEndpointSessionCredentials' {credentialType} -> credentialType) (\s@GetManagedEndpointSessionCredentials' {} a -> s {credentialType = a} :: GetManagedEndpointSessionCredentials)

-- | The ARN of the managed endpoint for which the request is submitted.
getManagedEndpointSessionCredentials_endpointIdentifier :: Lens.Lens' GetManagedEndpointSessionCredentials Prelude.Text
getManagedEndpointSessionCredentials_endpointIdentifier = Lens.lens (\GetManagedEndpointSessionCredentials' {endpointIdentifier} -> endpointIdentifier) (\s@GetManagedEndpointSessionCredentials' {} a -> s {endpointIdentifier = a} :: GetManagedEndpointSessionCredentials)

-- | The ARN of the Virtual Cluster which the Managed Endpoint belongs to.
getManagedEndpointSessionCredentials_virtualClusterIdentifier :: Lens.Lens' GetManagedEndpointSessionCredentials Prelude.Text
getManagedEndpointSessionCredentials_virtualClusterIdentifier = Lens.lens (\GetManagedEndpointSessionCredentials' {virtualClusterIdentifier} -> virtualClusterIdentifier) (\s@GetManagedEndpointSessionCredentials' {} a -> s {virtualClusterIdentifier = a} :: GetManagedEndpointSessionCredentials)

instance
  Core.AWSRequest
    GetManagedEndpointSessionCredentials
  where
  type
    AWSResponse GetManagedEndpointSessionCredentials =
      GetManagedEndpointSessionCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetManagedEndpointSessionCredentialsResponse'
            Prelude.<$> (x Data..?> "credentials")
            Prelude.<*> (x Data..?> "expiresAt")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetManagedEndpointSessionCredentials
  where
  hashWithSalt
    _salt
    GetManagedEndpointSessionCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` durationInSeconds
        `Prelude.hashWithSalt` logContext
        `Prelude.hashWithSalt` executionRoleArn
        `Prelude.hashWithSalt` credentialType
        `Prelude.hashWithSalt` endpointIdentifier
        `Prelude.hashWithSalt` virtualClusterIdentifier

instance
  Prelude.NFData
    GetManagedEndpointSessionCredentials
  where
  rnf GetManagedEndpointSessionCredentials' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf logContext
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf credentialType
      `Prelude.seq` Prelude.rnf endpointIdentifier
      `Prelude.seq` Prelude.rnf virtualClusterIdentifier

instance
  Data.ToHeaders
    GetManagedEndpointSessionCredentials
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
    GetManagedEndpointSessionCredentials
  where
  toJSON GetManagedEndpointSessionCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("durationInSeconds" Data..=)
              Prelude.<$> durationInSeconds,
            ("logContext" Data..=) Prelude.<$> logContext,
            Prelude.Just
              ("executionRoleArn" Data..= executionRoleArn),
            Prelude.Just
              ("credentialType" Data..= credentialType)
          ]
      )

instance
  Data.ToPath
    GetManagedEndpointSessionCredentials
  where
  toPath GetManagedEndpointSessionCredentials' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Data.toBS virtualClusterIdentifier,
        "/endpoints/",
        Data.toBS endpointIdentifier,
        "/credentials"
      ]

instance
  Data.ToQuery
    GetManagedEndpointSessionCredentials
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetManagedEndpointSessionCredentialsResponse' smart constructor.
data GetManagedEndpointSessionCredentialsResponse = GetManagedEndpointSessionCredentialsResponse'
  { -- | The structure containing the session credentials.
    credentials :: Prelude.Maybe Credentials,
    -- | The date and time when the session token will expire.
    expiresAt :: Prelude.Maybe Data.ISO8601,
    -- | The identifier of the session token returned.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedEndpointSessionCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'getManagedEndpointSessionCredentialsResponse_credentials' - The structure containing the session credentials.
--
-- 'expiresAt', 'getManagedEndpointSessionCredentialsResponse_expiresAt' - The date and time when the session token will expire.
--
-- 'id', 'getManagedEndpointSessionCredentialsResponse_id' - The identifier of the session token returned.
--
-- 'httpStatus', 'getManagedEndpointSessionCredentialsResponse_httpStatus' - The response's http status code.
newGetManagedEndpointSessionCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetManagedEndpointSessionCredentialsResponse
newGetManagedEndpointSessionCredentialsResponse
  pHttpStatus_ =
    GetManagedEndpointSessionCredentialsResponse'
      { credentials =
          Prelude.Nothing,
        expiresAt = Prelude.Nothing,
        id = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The structure containing the session credentials.
getManagedEndpointSessionCredentialsResponse_credentials :: Lens.Lens' GetManagedEndpointSessionCredentialsResponse (Prelude.Maybe Credentials)
getManagedEndpointSessionCredentialsResponse_credentials = Lens.lens (\GetManagedEndpointSessionCredentialsResponse' {credentials} -> credentials) (\s@GetManagedEndpointSessionCredentialsResponse' {} a -> s {credentials = a} :: GetManagedEndpointSessionCredentialsResponse)

-- | The date and time when the session token will expire.
getManagedEndpointSessionCredentialsResponse_expiresAt :: Lens.Lens' GetManagedEndpointSessionCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getManagedEndpointSessionCredentialsResponse_expiresAt = Lens.lens (\GetManagedEndpointSessionCredentialsResponse' {expiresAt} -> expiresAt) (\s@GetManagedEndpointSessionCredentialsResponse' {} a -> s {expiresAt = a} :: GetManagedEndpointSessionCredentialsResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier of the session token returned.
getManagedEndpointSessionCredentialsResponse_id :: Lens.Lens' GetManagedEndpointSessionCredentialsResponse (Prelude.Maybe Prelude.Text)
getManagedEndpointSessionCredentialsResponse_id = Lens.lens (\GetManagedEndpointSessionCredentialsResponse' {id} -> id) (\s@GetManagedEndpointSessionCredentialsResponse' {} a -> s {id = a} :: GetManagedEndpointSessionCredentialsResponse)

-- | The response's http status code.
getManagedEndpointSessionCredentialsResponse_httpStatus :: Lens.Lens' GetManagedEndpointSessionCredentialsResponse Prelude.Int
getManagedEndpointSessionCredentialsResponse_httpStatus = Lens.lens (\GetManagedEndpointSessionCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetManagedEndpointSessionCredentialsResponse' {} a -> s {httpStatus = a} :: GetManagedEndpointSessionCredentialsResponse)

instance
  Prelude.NFData
    GetManagedEndpointSessionCredentialsResponse
  where
  rnf GetManagedEndpointSessionCredentialsResponse' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf expiresAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
