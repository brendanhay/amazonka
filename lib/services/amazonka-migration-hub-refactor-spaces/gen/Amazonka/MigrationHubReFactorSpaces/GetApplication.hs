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
-- Module      : Amazonka.MigrationHubReFactorSpaces.GetApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Amazon Web Services Migration Hub Refactor Spaces application.
module Amazonka.MigrationHubReFactorSpaces.GetApplication
  ( -- * Creating a Request
    GetApplication (..),
    newGetApplication,

    -- * Request Lenses
    getApplication_applicationIdentifier,
    getApplication_environmentIdentifier,

    -- * Destructuring the Response
    GetApplicationResponse (..),
    newGetApplicationResponse,

    -- * Response Lenses
    getApplicationResponse_apiGatewayProxy,
    getApplicationResponse_applicationId,
    getApplicationResponse_arn,
    getApplicationResponse_createdByAccountId,
    getApplicationResponse_createdTime,
    getApplicationResponse_environmentId,
    getApplicationResponse_error,
    getApplicationResponse_lastUpdatedTime,
    getApplicationResponse_name,
    getApplicationResponse_ownerAccountId,
    getApplicationResponse_proxyType,
    getApplicationResponse_state,
    getApplicationResponse_tags,
    getApplicationResponse_vpcId,
    getApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The ID of the application.
    applicationIdentifier :: Prelude.Text,
    -- | The ID of the environment.
    environmentIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIdentifier', 'getApplication_applicationIdentifier' - The ID of the application.
--
-- 'environmentIdentifier', 'getApplication_environmentIdentifier' - The ID of the environment.
newGetApplication ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  GetApplication
newGetApplication
  pApplicationIdentifier_
  pEnvironmentIdentifier_ =
    GetApplication'
      { applicationIdentifier =
          pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_
      }

-- | The ID of the application.
getApplication_applicationIdentifier :: Lens.Lens' GetApplication Prelude.Text
getApplication_applicationIdentifier = Lens.lens (\GetApplication' {applicationIdentifier} -> applicationIdentifier) (\s@GetApplication' {} a -> s {applicationIdentifier = a} :: GetApplication)

-- | The ID of the environment.
getApplication_environmentIdentifier :: Lens.Lens' GetApplication Prelude.Text
getApplication_environmentIdentifier = Lens.lens (\GetApplication' {environmentIdentifier} -> environmentIdentifier) (\s@GetApplication' {} a -> s {environmentIdentifier = a} :: GetApplication)

instance Core.AWSRequest GetApplication where
  type
    AWSResponse GetApplication =
      GetApplicationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Prelude.<$> (x Data..?> "ApiGatewayProxy")
            Prelude.<*> (x Data..?> "ApplicationId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedByAccountId")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "EnvironmentId")
            Prelude.<*> (x Data..?> "Error")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OwnerAccountId")
            Prelude.<*> (x Data..?> "ProxyType")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplication where
  hashWithSalt _salt GetApplication' {..} =
    _salt
      `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier

instance Prelude.NFData GetApplication where
  rnf GetApplication' {..} =
    Prelude.rnf applicationIdentifier `Prelude.seq`
      Prelude.rnf environmentIdentifier

instance Data.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApplication where
  toPath GetApplication' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/applications/",
        Data.toBS applicationIdentifier
      ]

instance Data.ToQuery GetApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | The endpoint URL of the API Gateway proxy.
    apiGatewayProxy :: Prelude.Maybe ApiGatewayProxyConfig,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the application creator.
    createdByAccountId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the application is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Any error associated with the application resource.
    error :: Prelude.Maybe ErrorResponse,
    -- | A timestamp that indicates when the application was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the application owner (which is
    -- always the same as the environment owner account ID).
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The proxy type of the proxy created within the application.
    proxyType :: Prelude.Maybe ProxyType,
    -- | The current state of the application.
    state :: Prelude.Maybe ApplicationState,
    -- | The tags assigned to the application. A tag is a label that you assign
    -- to an Amazon Web Services resource. Each tag consists of a key-value
    -- pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The ID of the virtual private cloud (VPC).
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayProxy', 'getApplicationResponse_apiGatewayProxy' - The endpoint URL of the API Gateway proxy.
--
-- 'applicationId', 'getApplicationResponse_applicationId' - The unique identifier of the application.
--
-- 'arn', 'getApplicationResponse_arn' - The Amazon Resource Name (ARN) of the application.
--
-- 'createdByAccountId', 'getApplicationResponse_createdByAccountId' - The Amazon Web Services account ID of the application creator.
--
-- 'createdTime', 'getApplicationResponse_createdTime' - A timestamp that indicates when the application is created.
--
-- 'environmentId', 'getApplicationResponse_environmentId' - The unique identifier of the environment.
--
-- 'error', 'getApplicationResponse_error' - Any error associated with the application resource.
--
-- 'lastUpdatedTime', 'getApplicationResponse_lastUpdatedTime' - A timestamp that indicates when the application was last updated.
--
-- 'name', 'getApplicationResponse_name' - The name of the application.
--
-- 'ownerAccountId', 'getApplicationResponse_ownerAccountId' - The Amazon Web Services account ID of the application owner (which is
-- always the same as the environment owner account ID).
--
-- 'proxyType', 'getApplicationResponse_proxyType' - The proxy type of the proxy created within the application.
--
-- 'state', 'getApplicationResponse_state' - The current state of the application.
--
-- 'tags', 'getApplicationResponse_tags' - The tags assigned to the application. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
--
-- 'vpcId', 'getApplicationResponse_vpcId' - The ID of the virtual private cloud (VPC).
--
-- 'httpStatus', 'getApplicationResponse_httpStatus' - The response's http status code.
newGetApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApplicationResponse
newGetApplicationResponse pHttpStatus_ =
  GetApplicationResponse'
    { apiGatewayProxy =
        Prelude.Nothing,
      applicationId = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdByAccountId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      error = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      proxyType = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint URL of the API Gateway proxy.
getApplicationResponse_apiGatewayProxy :: Lens.Lens' GetApplicationResponse (Prelude.Maybe ApiGatewayProxyConfig)
getApplicationResponse_apiGatewayProxy = Lens.lens (\GetApplicationResponse' {apiGatewayProxy} -> apiGatewayProxy) (\s@GetApplicationResponse' {} a -> s {apiGatewayProxy = a} :: GetApplicationResponse)

-- | The unique identifier of the application.
getApplicationResponse_applicationId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_applicationId = Lens.lens (\GetApplicationResponse' {applicationId} -> applicationId) (\s@GetApplicationResponse' {} a -> s {applicationId = a} :: GetApplicationResponse)

-- | The Amazon Resource Name (ARN) of the application.
getApplicationResponse_arn :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_arn = Lens.lens (\GetApplicationResponse' {arn} -> arn) (\s@GetApplicationResponse' {} a -> s {arn = a} :: GetApplicationResponse)

-- | The Amazon Web Services account ID of the application creator.
getApplicationResponse_createdByAccountId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_createdByAccountId = Lens.lens (\GetApplicationResponse' {createdByAccountId} -> createdByAccountId) (\s@GetApplicationResponse' {} a -> s {createdByAccountId = a} :: GetApplicationResponse)

-- | A timestamp that indicates when the application is created.
getApplicationResponse_createdTime :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.UTCTime)
getApplicationResponse_createdTime = Lens.lens (\GetApplicationResponse' {createdTime} -> createdTime) (\s@GetApplicationResponse' {} a -> s {createdTime = a} :: GetApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the environment.
getApplicationResponse_environmentId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_environmentId = Lens.lens (\GetApplicationResponse' {environmentId} -> environmentId) (\s@GetApplicationResponse' {} a -> s {environmentId = a} :: GetApplicationResponse)

-- | Any error associated with the application resource.
getApplicationResponse_error :: Lens.Lens' GetApplicationResponse (Prelude.Maybe ErrorResponse)
getApplicationResponse_error = Lens.lens (\GetApplicationResponse' {error} -> error) (\s@GetApplicationResponse' {} a -> s {error = a} :: GetApplicationResponse)

-- | A timestamp that indicates when the application was last updated.
getApplicationResponse_lastUpdatedTime :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.UTCTime)
getApplicationResponse_lastUpdatedTime = Lens.lens (\GetApplicationResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@GetApplicationResponse' {} a -> s {lastUpdatedTime = a} :: GetApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the application.
getApplicationResponse_name :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_name = Lens.lens (\GetApplicationResponse' {name} -> name) (\s@GetApplicationResponse' {} a -> s {name = a} :: GetApplicationResponse)

-- | The Amazon Web Services account ID of the application owner (which is
-- always the same as the environment owner account ID).
getApplicationResponse_ownerAccountId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_ownerAccountId = Lens.lens (\GetApplicationResponse' {ownerAccountId} -> ownerAccountId) (\s@GetApplicationResponse' {} a -> s {ownerAccountId = a} :: GetApplicationResponse)

-- | The proxy type of the proxy created within the application.
getApplicationResponse_proxyType :: Lens.Lens' GetApplicationResponse (Prelude.Maybe ProxyType)
getApplicationResponse_proxyType = Lens.lens (\GetApplicationResponse' {proxyType} -> proxyType) (\s@GetApplicationResponse' {} a -> s {proxyType = a} :: GetApplicationResponse)

-- | The current state of the application.
getApplicationResponse_state :: Lens.Lens' GetApplicationResponse (Prelude.Maybe ApplicationState)
getApplicationResponse_state = Lens.lens (\GetApplicationResponse' {state} -> state) (\s@GetApplicationResponse' {} a -> s {state = a} :: GetApplicationResponse)

-- | The tags assigned to the application. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
getApplicationResponse_tags :: Lens.Lens' GetApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getApplicationResponse_tags = Lens.lens (\GetApplicationResponse' {tags} -> tags) (\s@GetApplicationResponse' {} a -> s {tags = a} :: GetApplicationResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ID of the virtual private cloud (VPC).
getApplicationResponse_vpcId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_vpcId = Lens.lens (\GetApplicationResponse' {vpcId} -> vpcId) (\s@GetApplicationResponse' {} a -> s {vpcId = a} :: GetApplicationResponse)

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Prelude.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

instance Prelude.NFData GetApplicationResponse where
  rnf GetApplicationResponse' {..} =
    Prelude.rnf apiGatewayProxy `Prelude.seq`
      Prelude.rnf applicationId `Prelude.seq`
        Prelude.rnf arn `Prelude.seq`
          Prelude.rnf createdByAccountId `Prelude.seq`
            Prelude.rnf createdTime `Prelude.seq`
              Prelude.rnf environmentId `Prelude.seq`
                Prelude.rnf error `Prelude.seq`
                  Prelude.rnf lastUpdatedTime `Prelude.seq`
                    Prelude.rnf name `Prelude.seq`
                      Prelude.rnf ownerAccountId `Prelude.seq`
                        Prelude.rnf proxyType `Prelude.seq`
                          Prelude.rnf state `Prelude.seq`
                            Prelude.rnf tags `Prelude.seq`
                              Prelude.rnf vpcId `Prelude.seq`
                                Prelude.rnf httpStatus
