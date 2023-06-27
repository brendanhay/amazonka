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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ApplicationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxySummary
import Amazonka.MigrationHubReFactorSpaces.Types.ApplicationState
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse
import Amazonka.MigrationHubReFactorSpaces.Types.ProxyType
import qualified Amazonka.Prelude as Prelude

-- | The list of @ApplicationSummary@ objects.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | The endpoint URL of the Amazon API Gateway proxy.
    apiGatewayProxy :: Prelude.Maybe ApiGatewayProxySummary,
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
    -- | The tags assigned to the application.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The ID of the virtual private cloud (VPC).
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayProxy', 'applicationSummary_apiGatewayProxy' - The endpoint URL of the Amazon API Gateway proxy.
--
-- 'applicationId', 'applicationSummary_applicationId' - The unique identifier of the application.
--
-- 'arn', 'applicationSummary_arn' - The Amazon Resource Name (ARN) of the application.
--
-- 'createdByAccountId', 'applicationSummary_createdByAccountId' - The Amazon Web Services account ID of the application creator.
--
-- 'createdTime', 'applicationSummary_createdTime' - A timestamp that indicates when the application is created.
--
-- 'environmentId', 'applicationSummary_environmentId' - The unique identifier of the environment.
--
-- 'error', 'applicationSummary_error' - Any error associated with the application resource.
--
-- 'lastUpdatedTime', 'applicationSummary_lastUpdatedTime' - A timestamp that indicates when the application was last updated.
--
-- 'name', 'applicationSummary_name' - The name of the application.
--
-- 'ownerAccountId', 'applicationSummary_ownerAccountId' - The Amazon Web Services account ID of the application owner (which is
-- always the same as the environment owner account ID).
--
-- 'proxyType', 'applicationSummary_proxyType' - The proxy type of the proxy created within the application.
--
-- 'state', 'applicationSummary_state' - The current state of the application.
--
-- 'tags', 'applicationSummary_tags' - The tags assigned to the application.
--
-- 'vpcId', 'applicationSummary_vpcId' - The ID of the virtual private cloud (VPC).
newApplicationSummary ::
  ApplicationSummary
newApplicationSummary =
  ApplicationSummary'
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
      vpcId = Prelude.Nothing
    }

-- | The endpoint URL of the Amazon API Gateway proxy.
applicationSummary_apiGatewayProxy :: Lens.Lens' ApplicationSummary (Prelude.Maybe ApiGatewayProxySummary)
applicationSummary_apiGatewayProxy = Lens.lens (\ApplicationSummary' {apiGatewayProxy} -> apiGatewayProxy) (\s@ApplicationSummary' {} a -> s {apiGatewayProxy = a} :: ApplicationSummary)

-- | The unique identifier of the application.
applicationSummary_applicationId :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_applicationId = Lens.lens (\ApplicationSummary' {applicationId} -> applicationId) (\s@ApplicationSummary' {} a -> s {applicationId = a} :: ApplicationSummary)

-- | The Amazon Resource Name (ARN) of the application.
applicationSummary_arn :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_arn = Lens.lens (\ApplicationSummary' {arn} -> arn) (\s@ApplicationSummary' {} a -> s {arn = a} :: ApplicationSummary)

-- | The Amazon Web Services account ID of the application creator.
applicationSummary_createdByAccountId :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_createdByAccountId = Lens.lens (\ApplicationSummary' {createdByAccountId} -> createdByAccountId) (\s@ApplicationSummary' {} a -> s {createdByAccountId = a} :: ApplicationSummary)

-- | A timestamp that indicates when the application is created.
applicationSummary_createdTime :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.UTCTime)
applicationSummary_createdTime = Lens.lens (\ApplicationSummary' {createdTime} -> createdTime) (\s@ApplicationSummary' {} a -> s {createdTime = a} :: ApplicationSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the environment.
applicationSummary_environmentId :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_environmentId = Lens.lens (\ApplicationSummary' {environmentId} -> environmentId) (\s@ApplicationSummary' {} a -> s {environmentId = a} :: ApplicationSummary)

-- | Any error associated with the application resource.
applicationSummary_error :: Lens.Lens' ApplicationSummary (Prelude.Maybe ErrorResponse)
applicationSummary_error = Lens.lens (\ApplicationSummary' {error} -> error) (\s@ApplicationSummary' {} a -> s {error = a} :: ApplicationSummary)

-- | A timestamp that indicates when the application was last updated.
applicationSummary_lastUpdatedTime :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.UTCTime)
applicationSummary_lastUpdatedTime = Lens.lens (\ApplicationSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ApplicationSummary' {} a -> s {lastUpdatedTime = a} :: ApplicationSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the application.
applicationSummary_name :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_name = Lens.lens (\ApplicationSummary' {name} -> name) (\s@ApplicationSummary' {} a -> s {name = a} :: ApplicationSummary)

-- | The Amazon Web Services account ID of the application owner (which is
-- always the same as the environment owner account ID).
applicationSummary_ownerAccountId :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_ownerAccountId = Lens.lens (\ApplicationSummary' {ownerAccountId} -> ownerAccountId) (\s@ApplicationSummary' {} a -> s {ownerAccountId = a} :: ApplicationSummary)

-- | The proxy type of the proxy created within the application.
applicationSummary_proxyType :: Lens.Lens' ApplicationSummary (Prelude.Maybe ProxyType)
applicationSummary_proxyType = Lens.lens (\ApplicationSummary' {proxyType} -> proxyType) (\s@ApplicationSummary' {} a -> s {proxyType = a} :: ApplicationSummary)

-- | The current state of the application.
applicationSummary_state :: Lens.Lens' ApplicationSummary (Prelude.Maybe ApplicationState)
applicationSummary_state = Lens.lens (\ApplicationSummary' {state} -> state) (\s@ApplicationSummary' {} a -> s {state = a} :: ApplicationSummary)

-- | The tags assigned to the application.
applicationSummary_tags :: Lens.Lens' ApplicationSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
applicationSummary_tags = Lens.lens (\ApplicationSummary' {tags} -> tags) (\s@ApplicationSummary' {} a -> s {tags = a} :: ApplicationSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ID of the virtual private cloud (VPC).
applicationSummary_vpcId :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_vpcId = Lens.lens (\ApplicationSummary' {vpcId} -> vpcId) (\s@ApplicationSummary' {} a -> s {vpcId = a} :: ApplicationSummary)

instance Data.FromJSON ApplicationSummary where
  parseJSON =
    Data.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Prelude.<$> (x Data..:? "ApiGatewayProxy")
            Prelude.<*> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedByAccountId")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "ProxyType")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable ApplicationSummary where
  hashWithSalt _salt ApplicationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` apiGatewayProxy
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdByAccountId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` proxyType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData ApplicationSummary where
  rnf ApplicationSummary' {..} =
    Prelude.rnf apiGatewayProxy
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdByAccountId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf proxyType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
