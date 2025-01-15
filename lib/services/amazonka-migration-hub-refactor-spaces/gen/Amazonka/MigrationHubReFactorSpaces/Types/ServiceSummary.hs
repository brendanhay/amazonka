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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ServiceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ServiceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse
import Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointSummary
import Amazonka.MigrationHubReFactorSpaces.Types.ServiceEndpointType
import Amazonka.MigrationHubReFactorSpaces.Types.ServiceState
import Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointSummary
import qualified Amazonka.Prelude as Prelude

-- | A summary for the service as a response to @ListServices@.
--
-- /See:/ 'newServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { -- | The unique identifier of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the service creator.
    createdByAccountId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the service is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | A description of the service.
    description :: Prelude.Maybe Prelude.Text,
    -- | The endpoint type of the service.
    endpointType :: Prelude.Maybe ServiceEndpointType,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Any error associated with the service resource.
    error :: Prelude.Maybe ErrorResponse,
    -- | A summary of the configuration for the Lambda endpoint type.
    lambdaEndpoint :: Prelude.Maybe LambdaEndpointSummary,
    -- | A timestamp that indicates when the service was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the service owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the service.
    state :: Prelude.Maybe ServiceState,
    -- | The tags assigned to the service.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The summary of the configuration for the URL endpoint type.
    urlEndpoint :: Prelude.Maybe UrlEndpointSummary,
    -- | The ID of the virtual private cloud (VPC).
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'serviceSummary_applicationId' - The unique identifier of the application.
--
-- 'arn', 'serviceSummary_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'createdByAccountId', 'serviceSummary_createdByAccountId' - The Amazon Web Services account ID of the service creator.
--
-- 'createdTime', 'serviceSummary_createdTime' - A timestamp that indicates when the service is created.
--
-- 'description', 'serviceSummary_description' - A description of the service.
--
-- 'endpointType', 'serviceSummary_endpointType' - The endpoint type of the service.
--
-- 'environmentId', 'serviceSummary_environmentId' - The unique identifier of the environment.
--
-- 'error', 'serviceSummary_error' - Any error associated with the service resource.
--
-- 'lambdaEndpoint', 'serviceSummary_lambdaEndpoint' - A summary of the configuration for the Lambda endpoint type.
--
-- 'lastUpdatedTime', 'serviceSummary_lastUpdatedTime' - A timestamp that indicates when the service was last updated.
--
-- 'name', 'serviceSummary_name' - The name of the service.
--
-- 'ownerAccountId', 'serviceSummary_ownerAccountId' - The Amazon Web Services account ID of the service owner.
--
-- 'serviceId', 'serviceSummary_serviceId' - The unique identifier of the service.
--
-- 'state', 'serviceSummary_state' - The current state of the service.
--
-- 'tags', 'serviceSummary_tags' - The tags assigned to the service.
--
-- 'urlEndpoint', 'serviceSummary_urlEndpoint' - The summary of the configuration for the URL endpoint type.
--
-- 'vpcId', 'serviceSummary_vpcId' - The ID of the virtual private cloud (VPC).
newServiceSummary ::
  ServiceSummary
newServiceSummary =
  ServiceSummary'
    { applicationId = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdByAccountId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      error = Prelude.Nothing,
      lambdaEndpoint = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      urlEndpoint = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The unique identifier of the application.
serviceSummary_applicationId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_applicationId = Lens.lens (\ServiceSummary' {applicationId} -> applicationId) (\s@ServiceSummary' {} a -> s {applicationId = a} :: ServiceSummary)

-- | The Amazon Resource Name (ARN) of the service.
serviceSummary_arn :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_arn = Lens.lens (\ServiceSummary' {arn} -> arn) (\s@ServiceSummary' {} a -> s {arn = a} :: ServiceSummary)

-- | The Amazon Web Services account ID of the service creator.
serviceSummary_createdByAccountId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_createdByAccountId = Lens.lens (\ServiceSummary' {createdByAccountId} -> createdByAccountId) (\s@ServiceSummary' {} a -> s {createdByAccountId = a} :: ServiceSummary)

-- | A timestamp that indicates when the service is created.
serviceSummary_createdTime :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_createdTime = Lens.lens (\ServiceSummary' {createdTime} -> createdTime) (\s@ServiceSummary' {} a -> s {createdTime = a} :: ServiceSummary) Prelude.. Lens.mapping Data._Time

-- | A description of the service.
serviceSummary_description :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_description = Lens.lens (\ServiceSummary' {description} -> description) (\s@ServiceSummary' {} a -> s {description = a} :: ServiceSummary)

-- | The endpoint type of the service.
serviceSummary_endpointType :: Lens.Lens' ServiceSummary (Prelude.Maybe ServiceEndpointType)
serviceSummary_endpointType = Lens.lens (\ServiceSummary' {endpointType} -> endpointType) (\s@ServiceSummary' {} a -> s {endpointType = a} :: ServiceSummary)

-- | The unique identifier of the environment.
serviceSummary_environmentId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_environmentId = Lens.lens (\ServiceSummary' {environmentId} -> environmentId) (\s@ServiceSummary' {} a -> s {environmentId = a} :: ServiceSummary)

-- | Any error associated with the service resource.
serviceSummary_error :: Lens.Lens' ServiceSummary (Prelude.Maybe ErrorResponse)
serviceSummary_error = Lens.lens (\ServiceSummary' {error} -> error) (\s@ServiceSummary' {} a -> s {error = a} :: ServiceSummary)

-- | A summary of the configuration for the Lambda endpoint type.
serviceSummary_lambdaEndpoint :: Lens.Lens' ServiceSummary (Prelude.Maybe LambdaEndpointSummary)
serviceSummary_lambdaEndpoint = Lens.lens (\ServiceSummary' {lambdaEndpoint} -> lambdaEndpoint) (\s@ServiceSummary' {} a -> s {lambdaEndpoint = a} :: ServiceSummary)

-- | A timestamp that indicates when the service was last updated.
serviceSummary_lastUpdatedTime :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_lastUpdatedTime = Lens.lens (\ServiceSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ServiceSummary' {} a -> s {lastUpdatedTime = a} :: ServiceSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the service.
serviceSummary_name :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_name = Lens.lens (\ServiceSummary' {name} -> name) (\s@ServiceSummary' {} a -> s {name = a} :: ServiceSummary)

-- | The Amazon Web Services account ID of the service owner.
serviceSummary_ownerAccountId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_ownerAccountId = Lens.lens (\ServiceSummary' {ownerAccountId} -> ownerAccountId) (\s@ServiceSummary' {} a -> s {ownerAccountId = a} :: ServiceSummary)

-- | The unique identifier of the service.
serviceSummary_serviceId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_serviceId = Lens.lens (\ServiceSummary' {serviceId} -> serviceId) (\s@ServiceSummary' {} a -> s {serviceId = a} :: ServiceSummary)

-- | The current state of the service.
serviceSummary_state :: Lens.Lens' ServiceSummary (Prelude.Maybe ServiceState)
serviceSummary_state = Lens.lens (\ServiceSummary' {state} -> state) (\s@ServiceSummary' {} a -> s {state = a} :: ServiceSummary)

-- | The tags assigned to the service.
serviceSummary_tags :: Lens.Lens' ServiceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
serviceSummary_tags = Lens.lens (\ServiceSummary' {tags} -> tags) (\s@ServiceSummary' {} a -> s {tags = a} :: ServiceSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The summary of the configuration for the URL endpoint type.
serviceSummary_urlEndpoint :: Lens.Lens' ServiceSummary (Prelude.Maybe UrlEndpointSummary)
serviceSummary_urlEndpoint = Lens.lens (\ServiceSummary' {urlEndpoint} -> urlEndpoint) (\s@ServiceSummary' {} a -> s {urlEndpoint = a} :: ServiceSummary)

-- | The ID of the virtual private cloud (VPC).
serviceSummary_vpcId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_vpcId = Lens.lens (\ServiceSummary' {vpcId} -> vpcId) (\s@ServiceSummary' {} a -> s {vpcId = a} :: ServiceSummary)

instance Data.FromJSON ServiceSummary where
  parseJSON =
    Data.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedByAccountId")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EndpointType")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "LambdaEndpoint")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "ServiceId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UrlEndpoint")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable ServiceSummary where
  hashWithSalt _salt ServiceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdByAccountId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` lambdaEndpoint
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` urlEndpoint
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData ServiceSummary where
  rnf ServiceSummary' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf createdByAccountId `Prelude.seq`
          Prelude.rnf createdTime `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf endpointType `Prelude.seq`
                Prelude.rnf environmentId `Prelude.seq`
                  Prelude.rnf error `Prelude.seq`
                    Prelude.rnf lambdaEndpoint `Prelude.seq`
                      Prelude.rnf lastUpdatedTime `Prelude.seq`
                        Prelude.rnf name `Prelude.seq`
                          Prelude.rnf ownerAccountId `Prelude.seq`
                            Prelude.rnf serviceId `Prelude.seq`
                              Prelude.rnf state `Prelude.seq`
                                Prelude.rnf tags `Prelude.seq`
                                  Prelude.rnf urlEndpoint `Prelude.seq`
                                    Prelude.rnf vpcId
