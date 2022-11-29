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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ServiceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The tags assigned to the service.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the service is created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account ID of the service creator.
    createdByAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the service.
    state :: Prelude.Maybe ServiceState,
    -- | The summary of the configuration for the URL endpoint type.
    urlEndpoint :: Prelude.Maybe UrlEndpointSummary,
    -- | A timestamp that indicates when the service was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The endpoint type of the service.
    endpointType :: Prelude.Maybe ServiceEndpointType,
    -- | A description of the service.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the service owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | A summary of the configuration for the Lambda endpoint type.
    lambdaEndpoint :: Prelude.Maybe LambdaEndpointSummary,
    -- | The ID of the virtual private cloud (VPC).
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Any error associated with the service resource.
    error :: Prelude.Maybe ErrorResponse,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the service.
    serviceId :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'serviceSummary_tags' - The tags assigned to the service.
--
-- 'name', 'serviceSummary_name' - The name of the service.
--
-- 'createdTime', 'serviceSummary_createdTime' - A timestamp that indicates when the service is created.
--
-- 'createdByAccountId', 'serviceSummary_createdByAccountId' - The Amazon Web Services account ID of the service creator.
--
-- 'arn', 'serviceSummary_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'state', 'serviceSummary_state' - The current state of the service.
--
-- 'urlEndpoint', 'serviceSummary_urlEndpoint' - The summary of the configuration for the URL endpoint type.
--
-- 'lastUpdatedTime', 'serviceSummary_lastUpdatedTime' - A timestamp that indicates when the service was last updated.
--
-- 'endpointType', 'serviceSummary_endpointType' - The endpoint type of the service.
--
-- 'description', 'serviceSummary_description' - A description of the service.
--
-- 'ownerAccountId', 'serviceSummary_ownerAccountId' - The Amazon Web Services account ID of the service owner.
--
-- 'lambdaEndpoint', 'serviceSummary_lambdaEndpoint' - A summary of the configuration for the Lambda endpoint type.
--
-- 'vpcId', 'serviceSummary_vpcId' - The ID of the virtual private cloud (VPC).
--
-- 'environmentId', 'serviceSummary_environmentId' - The unique identifier of the environment.
--
-- 'error', 'serviceSummary_error' - Any error associated with the service resource.
--
-- 'applicationId', 'serviceSummary_applicationId' - The unique identifier of the application.
--
-- 'serviceId', 'serviceSummary_serviceId' - The unique identifier of the service.
newServiceSummary ::
  ServiceSummary
newServiceSummary =
  ServiceSummary'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      createdByAccountId = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      urlEndpoint = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      description = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      lambdaEndpoint = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      error = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      serviceId = Prelude.Nothing
    }

-- | The tags assigned to the service.
serviceSummary_tags :: Lens.Lens' ServiceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
serviceSummary_tags = Lens.lens (\ServiceSummary' {tags} -> tags) (\s@ServiceSummary' {} a -> s {tags = a} :: ServiceSummary) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The name of the service.
serviceSummary_name :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_name = Lens.lens (\ServiceSummary' {name} -> name) (\s@ServiceSummary' {} a -> s {name = a} :: ServiceSummary)

-- | A timestamp that indicates when the service is created.
serviceSummary_createdTime :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_createdTime = Lens.lens (\ServiceSummary' {createdTime} -> createdTime) (\s@ServiceSummary' {} a -> s {createdTime = a} :: ServiceSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account ID of the service creator.
serviceSummary_createdByAccountId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_createdByAccountId = Lens.lens (\ServiceSummary' {createdByAccountId} -> createdByAccountId) (\s@ServiceSummary' {} a -> s {createdByAccountId = a} :: ServiceSummary)

-- | The Amazon Resource Name (ARN) of the service.
serviceSummary_arn :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_arn = Lens.lens (\ServiceSummary' {arn} -> arn) (\s@ServiceSummary' {} a -> s {arn = a} :: ServiceSummary)

-- | The current state of the service.
serviceSummary_state :: Lens.Lens' ServiceSummary (Prelude.Maybe ServiceState)
serviceSummary_state = Lens.lens (\ServiceSummary' {state} -> state) (\s@ServiceSummary' {} a -> s {state = a} :: ServiceSummary)

-- | The summary of the configuration for the URL endpoint type.
serviceSummary_urlEndpoint :: Lens.Lens' ServiceSummary (Prelude.Maybe UrlEndpointSummary)
serviceSummary_urlEndpoint = Lens.lens (\ServiceSummary' {urlEndpoint} -> urlEndpoint) (\s@ServiceSummary' {} a -> s {urlEndpoint = a} :: ServiceSummary)

-- | A timestamp that indicates when the service was last updated.
serviceSummary_lastUpdatedTime :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_lastUpdatedTime = Lens.lens (\ServiceSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ServiceSummary' {} a -> s {lastUpdatedTime = a} :: ServiceSummary) Prelude.. Lens.mapping Core._Time

-- | The endpoint type of the service.
serviceSummary_endpointType :: Lens.Lens' ServiceSummary (Prelude.Maybe ServiceEndpointType)
serviceSummary_endpointType = Lens.lens (\ServiceSummary' {endpointType} -> endpointType) (\s@ServiceSummary' {} a -> s {endpointType = a} :: ServiceSummary)

-- | A description of the service.
serviceSummary_description :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_description = Lens.lens (\ServiceSummary' {description} -> description) (\s@ServiceSummary' {} a -> s {description = a} :: ServiceSummary)

-- | The Amazon Web Services account ID of the service owner.
serviceSummary_ownerAccountId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_ownerAccountId = Lens.lens (\ServiceSummary' {ownerAccountId} -> ownerAccountId) (\s@ServiceSummary' {} a -> s {ownerAccountId = a} :: ServiceSummary)

-- | A summary of the configuration for the Lambda endpoint type.
serviceSummary_lambdaEndpoint :: Lens.Lens' ServiceSummary (Prelude.Maybe LambdaEndpointSummary)
serviceSummary_lambdaEndpoint = Lens.lens (\ServiceSummary' {lambdaEndpoint} -> lambdaEndpoint) (\s@ServiceSummary' {} a -> s {lambdaEndpoint = a} :: ServiceSummary)

-- | The ID of the virtual private cloud (VPC).
serviceSummary_vpcId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_vpcId = Lens.lens (\ServiceSummary' {vpcId} -> vpcId) (\s@ServiceSummary' {} a -> s {vpcId = a} :: ServiceSummary)

-- | The unique identifier of the environment.
serviceSummary_environmentId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_environmentId = Lens.lens (\ServiceSummary' {environmentId} -> environmentId) (\s@ServiceSummary' {} a -> s {environmentId = a} :: ServiceSummary)

-- | Any error associated with the service resource.
serviceSummary_error :: Lens.Lens' ServiceSummary (Prelude.Maybe ErrorResponse)
serviceSummary_error = Lens.lens (\ServiceSummary' {error} -> error) (\s@ServiceSummary' {} a -> s {error = a} :: ServiceSummary)

-- | The unique identifier of the application.
serviceSummary_applicationId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_applicationId = Lens.lens (\ServiceSummary' {applicationId} -> applicationId) (\s@ServiceSummary' {} a -> s {applicationId = a} :: ServiceSummary)

-- | The unique identifier of the service.
serviceSummary_serviceId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_serviceId = Lens.lens (\ServiceSummary' {serviceId} -> serviceId) (\s@ServiceSummary' {} a -> s {serviceId = a} :: ServiceSummary)

instance Core.FromJSON ServiceSummary where
  parseJSON =
    Core.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "CreatedByAccountId")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "UrlEndpoint")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "EndpointType")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "OwnerAccountId")
            Prelude.<*> (x Core..:? "LambdaEndpoint")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "EnvironmentId")
            Prelude.<*> (x Core..:? "Error")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "ServiceId")
      )

instance Prelude.Hashable ServiceSummary where
  hashWithSalt _salt ServiceSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` createdByAccountId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` urlEndpoint
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` lambdaEndpoint
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` serviceId

instance Prelude.NFData ServiceSummary where
  rnf ServiceSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf createdByAccountId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf urlEndpoint
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf lambdaEndpoint
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf serviceId
