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
-- Module      : Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the cluster configuration of the specified Elasticsearch
-- domain, setting as setting the instance type and the number of
-- instances.
module Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
  ( -- * Creating a Request
    UpdateElasticsearchDomainConfig (..),
    newUpdateElasticsearchDomainConfig,

    -- * Request Lenses
    updateElasticsearchDomainConfig_eBSOptions,
    updateElasticsearchDomainConfig_snapshotOptions,
    updateElasticsearchDomainConfig_elasticsearchClusterConfig,
    updateElasticsearchDomainConfig_domainEndpointOptions,
    updateElasticsearchDomainConfig_vPCOptions,
    updateElasticsearchDomainConfig_autoTuneOptions,
    updateElasticsearchDomainConfig_accessPolicies,
    updateElasticsearchDomainConfig_encryptionAtRestOptions,
    updateElasticsearchDomainConfig_cognitoOptions,
    updateElasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    updateElasticsearchDomainConfig_advancedOptions,
    updateElasticsearchDomainConfig_advancedSecurityOptions,
    updateElasticsearchDomainConfig_logPublishingOptions,
    updateElasticsearchDomainConfig_domainName,

    -- * Destructuring the Response
    UpdateElasticsearchDomainConfigResponse (..),
    newUpdateElasticsearchDomainConfigResponse,

    -- * Response Lenses
    updateElasticsearchDomainConfigResponse_httpStatus,
    updateElasticsearchDomainConfigResponse_domainConfig,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @UpdateElasticsearchDomain@
-- operation. Specifies the type and number of instances in the domain
-- cluster.
--
-- /See:/ 'newUpdateElasticsearchDomainConfig' smart constructor.
data UpdateElasticsearchDomainConfig = UpdateElasticsearchDomainConfig'
  { -- | Specify the type and size of the EBS volume that you want to use.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | Option to set the time, in UTC format, for the daily automated snapshot.
    -- Default value is @0@ hours.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | The type and number of instances to instantiate for the domain cluster.
    elasticsearchClusterConfig :: Prelude.Maybe ElasticsearchClusterConfig,
    -- | Options to specify configuration that will be applied to the domain
    -- endpoint.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | Options to specify the subnets and security groups for VPC endpoint. For
    -- more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
    -- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
    vPCOptions :: Prelude.Maybe VPCOptions,
    -- | Specifies Auto-Tune options.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Encryption At Rest Options.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | Options to specify the Cognito user and identity pools for Kibana
    -- authentication. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Specifies the NodeToNodeEncryptionOptions.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | Modifies the advanced option to allow references to indices in an HTTP
    -- request body. Must be @false@ when configuring access to individual
    -- sub-resources. By default, the value is @true@. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
    -- for more information.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsInput,
    -- | Map of @LogType@ and @LogPublishingOption@, each containing options to
    -- publish a given type of Elasticsearch log.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | The name of the Elasticsearch domain that you are updating.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateElasticsearchDomainConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eBSOptions', 'updateElasticsearchDomainConfig_eBSOptions' - Specify the type and size of the EBS volume that you want to use.
--
-- 'snapshotOptions', 'updateElasticsearchDomainConfig_snapshotOptions' - Option to set the time, in UTC format, for the daily automated snapshot.
-- Default value is @0@ hours.
--
-- 'elasticsearchClusterConfig', 'updateElasticsearchDomainConfig_elasticsearchClusterConfig' - The type and number of instances to instantiate for the domain cluster.
--
-- 'domainEndpointOptions', 'updateElasticsearchDomainConfig_domainEndpointOptions' - Options to specify configuration that will be applied to the domain
-- endpoint.
--
-- 'vPCOptions', 'updateElasticsearchDomainConfig_vPCOptions' - Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- 'autoTuneOptions', 'updateElasticsearchDomainConfig_autoTuneOptions' - Specifies Auto-Tune options.
--
-- 'accessPolicies', 'updateElasticsearchDomainConfig_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'encryptionAtRestOptions', 'updateElasticsearchDomainConfig_encryptionAtRestOptions' - Specifies the Encryption At Rest Options.
--
-- 'cognitoOptions', 'updateElasticsearchDomainConfig_cognitoOptions' - Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- 'nodeToNodeEncryptionOptions', 'updateElasticsearchDomainConfig_nodeToNodeEncryptionOptions' - Specifies the NodeToNodeEncryptionOptions.
--
-- 'advancedOptions', 'updateElasticsearchDomainConfig_advancedOptions' - Modifies the advanced option to allow references to indices in an HTTP
-- request body. Must be @false@ when configuring access to individual
-- sub-resources. By default, the value is @true@. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
-- for more information.
--
-- 'advancedSecurityOptions', 'updateElasticsearchDomainConfig_advancedSecurityOptions' - Specifies advanced security options.
--
-- 'logPublishingOptions', 'updateElasticsearchDomainConfig_logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of Elasticsearch log.
--
-- 'domainName', 'updateElasticsearchDomainConfig_domainName' - The name of the Elasticsearch domain that you are updating.
newUpdateElasticsearchDomainConfig ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateElasticsearchDomainConfig
newUpdateElasticsearchDomainConfig pDomainName_ =
  UpdateElasticsearchDomainConfig'
    { eBSOptions =
        Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      elasticsearchClusterConfig =
        Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      nodeToNodeEncryptionOptions =
        Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Specify the type and size of the EBS volume that you want to use.
updateElasticsearchDomainConfig_eBSOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe EBSOptions)
updateElasticsearchDomainConfig_eBSOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {eBSOptions} -> eBSOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {eBSOptions = a} :: UpdateElasticsearchDomainConfig)

-- | Option to set the time, in UTC format, for the daily automated snapshot.
-- Default value is @0@ hours.
updateElasticsearchDomainConfig_snapshotOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe SnapshotOptions)
updateElasticsearchDomainConfig_snapshotOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {snapshotOptions} -> snapshotOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {snapshotOptions = a} :: UpdateElasticsearchDomainConfig)

-- | The type and number of instances to instantiate for the domain cluster.
updateElasticsearchDomainConfig_elasticsearchClusterConfig :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe ElasticsearchClusterConfig)
updateElasticsearchDomainConfig_elasticsearchClusterConfig = Lens.lens (\UpdateElasticsearchDomainConfig' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@UpdateElasticsearchDomainConfig' {} a -> s {elasticsearchClusterConfig = a} :: UpdateElasticsearchDomainConfig)

-- | Options to specify configuration that will be applied to the domain
-- endpoint.
updateElasticsearchDomainConfig_domainEndpointOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe DomainEndpointOptions)
updateElasticsearchDomainConfig_domainEndpointOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {domainEndpointOptions = a} :: UpdateElasticsearchDomainConfig)

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
updateElasticsearchDomainConfig_vPCOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe VPCOptions)
updateElasticsearchDomainConfig_vPCOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {vPCOptions} -> vPCOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {vPCOptions = a} :: UpdateElasticsearchDomainConfig)

-- | Specifies Auto-Tune options.
updateElasticsearchDomainConfig_autoTuneOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe AutoTuneOptions)
updateElasticsearchDomainConfig_autoTuneOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {autoTuneOptions = a} :: UpdateElasticsearchDomainConfig)

-- | IAM access policy as a JSON-formatted string.
updateElasticsearchDomainConfig_accessPolicies :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe Prelude.Text)
updateElasticsearchDomainConfig_accessPolicies = Lens.lens (\UpdateElasticsearchDomainConfig' {accessPolicies} -> accessPolicies) (\s@UpdateElasticsearchDomainConfig' {} a -> s {accessPolicies = a} :: UpdateElasticsearchDomainConfig)

-- | Specifies the Encryption At Rest Options.
updateElasticsearchDomainConfig_encryptionAtRestOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe EncryptionAtRestOptions)
updateElasticsearchDomainConfig_encryptionAtRestOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {encryptionAtRestOptions = a} :: UpdateElasticsearchDomainConfig)

-- | Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
updateElasticsearchDomainConfig_cognitoOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe CognitoOptions)
updateElasticsearchDomainConfig_cognitoOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {cognitoOptions} -> cognitoOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {cognitoOptions = a} :: UpdateElasticsearchDomainConfig)

-- | Specifies the NodeToNodeEncryptionOptions.
updateElasticsearchDomainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe NodeToNodeEncryptionOptions)
updateElasticsearchDomainConfig_nodeToNodeEncryptionOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: UpdateElasticsearchDomainConfig)

-- | Modifies the advanced option to allow references to indices in an HTTP
-- request body. Must be @false@ when configuring access to individual
-- sub-resources. By default, the value is @true@. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
-- for more information.
updateElasticsearchDomainConfig_advancedOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateElasticsearchDomainConfig_advancedOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {advancedOptions} -> advancedOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {advancedOptions = a} :: UpdateElasticsearchDomainConfig) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies advanced security options.
updateElasticsearchDomainConfig_advancedSecurityOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe AdvancedSecurityOptionsInput)
updateElasticsearchDomainConfig_advancedSecurityOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {advancedSecurityOptions = a} :: UpdateElasticsearchDomainConfig)

-- | Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of Elasticsearch log.
updateElasticsearchDomainConfig_logPublishingOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
updateElasticsearchDomainConfig_logPublishingOptions = Lens.lens (\UpdateElasticsearchDomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@UpdateElasticsearchDomainConfig' {} a -> s {logPublishingOptions = a} :: UpdateElasticsearchDomainConfig) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the Elasticsearch domain that you are updating.
updateElasticsearchDomainConfig_domainName :: Lens.Lens' UpdateElasticsearchDomainConfig Prelude.Text
updateElasticsearchDomainConfig_domainName = Lens.lens (\UpdateElasticsearchDomainConfig' {domainName} -> domainName) (\s@UpdateElasticsearchDomainConfig' {} a -> s {domainName = a} :: UpdateElasticsearchDomainConfig)

instance
  Core.AWSRequest
    UpdateElasticsearchDomainConfig
  where
  type
    AWSResponse UpdateElasticsearchDomainConfig =
      UpdateElasticsearchDomainConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateElasticsearchDomainConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DomainConfig")
      )

instance
  Prelude.Hashable
    UpdateElasticsearchDomainConfig

instance
  Prelude.NFData
    UpdateElasticsearchDomainConfig

instance
  Core.ToHeaders
    UpdateElasticsearchDomainConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateElasticsearchDomainConfig where
  toJSON UpdateElasticsearchDomainConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EBSOptions" Core..=) Prelude.<$> eBSOptions,
            ("SnapshotOptions" Core..=)
              Prelude.<$> snapshotOptions,
            ("ElasticsearchClusterConfig" Core..=)
              Prelude.<$> elasticsearchClusterConfig,
            ("DomainEndpointOptions" Core..=)
              Prelude.<$> domainEndpointOptions,
            ("VPCOptions" Core..=) Prelude.<$> vPCOptions,
            ("AutoTuneOptions" Core..=)
              Prelude.<$> autoTuneOptions,
            ("AccessPolicies" Core..=)
              Prelude.<$> accessPolicies,
            ("EncryptionAtRestOptions" Core..=)
              Prelude.<$> encryptionAtRestOptions,
            ("CognitoOptions" Core..=)
              Prelude.<$> cognitoOptions,
            ("NodeToNodeEncryptionOptions" Core..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("AdvancedOptions" Core..=)
              Prelude.<$> advancedOptions,
            ("AdvancedSecurityOptions" Core..=)
              Prelude.<$> advancedSecurityOptions,
            ("LogPublishingOptions" Core..=)
              Prelude.<$> logPublishingOptions
          ]
      )

instance Core.ToPath UpdateElasticsearchDomainConfig where
  toPath UpdateElasticsearchDomainConfig' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Core.toBS domainName,
        "/config"
      ]

instance Core.ToQuery UpdateElasticsearchDomainConfig where
  toQuery = Prelude.const Prelude.mempty

-- | The result of an @UpdateElasticsearchDomain@ request. Contains the
-- status of the Elasticsearch domain being updated.
--
-- /See:/ 'newUpdateElasticsearchDomainConfigResponse' smart constructor.
data UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the updated Elasticsearch domain.
    domainConfig :: ElasticsearchDomainConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateElasticsearchDomainConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateElasticsearchDomainConfigResponse_httpStatus' - The response's http status code.
--
-- 'domainConfig', 'updateElasticsearchDomainConfigResponse_domainConfig' - The status of the updated Elasticsearch domain.
newUpdateElasticsearchDomainConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainConfig'
  ElasticsearchDomainConfig ->
  UpdateElasticsearchDomainConfigResponse
newUpdateElasticsearchDomainConfigResponse
  pHttpStatus_
  pDomainConfig_ =
    UpdateElasticsearchDomainConfigResponse'
      { httpStatus =
          pHttpStatus_,
        domainConfig = pDomainConfig_
      }

-- | The response's http status code.
updateElasticsearchDomainConfigResponse_httpStatus :: Lens.Lens' UpdateElasticsearchDomainConfigResponse Prelude.Int
updateElasticsearchDomainConfigResponse_httpStatus = Lens.lens (\UpdateElasticsearchDomainConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateElasticsearchDomainConfigResponse' {} a -> s {httpStatus = a} :: UpdateElasticsearchDomainConfigResponse)

-- | The status of the updated Elasticsearch domain.
updateElasticsearchDomainConfigResponse_domainConfig :: Lens.Lens' UpdateElasticsearchDomainConfigResponse ElasticsearchDomainConfig
updateElasticsearchDomainConfigResponse_domainConfig = Lens.lens (\UpdateElasticsearchDomainConfigResponse' {domainConfig} -> domainConfig) (\s@UpdateElasticsearchDomainConfigResponse' {} a -> s {domainConfig = a} :: UpdateElasticsearchDomainConfigResponse)

instance
  Prelude.NFData
    UpdateElasticsearchDomainConfigResponse
