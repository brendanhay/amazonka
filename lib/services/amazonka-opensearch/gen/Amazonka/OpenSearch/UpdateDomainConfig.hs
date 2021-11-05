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
-- Module      : Amazonka.OpenSearch.UpdateDomainConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the cluster configuration of the specified domain, such as
-- setting the instance type and the number of instances.
module Amazonka.OpenSearch.UpdateDomainConfig
  ( -- * Creating a Request
    UpdateDomainConfig (..),
    newUpdateDomainConfig,

    -- * Request Lenses
    updateDomainConfig_eBSOptions,
    updateDomainConfig_nodeToNodeEncryptionOptions,
    updateDomainConfig_accessPolicies,
    updateDomainConfig_autoTuneOptions,
    updateDomainConfig_logPublishingOptions,
    updateDomainConfig_clusterConfig,
    updateDomainConfig_advancedSecurityOptions,
    updateDomainConfig_snapshotOptions,
    updateDomainConfig_cognitoOptions,
    updateDomainConfig_encryptionAtRestOptions,
    updateDomainConfig_vPCOptions,
    updateDomainConfig_domainEndpointOptions,
    updateDomainConfig_advancedOptions,
    updateDomainConfig_domainName,

    -- * Destructuring the Response
    UpdateDomainConfigResponse (..),
    newUpdateDomainConfigResponse,

    -- * Response Lenses
    updateDomainConfigResponse_httpStatus,
    updateDomainConfigResponse_domainConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @ UpdateDomain @ operation.
-- Specifies the type and number of instances in the domain cluster.
--
-- /See:/ 'newUpdateDomainConfig' smart constructor.
data UpdateDomainConfig = UpdateDomainConfig'
  { -- | Specify the type and size of the EBS volume to use.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | Specifies node-to-node encryption options.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Specifies Auto-Tune options.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptions,
    -- | Map of @LogType@ and @LogPublishingOption@, each containing options to
    -- publish a given type of OpenSearch log.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | The type and number of instances to instantiate for the domain cluster.
    clusterConfig :: Prelude.Maybe ClusterConfig,
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsInput,
    -- | Option to set the time, in UTC format, for the daily automated snapshot.
    -- Default value is @0@ hours.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | Options to specify the Cognito user and identity pools for OpenSearch
    -- Dashboards authentication. For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Specifies encryption of data at rest options.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | Options to specify the subnets and security groups for the VPC endpoint.
    -- For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
    -- .
    vPCOptions :: Prelude.Maybe VPCOptions,
    -- | Options to specify configuration that will be applied to the domain
    -- endpoint.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | Modifies the advanced option to allow references to indices in an HTTP
    -- request body. Must be @false@ when configuring access to individual
    -- sub-resources. By default, the value is @true@. See
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>
    -- for more information.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the domain you\'re updating.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eBSOptions', 'updateDomainConfig_eBSOptions' - Specify the type and size of the EBS volume to use.
--
-- 'nodeToNodeEncryptionOptions', 'updateDomainConfig_nodeToNodeEncryptionOptions' - Specifies node-to-node encryption options.
--
-- 'accessPolicies', 'updateDomainConfig_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'autoTuneOptions', 'updateDomainConfig_autoTuneOptions' - Specifies Auto-Tune options.
--
-- 'logPublishingOptions', 'updateDomainConfig_logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of OpenSearch log.
--
-- 'clusterConfig', 'updateDomainConfig_clusterConfig' - The type and number of instances to instantiate for the domain cluster.
--
-- 'advancedSecurityOptions', 'updateDomainConfig_advancedSecurityOptions' - Specifies advanced security options.
--
-- 'snapshotOptions', 'updateDomainConfig_snapshotOptions' - Option to set the time, in UTC format, for the daily automated snapshot.
-- Default value is @0@ hours.
--
-- 'cognitoOptions', 'updateDomainConfig_cognitoOptions' - Options to specify the Cognito user and identity pools for OpenSearch
-- Dashboards authentication. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- 'encryptionAtRestOptions', 'updateDomainConfig_encryptionAtRestOptions' - Specifies encryption of data at rest options.
--
-- 'vPCOptions', 'updateDomainConfig_vPCOptions' - Options to specify the subnets and security groups for the VPC endpoint.
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
-- .
--
-- 'domainEndpointOptions', 'updateDomainConfig_domainEndpointOptions' - Options to specify configuration that will be applied to the domain
-- endpoint.
--
-- 'advancedOptions', 'updateDomainConfig_advancedOptions' - Modifies the advanced option to allow references to indices in an HTTP
-- request body. Must be @false@ when configuring access to individual
-- sub-resources. By default, the value is @true@. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>
-- for more information.
--
-- 'domainName', 'updateDomainConfig_domainName' - The name of the domain you\'re updating.
newUpdateDomainConfig ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainConfig
newUpdateDomainConfig pDomainName_ =
  UpdateDomainConfig'
    { eBSOptions = Prelude.Nothing,
      nodeToNodeEncryptionOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      clusterConfig = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Specify the type and size of the EBS volume to use.
updateDomainConfig_eBSOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe EBSOptions)
updateDomainConfig_eBSOptions = Lens.lens (\UpdateDomainConfig' {eBSOptions} -> eBSOptions) (\s@UpdateDomainConfig' {} a -> s {eBSOptions = a} :: UpdateDomainConfig)

-- | Specifies node-to-node encryption options.
updateDomainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe NodeToNodeEncryptionOptions)
updateDomainConfig_nodeToNodeEncryptionOptions = Lens.lens (\UpdateDomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@UpdateDomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: UpdateDomainConfig)

-- | IAM access policy as a JSON-formatted string.
updateDomainConfig_accessPolicies :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe Prelude.Text)
updateDomainConfig_accessPolicies = Lens.lens (\UpdateDomainConfig' {accessPolicies} -> accessPolicies) (\s@UpdateDomainConfig' {} a -> s {accessPolicies = a} :: UpdateDomainConfig)

-- | Specifies Auto-Tune options.
updateDomainConfig_autoTuneOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe AutoTuneOptions)
updateDomainConfig_autoTuneOptions = Lens.lens (\UpdateDomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@UpdateDomainConfig' {} a -> s {autoTuneOptions = a} :: UpdateDomainConfig)

-- | Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of OpenSearch log.
updateDomainConfig_logPublishingOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
updateDomainConfig_logPublishingOptions = Lens.lens (\UpdateDomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@UpdateDomainConfig' {} a -> s {logPublishingOptions = a} :: UpdateDomainConfig) Prelude.. Lens.mapping Lens.coerced

-- | The type and number of instances to instantiate for the domain cluster.
updateDomainConfig_clusterConfig :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe ClusterConfig)
updateDomainConfig_clusterConfig = Lens.lens (\UpdateDomainConfig' {clusterConfig} -> clusterConfig) (\s@UpdateDomainConfig' {} a -> s {clusterConfig = a} :: UpdateDomainConfig)

-- | Specifies advanced security options.
updateDomainConfig_advancedSecurityOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe AdvancedSecurityOptionsInput)
updateDomainConfig_advancedSecurityOptions = Lens.lens (\UpdateDomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@UpdateDomainConfig' {} a -> s {advancedSecurityOptions = a} :: UpdateDomainConfig)

-- | Option to set the time, in UTC format, for the daily automated snapshot.
-- Default value is @0@ hours.
updateDomainConfig_snapshotOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe SnapshotOptions)
updateDomainConfig_snapshotOptions = Lens.lens (\UpdateDomainConfig' {snapshotOptions} -> snapshotOptions) (\s@UpdateDomainConfig' {} a -> s {snapshotOptions = a} :: UpdateDomainConfig)

-- | Options to specify the Cognito user and identity pools for OpenSearch
-- Dashboards authentication. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
updateDomainConfig_cognitoOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe CognitoOptions)
updateDomainConfig_cognitoOptions = Lens.lens (\UpdateDomainConfig' {cognitoOptions} -> cognitoOptions) (\s@UpdateDomainConfig' {} a -> s {cognitoOptions = a} :: UpdateDomainConfig)

-- | Specifies encryption of data at rest options.
updateDomainConfig_encryptionAtRestOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe EncryptionAtRestOptions)
updateDomainConfig_encryptionAtRestOptions = Lens.lens (\UpdateDomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@UpdateDomainConfig' {} a -> s {encryptionAtRestOptions = a} :: UpdateDomainConfig)

-- | Options to specify the subnets and security groups for the VPC endpoint.
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
-- .
updateDomainConfig_vPCOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe VPCOptions)
updateDomainConfig_vPCOptions = Lens.lens (\UpdateDomainConfig' {vPCOptions} -> vPCOptions) (\s@UpdateDomainConfig' {} a -> s {vPCOptions = a} :: UpdateDomainConfig)

-- | Options to specify configuration that will be applied to the domain
-- endpoint.
updateDomainConfig_domainEndpointOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe DomainEndpointOptions)
updateDomainConfig_domainEndpointOptions = Lens.lens (\UpdateDomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@UpdateDomainConfig' {} a -> s {domainEndpointOptions = a} :: UpdateDomainConfig)

-- | Modifies the advanced option to allow references to indices in an HTTP
-- request body. Must be @false@ when configuring access to individual
-- sub-resources. By default, the value is @true@. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>
-- for more information.
updateDomainConfig_advancedOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateDomainConfig_advancedOptions = Lens.lens (\UpdateDomainConfig' {advancedOptions} -> advancedOptions) (\s@UpdateDomainConfig' {} a -> s {advancedOptions = a} :: UpdateDomainConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain you\'re updating.
updateDomainConfig_domainName :: Lens.Lens' UpdateDomainConfig Prelude.Text
updateDomainConfig_domainName = Lens.lens (\UpdateDomainConfig' {domainName} -> domainName) (\s@UpdateDomainConfig' {} a -> s {domainName = a} :: UpdateDomainConfig)

instance Core.AWSRequest UpdateDomainConfig where
  type
    AWSResponse UpdateDomainConfig =
      UpdateDomainConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DomainConfig")
      )

instance Prelude.Hashable UpdateDomainConfig

instance Prelude.NFData UpdateDomainConfig

instance Core.ToHeaders UpdateDomainConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateDomainConfig where
  toJSON UpdateDomainConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EBSOptions" Core..=) Prelude.<$> eBSOptions,
            ("NodeToNodeEncryptionOptions" Core..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("AccessPolicies" Core..=)
              Prelude.<$> accessPolicies,
            ("AutoTuneOptions" Core..=)
              Prelude.<$> autoTuneOptions,
            ("LogPublishingOptions" Core..=)
              Prelude.<$> logPublishingOptions,
            ("ClusterConfig" Core..=) Prelude.<$> clusterConfig,
            ("AdvancedSecurityOptions" Core..=)
              Prelude.<$> advancedSecurityOptions,
            ("SnapshotOptions" Core..=)
              Prelude.<$> snapshotOptions,
            ("CognitoOptions" Core..=)
              Prelude.<$> cognitoOptions,
            ("EncryptionAtRestOptions" Core..=)
              Prelude.<$> encryptionAtRestOptions,
            ("VPCOptions" Core..=) Prelude.<$> vPCOptions,
            ("DomainEndpointOptions" Core..=)
              Prelude.<$> domainEndpointOptions,
            ("AdvancedOptions" Core..=)
              Prelude.<$> advancedOptions
          ]
      )

instance Core.ToPath UpdateDomainConfig where
  toPath UpdateDomainConfig' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Core.toBS domainName,
        "/config"
      ]

instance Core.ToQuery UpdateDomainConfig where
  toQuery = Prelude.const Prelude.mempty

-- | The result of an @UpdateDomain@ request. Contains the status of the
-- domain being updated.
--
-- /See:/ 'newUpdateDomainConfigResponse' smart constructor.
data UpdateDomainConfigResponse = UpdateDomainConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the updated domain.
    domainConfig :: DomainConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDomainConfigResponse_httpStatus' - The response's http status code.
--
-- 'domainConfig', 'updateDomainConfigResponse_domainConfig' - The status of the updated domain.
newUpdateDomainConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainConfig'
  DomainConfig ->
  UpdateDomainConfigResponse
newUpdateDomainConfigResponse
  pHttpStatus_
  pDomainConfig_ =
    UpdateDomainConfigResponse'
      { httpStatus =
          pHttpStatus_,
        domainConfig = pDomainConfig_
      }

-- | The response's http status code.
updateDomainConfigResponse_httpStatus :: Lens.Lens' UpdateDomainConfigResponse Prelude.Int
updateDomainConfigResponse_httpStatus = Lens.lens (\UpdateDomainConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainConfigResponse' {} a -> s {httpStatus = a} :: UpdateDomainConfigResponse)

-- | The status of the updated domain.
updateDomainConfigResponse_domainConfig :: Lens.Lens' UpdateDomainConfigResponse DomainConfig
updateDomainConfigResponse_domainConfig = Lens.lens (\UpdateDomainConfigResponse' {domainConfig} -> domainConfig) (\s@UpdateDomainConfigResponse' {} a -> s {domainConfig = a} :: UpdateDomainConfigResponse)

instance Prelude.NFData UpdateDomainConfigResponse
