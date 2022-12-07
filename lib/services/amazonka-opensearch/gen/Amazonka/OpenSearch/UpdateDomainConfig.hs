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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the cluster configuration of the specified Amazon OpenSearch
-- Service domain.
module Amazonka.OpenSearch.UpdateDomainConfig
  ( -- * Creating a Request
    UpdateDomainConfig (..),
    newUpdateDomainConfig,

    -- * Request Lenses
    updateDomainConfig_nodeToNodeEncryptionOptions,
    updateDomainConfig_clusterConfig,
    updateDomainConfig_advancedOptions,
    updateDomainConfig_advancedSecurityOptions,
    updateDomainConfig_cognitoOptions,
    updateDomainConfig_encryptionAtRestOptions,
    updateDomainConfig_dryRun,
    updateDomainConfig_eBSOptions,
    updateDomainConfig_accessPolicies,
    updateDomainConfig_vPCOptions,
    updateDomainConfig_autoTuneOptions,
    updateDomainConfig_domainEndpointOptions,
    updateDomainConfig_snapshotOptions,
    updateDomainConfig_logPublishingOptions,
    updateDomainConfig_domainName,

    -- * Destructuring the Response
    UpdateDomainConfigResponse (..),
    newUpdateDomainConfigResponse,

    -- * Response Lenses
    updateDomainConfigResponse_dryRunResults,
    updateDomainConfigResponse_httpStatus,
    updateDomainConfigResponse_domainConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @UpdateDomain@ operation.
--
-- /See:/ 'newUpdateDomainConfig' smart constructor.
data UpdateDomainConfig = UpdateDomainConfig'
  { -- | Node-To-Node Encryption options for the domain.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | Changes that you want to make to the cluster configuration, such as the
    -- instance type and number of EC2 instances.
    clusterConfig :: Prelude.Maybe ClusterConfig,
    -- | Key-value pairs to specify advanced configuration options. The following
    -- key-value pairs are supported:
    --
    -- -   @\"rest.action.multi.allow_explicit_index\": \"true\" | \"false\"@ -
    --     Note the use of a string rather than a boolean. Specifies whether
    --     explicit references to indexes are allowed inside the body of HTTP
    --     requests. If you want to configure access policies for domain
    --     sub-resources, such as specific indexes and domain APIs, you must
    --     disable this property. Default is true.
    --
    -- -   @\"indices.fielddata.cache.size\": \"80\" @ - Note the use of a
    --     string rather than a boolean. Specifies the percentage of heap space
    --     allocated to field data. Default is unbounded.
    --
    -- -   @\"indices.query.bool.max_clause_count\": \"1024\"@ - Note the use
    --     of a string rather than a boolean. Specifies the maximum number of
    --     clauses allowed in a Lucene boolean query. Default is 1,024. Queries
    --     with more than the permitted number of clauses result in a
    --     @TooManyClauses@ error.
    --
    -- -   @\"override_main_response_version\": \"true\" | \"false\"@ - Note
    --     the use of a string rather than a boolean. Specifies whether the
    --     domain reports its version as 7.10 to allow Elasticsearch OSS
    --     clients and plugins to continue working with it. Default is false
    --     when creating a domain and true when upgrading a domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Options for fine-grained access control.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsInput,
    -- | Key-value pairs to configure Amazon Cognito authentication for
    -- OpenSearch Dashboards.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Encryption at rest options for the domain.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | This flag, when set to True, specifies whether the @UpdateDomain@
    -- request should return the results of validation check without actually
    -- applying the change.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The type and size of the EBS volume to attach to instances in the
    -- domain.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | Identity and Access Management (IAM) access policy as a JSON-formatted
    -- string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Options to specify the subnets and security groups for a VPC endpoint.
    -- For more information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
    vPCOptions :: Prelude.Maybe VPCOptions,
    -- | Options for Auto-Tune.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptions,
    -- | Additional options for the domain endpoint, such as whether to require
    -- HTTPS for all traffic.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | Option to set the time, in UTC format, for the daily automated snapshot.
    -- Default value is @0@ hours.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | Options to publish OpenSearch lots to Amazon CloudWatch Logs.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | The name of the domain that you\'re updating.
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
-- 'nodeToNodeEncryptionOptions', 'updateDomainConfig_nodeToNodeEncryptionOptions' - Node-To-Node Encryption options for the domain.
--
-- 'clusterConfig', 'updateDomainConfig_clusterConfig' - Changes that you want to make to the cluster configuration, such as the
-- instance type and number of EC2 instances.
--
-- 'advancedOptions', 'updateDomainConfig_advancedOptions' - Key-value pairs to specify advanced configuration options. The following
-- key-value pairs are supported:
--
-- -   @\"rest.action.multi.allow_explicit_index\": \"true\" | \"false\"@ -
--     Note the use of a string rather than a boolean. Specifies whether
--     explicit references to indexes are allowed inside the body of HTTP
--     requests. If you want to configure access policies for domain
--     sub-resources, such as specific indexes and domain APIs, you must
--     disable this property. Default is true.
--
-- -   @\"indices.fielddata.cache.size\": \"80\" @ - Note the use of a
--     string rather than a boolean. Specifies the percentage of heap space
--     allocated to field data. Default is unbounded.
--
-- -   @\"indices.query.bool.max_clause_count\": \"1024\"@ - Note the use
--     of a string rather than a boolean. Specifies the maximum number of
--     clauses allowed in a Lucene boolean query. Default is 1,024. Queries
--     with more than the permitted number of clauses result in a
--     @TooManyClauses@ error.
--
-- -   @\"override_main_response_version\": \"true\" | \"false\"@ - Note
--     the use of a string rather than a boolean. Specifies whether the
--     domain reports its version as 7.10 to allow Elasticsearch OSS
--     clients and plugins to continue working with it. Default is false
--     when creating a domain and true when upgrading a domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>.
--
-- 'advancedSecurityOptions', 'updateDomainConfig_advancedSecurityOptions' - Options for fine-grained access control.
--
-- 'cognitoOptions', 'updateDomainConfig_cognitoOptions' - Key-value pairs to configure Amazon Cognito authentication for
-- OpenSearch Dashboards.
--
-- 'encryptionAtRestOptions', 'updateDomainConfig_encryptionAtRestOptions' - Encryption at rest options for the domain.
--
-- 'dryRun', 'updateDomainConfig_dryRun' - This flag, when set to True, specifies whether the @UpdateDomain@
-- request should return the results of validation check without actually
-- applying the change.
--
-- 'eBSOptions', 'updateDomainConfig_eBSOptions' - The type and size of the EBS volume to attach to instances in the
-- domain.
--
-- 'accessPolicies', 'updateDomainConfig_accessPolicies' - Identity and Access Management (IAM) access policy as a JSON-formatted
-- string.
--
-- 'vPCOptions', 'updateDomainConfig_vPCOptions' - Options to specify the subnets and security groups for a VPC endpoint.
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
--
-- 'autoTuneOptions', 'updateDomainConfig_autoTuneOptions' - Options for Auto-Tune.
--
-- 'domainEndpointOptions', 'updateDomainConfig_domainEndpointOptions' - Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
--
-- 'snapshotOptions', 'updateDomainConfig_snapshotOptions' - Option to set the time, in UTC format, for the daily automated snapshot.
-- Default value is @0@ hours.
--
-- 'logPublishingOptions', 'updateDomainConfig_logPublishingOptions' - Options to publish OpenSearch lots to Amazon CloudWatch Logs.
--
-- 'domainName', 'updateDomainConfig_domainName' - The name of the domain that you\'re updating.
newUpdateDomainConfig ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainConfig
newUpdateDomainConfig pDomainName_ =
  UpdateDomainConfig'
    { nodeToNodeEncryptionOptions =
        Prelude.Nothing,
      clusterConfig = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      eBSOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Node-To-Node Encryption options for the domain.
updateDomainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe NodeToNodeEncryptionOptions)
updateDomainConfig_nodeToNodeEncryptionOptions = Lens.lens (\UpdateDomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@UpdateDomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: UpdateDomainConfig)

-- | Changes that you want to make to the cluster configuration, such as the
-- instance type and number of EC2 instances.
updateDomainConfig_clusterConfig :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe ClusterConfig)
updateDomainConfig_clusterConfig = Lens.lens (\UpdateDomainConfig' {clusterConfig} -> clusterConfig) (\s@UpdateDomainConfig' {} a -> s {clusterConfig = a} :: UpdateDomainConfig)

-- | Key-value pairs to specify advanced configuration options. The following
-- key-value pairs are supported:
--
-- -   @\"rest.action.multi.allow_explicit_index\": \"true\" | \"false\"@ -
--     Note the use of a string rather than a boolean. Specifies whether
--     explicit references to indexes are allowed inside the body of HTTP
--     requests. If you want to configure access policies for domain
--     sub-resources, such as specific indexes and domain APIs, you must
--     disable this property. Default is true.
--
-- -   @\"indices.fielddata.cache.size\": \"80\" @ - Note the use of a
--     string rather than a boolean. Specifies the percentage of heap space
--     allocated to field data. Default is unbounded.
--
-- -   @\"indices.query.bool.max_clause_count\": \"1024\"@ - Note the use
--     of a string rather than a boolean. Specifies the maximum number of
--     clauses allowed in a Lucene boolean query. Default is 1,024. Queries
--     with more than the permitted number of clauses result in a
--     @TooManyClauses@ error.
--
-- -   @\"override_main_response_version\": \"true\" | \"false\"@ - Note
--     the use of a string rather than a boolean. Specifies whether the
--     domain reports its version as 7.10 to allow Elasticsearch OSS
--     clients and plugins to continue working with it. Default is false
--     when creating a domain and true when upgrading a domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>.
updateDomainConfig_advancedOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateDomainConfig_advancedOptions = Lens.lens (\UpdateDomainConfig' {advancedOptions} -> advancedOptions) (\s@UpdateDomainConfig' {} a -> s {advancedOptions = a} :: UpdateDomainConfig) Prelude.. Lens.mapping Lens.coerced

-- | Options for fine-grained access control.
updateDomainConfig_advancedSecurityOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe AdvancedSecurityOptionsInput)
updateDomainConfig_advancedSecurityOptions = Lens.lens (\UpdateDomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@UpdateDomainConfig' {} a -> s {advancedSecurityOptions = a} :: UpdateDomainConfig)

-- | Key-value pairs to configure Amazon Cognito authentication for
-- OpenSearch Dashboards.
updateDomainConfig_cognitoOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe CognitoOptions)
updateDomainConfig_cognitoOptions = Lens.lens (\UpdateDomainConfig' {cognitoOptions} -> cognitoOptions) (\s@UpdateDomainConfig' {} a -> s {cognitoOptions = a} :: UpdateDomainConfig)

-- | Encryption at rest options for the domain.
updateDomainConfig_encryptionAtRestOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe EncryptionAtRestOptions)
updateDomainConfig_encryptionAtRestOptions = Lens.lens (\UpdateDomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@UpdateDomainConfig' {} a -> s {encryptionAtRestOptions = a} :: UpdateDomainConfig)

-- | This flag, when set to True, specifies whether the @UpdateDomain@
-- request should return the results of validation check without actually
-- applying the change.
updateDomainConfig_dryRun :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe Prelude.Bool)
updateDomainConfig_dryRun = Lens.lens (\UpdateDomainConfig' {dryRun} -> dryRun) (\s@UpdateDomainConfig' {} a -> s {dryRun = a} :: UpdateDomainConfig)

-- | The type and size of the EBS volume to attach to instances in the
-- domain.
updateDomainConfig_eBSOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe EBSOptions)
updateDomainConfig_eBSOptions = Lens.lens (\UpdateDomainConfig' {eBSOptions} -> eBSOptions) (\s@UpdateDomainConfig' {} a -> s {eBSOptions = a} :: UpdateDomainConfig)

-- | Identity and Access Management (IAM) access policy as a JSON-formatted
-- string.
updateDomainConfig_accessPolicies :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe Prelude.Text)
updateDomainConfig_accessPolicies = Lens.lens (\UpdateDomainConfig' {accessPolicies} -> accessPolicies) (\s@UpdateDomainConfig' {} a -> s {accessPolicies = a} :: UpdateDomainConfig)

-- | Options to specify the subnets and security groups for a VPC endpoint.
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
updateDomainConfig_vPCOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe VPCOptions)
updateDomainConfig_vPCOptions = Lens.lens (\UpdateDomainConfig' {vPCOptions} -> vPCOptions) (\s@UpdateDomainConfig' {} a -> s {vPCOptions = a} :: UpdateDomainConfig)

-- | Options for Auto-Tune.
updateDomainConfig_autoTuneOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe AutoTuneOptions)
updateDomainConfig_autoTuneOptions = Lens.lens (\UpdateDomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@UpdateDomainConfig' {} a -> s {autoTuneOptions = a} :: UpdateDomainConfig)

-- | Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
updateDomainConfig_domainEndpointOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe DomainEndpointOptions)
updateDomainConfig_domainEndpointOptions = Lens.lens (\UpdateDomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@UpdateDomainConfig' {} a -> s {domainEndpointOptions = a} :: UpdateDomainConfig)

-- | Option to set the time, in UTC format, for the daily automated snapshot.
-- Default value is @0@ hours.
updateDomainConfig_snapshotOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe SnapshotOptions)
updateDomainConfig_snapshotOptions = Lens.lens (\UpdateDomainConfig' {snapshotOptions} -> snapshotOptions) (\s@UpdateDomainConfig' {} a -> s {snapshotOptions = a} :: UpdateDomainConfig)

-- | Options to publish OpenSearch lots to Amazon CloudWatch Logs.
updateDomainConfig_logPublishingOptions :: Lens.Lens' UpdateDomainConfig (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
updateDomainConfig_logPublishingOptions = Lens.lens (\UpdateDomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@UpdateDomainConfig' {} a -> s {logPublishingOptions = a} :: UpdateDomainConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain that you\'re updating.
updateDomainConfig_domainName :: Lens.Lens' UpdateDomainConfig Prelude.Text
updateDomainConfig_domainName = Lens.lens (\UpdateDomainConfig' {domainName} -> domainName) (\s@UpdateDomainConfig' {} a -> s {domainName = a} :: UpdateDomainConfig)

instance Core.AWSRequest UpdateDomainConfig where
  type
    AWSResponse UpdateDomainConfig =
      UpdateDomainConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainConfigResponse'
            Prelude.<$> (x Data..?> "DryRunResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainConfig")
      )

instance Prelude.Hashable UpdateDomainConfig where
  hashWithSalt _salt UpdateDomainConfig' {..} =
    _salt
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` clusterConfig
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` vPCOptions
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainConfig where
  rnf UpdateDomainConfig' {..} =
    Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf clusterConfig
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf vPCOptions
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateDomainConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateDomainConfig where
  toJSON UpdateDomainConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NodeToNodeEncryptionOptions" Data..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("ClusterConfig" Data..=) Prelude.<$> clusterConfig,
            ("AdvancedOptions" Data..=)
              Prelude.<$> advancedOptions,
            ("AdvancedSecurityOptions" Data..=)
              Prelude.<$> advancedSecurityOptions,
            ("CognitoOptions" Data..=)
              Prelude.<$> cognitoOptions,
            ("EncryptionAtRestOptions" Data..=)
              Prelude.<$> encryptionAtRestOptions,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("EBSOptions" Data..=) Prelude.<$> eBSOptions,
            ("AccessPolicies" Data..=)
              Prelude.<$> accessPolicies,
            ("VPCOptions" Data..=) Prelude.<$> vPCOptions,
            ("AutoTuneOptions" Data..=)
              Prelude.<$> autoTuneOptions,
            ("DomainEndpointOptions" Data..=)
              Prelude.<$> domainEndpointOptions,
            ("SnapshotOptions" Data..=)
              Prelude.<$> snapshotOptions,
            ("LogPublishingOptions" Data..=)
              Prelude.<$> logPublishingOptions
          ]
      )

instance Data.ToPath UpdateDomainConfig where
  toPath UpdateDomainConfig' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/config"
      ]

instance Data.ToQuery UpdateDomainConfig where
  toQuery = Prelude.const Prelude.mempty

-- | The results of an @UpdateDomain@ request. Contains the status of the
-- domain being updated.
--
-- /See:/ 'newUpdateDomainConfigResponse' smart constructor.
data UpdateDomainConfigResponse = UpdateDomainConfigResponse'
  { -- | Results of a dry run performed in an update domain request.
    dryRunResults :: Prelude.Maybe DryRunResults,
    -- | The response's http status code.
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
-- 'dryRunResults', 'updateDomainConfigResponse_dryRunResults' - Results of a dry run performed in an update domain request.
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
      { dryRunResults =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainConfig = pDomainConfig_
      }

-- | Results of a dry run performed in an update domain request.
updateDomainConfigResponse_dryRunResults :: Lens.Lens' UpdateDomainConfigResponse (Prelude.Maybe DryRunResults)
updateDomainConfigResponse_dryRunResults = Lens.lens (\UpdateDomainConfigResponse' {dryRunResults} -> dryRunResults) (\s@UpdateDomainConfigResponse' {} a -> s {dryRunResults = a} :: UpdateDomainConfigResponse)

-- | The response's http status code.
updateDomainConfigResponse_httpStatus :: Lens.Lens' UpdateDomainConfigResponse Prelude.Int
updateDomainConfigResponse_httpStatus = Lens.lens (\UpdateDomainConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainConfigResponse' {} a -> s {httpStatus = a} :: UpdateDomainConfigResponse)

-- | The status of the updated domain.
updateDomainConfigResponse_domainConfig :: Lens.Lens' UpdateDomainConfigResponse DomainConfig
updateDomainConfigResponse_domainConfig = Lens.lens (\UpdateDomainConfigResponse' {domainConfig} -> domainConfig) (\s@UpdateDomainConfigResponse' {} a -> s {domainConfig = a} :: UpdateDomainConfigResponse)

instance Prelude.NFData UpdateDomainConfigResponse where
  rnf UpdateDomainConfigResponse' {..} =
    Prelude.rnf dryRunResults
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainConfig
