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
-- Module      : Amazonka.OpenSearch.CreateDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html Creating and managing Amazon OpenSearch Service domains>.
module Amazonka.OpenSearch.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_nodeToNodeEncryptionOptions,
    createDomain_clusterConfig,
    createDomain_advancedOptions,
    createDomain_tagList,
    createDomain_advancedSecurityOptions,
    createDomain_cognitoOptions,
    createDomain_encryptionAtRestOptions,
    createDomain_eBSOptions,
    createDomain_accessPolicies,
    createDomain_vPCOptions,
    createDomain_autoTuneOptions,
    createDomain_domainEndpointOptions,
    createDomain_snapshotOptions,
    createDomain_logPublishingOptions,
    createDomain_engineVersion,
    createDomain_domainName,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | Enables node-to-node encryption.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | Container for the cluster configuration of a domain.
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
    -- | List of tags to add to the domain upon creation.
    tagList :: Prelude.Maybe [Tag],
    -- | Options for fine-grained access control.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsInput,
    -- | Key-value pairs to configure Amazon Cognito authentication. For more
    -- information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Key-value pairs to enable encryption at rest.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | Container for the parameters required to enable EBS-based storage for an
    -- OpenSearch Service domain.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | Identity and Access Management (IAM) policy document specifying the
    -- access policies for the new domain.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Container for the values required to configure VPC access domains. If
    -- you don\'t specify these values, OpenSearch Service creates the domain
    -- with a public endpoint. For more information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
    vPCOptions :: Prelude.Maybe VPCOptions,
    -- | Options for Auto-Tune.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsInput,
    -- | Additional options for the domain endpoint, such as whether to require
    -- HTTPS for all traffic.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | DEPRECATED. Container for the parameters required to configure automated
    -- snapshots of domain indexes.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | Key-value pairs to configure slow log publishing.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
    -- engine version for the OpenSearch Service domain. For example,
    -- @OpenSearch_1.0@ or @Elasticsearch_7.9@. For more information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Name of the OpenSearch Service domain to create. Domain names are unique
    -- across the domains owned by an account within an Amazon Web Services
    -- Region.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeToNodeEncryptionOptions', 'createDomain_nodeToNodeEncryptionOptions' - Enables node-to-node encryption.
--
-- 'clusterConfig', 'createDomain_clusterConfig' - Container for the cluster configuration of a domain.
--
-- 'advancedOptions', 'createDomain_advancedOptions' - Key-value pairs to specify advanced configuration options. The following
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
-- 'tagList', 'createDomain_tagList' - List of tags to add to the domain upon creation.
--
-- 'advancedSecurityOptions', 'createDomain_advancedSecurityOptions' - Options for fine-grained access control.
--
-- 'cognitoOptions', 'createDomain_cognitoOptions' - Key-value pairs to configure Amazon Cognito authentication. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- 'encryptionAtRestOptions', 'createDomain_encryptionAtRestOptions' - Key-value pairs to enable encryption at rest.
--
-- 'eBSOptions', 'createDomain_eBSOptions' - Container for the parameters required to enable EBS-based storage for an
-- OpenSearch Service domain.
--
-- 'accessPolicies', 'createDomain_accessPolicies' - Identity and Access Management (IAM) policy document specifying the
-- access policies for the new domain.
--
-- 'vPCOptions', 'createDomain_vPCOptions' - Container for the values required to configure VPC access domains. If
-- you don\'t specify these values, OpenSearch Service creates the domain
-- with a public endpoint. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
--
-- 'autoTuneOptions', 'createDomain_autoTuneOptions' - Options for Auto-Tune.
--
-- 'domainEndpointOptions', 'createDomain_domainEndpointOptions' - Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
--
-- 'snapshotOptions', 'createDomain_snapshotOptions' - DEPRECATED. Container for the parameters required to configure automated
-- snapshots of domain indexes.
--
-- 'logPublishingOptions', 'createDomain_logPublishingOptions' - Key-value pairs to configure slow log publishing.
--
-- 'engineVersion', 'createDomain_engineVersion' - String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the OpenSearch Service domain. For example,
-- @OpenSearch_1.0@ or @Elasticsearch_7.9@. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>.
--
-- 'domainName', 'createDomain_domainName' - Name of the OpenSearch Service domain to create. Domain names are unique
-- across the domains owned by an account within an Amazon Web Services
-- Region.
newCreateDomain ::
  -- | 'domainName'
  Prelude.Text ->
  CreateDomain
newCreateDomain pDomainName_ =
  CreateDomain'
    { nodeToNodeEncryptionOptions =
        Prelude.Nothing,
      clusterConfig = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      tagList = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      eBSOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Enables node-to-node encryption.
createDomain_nodeToNodeEncryptionOptions :: Lens.Lens' CreateDomain (Prelude.Maybe NodeToNodeEncryptionOptions)
createDomain_nodeToNodeEncryptionOptions = Lens.lens (\CreateDomain' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@CreateDomain' {} a -> s {nodeToNodeEncryptionOptions = a} :: CreateDomain)

-- | Container for the cluster configuration of a domain.
createDomain_clusterConfig :: Lens.Lens' CreateDomain (Prelude.Maybe ClusterConfig)
createDomain_clusterConfig = Lens.lens (\CreateDomain' {clusterConfig} -> clusterConfig) (\s@CreateDomain' {} a -> s {clusterConfig = a} :: CreateDomain)

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
createDomain_advancedOptions :: Lens.Lens' CreateDomain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomain_advancedOptions = Lens.lens (\CreateDomain' {advancedOptions} -> advancedOptions) (\s@CreateDomain' {} a -> s {advancedOptions = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | List of tags to add to the domain upon creation.
createDomain_tagList :: Lens.Lens' CreateDomain (Prelude.Maybe [Tag])
createDomain_tagList = Lens.lens (\CreateDomain' {tagList} -> tagList) (\s@CreateDomain' {} a -> s {tagList = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | Options for fine-grained access control.
createDomain_advancedSecurityOptions :: Lens.Lens' CreateDomain (Prelude.Maybe AdvancedSecurityOptionsInput)
createDomain_advancedSecurityOptions = Lens.lens (\CreateDomain' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@CreateDomain' {} a -> s {advancedSecurityOptions = a} :: CreateDomain)

-- | Key-value pairs to configure Amazon Cognito authentication. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
createDomain_cognitoOptions :: Lens.Lens' CreateDomain (Prelude.Maybe CognitoOptions)
createDomain_cognitoOptions = Lens.lens (\CreateDomain' {cognitoOptions} -> cognitoOptions) (\s@CreateDomain' {} a -> s {cognitoOptions = a} :: CreateDomain)

-- | Key-value pairs to enable encryption at rest.
createDomain_encryptionAtRestOptions :: Lens.Lens' CreateDomain (Prelude.Maybe EncryptionAtRestOptions)
createDomain_encryptionAtRestOptions = Lens.lens (\CreateDomain' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@CreateDomain' {} a -> s {encryptionAtRestOptions = a} :: CreateDomain)

-- | Container for the parameters required to enable EBS-based storage for an
-- OpenSearch Service domain.
createDomain_eBSOptions :: Lens.Lens' CreateDomain (Prelude.Maybe EBSOptions)
createDomain_eBSOptions = Lens.lens (\CreateDomain' {eBSOptions} -> eBSOptions) (\s@CreateDomain' {} a -> s {eBSOptions = a} :: CreateDomain)

-- | Identity and Access Management (IAM) policy document specifying the
-- access policies for the new domain.
createDomain_accessPolicies :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_accessPolicies = Lens.lens (\CreateDomain' {accessPolicies} -> accessPolicies) (\s@CreateDomain' {} a -> s {accessPolicies = a} :: CreateDomain)

-- | Container for the values required to configure VPC access domains. If
-- you don\'t specify these values, OpenSearch Service creates the domain
-- with a public endpoint. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
createDomain_vPCOptions :: Lens.Lens' CreateDomain (Prelude.Maybe VPCOptions)
createDomain_vPCOptions = Lens.lens (\CreateDomain' {vPCOptions} -> vPCOptions) (\s@CreateDomain' {} a -> s {vPCOptions = a} :: CreateDomain)

-- | Options for Auto-Tune.
createDomain_autoTuneOptions :: Lens.Lens' CreateDomain (Prelude.Maybe AutoTuneOptionsInput)
createDomain_autoTuneOptions = Lens.lens (\CreateDomain' {autoTuneOptions} -> autoTuneOptions) (\s@CreateDomain' {} a -> s {autoTuneOptions = a} :: CreateDomain)

-- | Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
createDomain_domainEndpointOptions :: Lens.Lens' CreateDomain (Prelude.Maybe DomainEndpointOptions)
createDomain_domainEndpointOptions = Lens.lens (\CreateDomain' {domainEndpointOptions} -> domainEndpointOptions) (\s@CreateDomain' {} a -> s {domainEndpointOptions = a} :: CreateDomain)

-- | DEPRECATED. Container for the parameters required to configure automated
-- snapshots of domain indexes.
createDomain_snapshotOptions :: Lens.Lens' CreateDomain (Prelude.Maybe SnapshotOptions)
createDomain_snapshotOptions = Lens.lens (\CreateDomain' {snapshotOptions} -> snapshotOptions) (\s@CreateDomain' {} a -> s {snapshotOptions = a} :: CreateDomain)

-- | Key-value pairs to configure slow log publishing.
createDomain_logPublishingOptions :: Lens.Lens' CreateDomain (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
createDomain_logPublishingOptions = Lens.lens (\CreateDomain' {logPublishingOptions} -> logPublishingOptions) (\s@CreateDomain' {} a -> s {logPublishingOptions = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the OpenSearch Service domain. For example,
-- @OpenSearch_1.0@ or @Elasticsearch_7.9@. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>.
createDomain_engineVersion :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_engineVersion = Lens.lens (\CreateDomain' {engineVersion} -> engineVersion) (\s@CreateDomain' {} a -> s {engineVersion = a} :: CreateDomain)

-- | Name of the OpenSearch Service domain to create. Domain names are unique
-- across the domains owned by an account within an Amazon Web Services
-- Region.
createDomain_domainName :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Data..?> "DomainStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomain where
  hashWithSalt _salt CreateDomain' {..} =
    _salt
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` clusterConfig
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` vPCOptions
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateDomain where
  rnf CreateDomain' {..} =
    Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf clusterConfig
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf vPCOptions
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders CreateDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NodeToNodeEncryptionOptions" Data..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("ClusterConfig" Data..=) Prelude.<$> clusterConfig,
            ("AdvancedOptions" Data..=)
              Prelude.<$> advancedOptions,
            ("TagList" Data..=) Prelude.<$> tagList,
            ("AdvancedSecurityOptions" Data..=)
              Prelude.<$> advancedSecurityOptions,
            ("CognitoOptions" Data..=)
              Prelude.<$> cognitoOptions,
            ("EncryptionAtRestOptions" Data..=)
              Prelude.<$> encryptionAtRestOptions,
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
              Prelude.<$> logPublishingOptions,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath CreateDomain where
  toPath =
    Prelude.const "/2021-01-01/opensearch/domain"

instance Data.ToQuery CreateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CreateDomain@ operation. Contains the status of the
-- newly created domain.
--
-- /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | The status of the newly created domain.
    domainStatus :: Prelude.Maybe DomainStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainStatus', 'createDomainResponse_domainStatus' - The status of the newly created domain.
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainResponse
newCreateDomainResponse pHttpStatus_ =
  CreateDomainResponse'
    { domainStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the newly created domain.
createDomainResponse_domainStatus :: Lens.Lens' CreateDomainResponse (Prelude.Maybe DomainStatus)
createDomainResponse_domainStatus = Lens.lens (\CreateDomainResponse' {domainStatus} -> domainStatus) (\s@CreateDomainResponse' {} a -> s {domainStatus = a} :: CreateDomainResponse)

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Prelude.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

instance Prelude.NFData CreateDomainResponse where
  rnf CreateDomainResponse' {..} =
    Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf httpStatus
