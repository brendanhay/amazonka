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
-- Creates a new Amazon OpenSearch Service domain. For more information,
-- see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html Creating and managing Amazon OpenSearch Service domains>
-- in the /Amazon OpenSearch Service Developer Guide/.
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
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | Node-to-node encryption options.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | Configuration options for a domain. Specifies the instance type and
    -- number of instances in the domain.
    clusterConfig :: Prelude.Maybe ClusterConfig,
    -- | Option to allow references to indices in an HTTP request body. Must be
    -- @false@ when configuring access to individual sub-resources. By default,
    -- the value is @true@. See
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>
    -- for more information.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of @Tag@ added during domain creation.
    tagList :: Prelude.Maybe [Tag],
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsInput,
    -- | Options to specify the Cognito user and identity pools for OpenSearch
    -- Dashboards authentication. For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Options for encryption of data at rest.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | Options to enable, disable, and specify the type and size of EBS storage
    -- volumes.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Options to specify the subnets and security groups for a VPC endpoint.
    -- For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
    -- .
    vPCOptions :: Prelude.Maybe VPCOptions,
    -- | Specifies Auto-Tune options.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsInput,
    -- | Options to specify configurations that will be applied to the domain
    -- endpoint.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | Option to set time, in UTC format, of the daily automated snapshot.
    -- Default value is 0 hours.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | Map of @LogType@ and @LogPublishingOption@, each containing options to
    -- publish a given type of OpenSearch log.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
    -- engine version for the Amazon OpenSearch Service domain. For example,
    -- \"OpenSearch_1.0\" or \"Elasticsearch_7.9\". For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>
    -- .
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon OpenSearch Service domain you\'re creating.
    -- Domain names are unique across the domains owned by an account within an
    -- AWS region. Domain names must start with a lowercase letter and can
    -- contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
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
-- 'nodeToNodeEncryptionOptions', 'createDomain_nodeToNodeEncryptionOptions' - Node-to-node encryption options.
--
-- 'clusterConfig', 'createDomain_clusterConfig' - Configuration options for a domain. Specifies the instance type and
-- number of instances in the domain.
--
-- 'advancedOptions', 'createDomain_advancedOptions' - Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>
-- for more information.
--
-- 'tagList', 'createDomain_tagList' - A list of @Tag@ added during domain creation.
--
-- 'advancedSecurityOptions', 'createDomain_advancedSecurityOptions' - Specifies advanced security options.
--
-- 'cognitoOptions', 'createDomain_cognitoOptions' - Options to specify the Cognito user and identity pools for OpenSearch
-- Dashboards authentication. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- 'encryptionAtRestOptions', 'createDomain_encryptionAtRestOptions' - Options for encryption of data at rest.
--
-- 'eBSOptions', 'createDomain_eBSOptions' - Options to enable, disable, and specify the type and size of EBS storage
-- volumes.
--
-- 'accessPolicies', 'createDomain_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'vPCOptions', 'createDomain_vPCOptions' - Options to specify the subnets and security groups for a VPC endpoint.
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
-- .
--
-- 'autoTuneOptions', 'createDomain_autoTuneOptions' - Specifies Auto-Tune options.
--
-- 'domainEndpointOptions', 'createDomain_domainEndpointOptions' - Options to specify configurations that will be applied to the domain
-- endpoint.
--
-- 'snapshotOptions', 'createDomain_snapshotOptions' - Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
--
-- 'logPublishingOptions', 'createDomain_logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of OpenSearch log.
--
-- 'engineVersion', 'createDomain_engineVersion' - String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the Amazon OpenSearch Service domain. For example,
-- \"OpenSearch_1.0\" or \"Elasticsearch_7.9\". For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>
-- .
--
-- 'domainName', 'createDomain_domainName' - The name of the Amazon OpenSearch Service domain you\'re creating.
-- Domain names are unique across the domains owned by an account within an
-- AWS region. Domain names must start with a lowercase letter and can
-- contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
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

-- | Node-to-node encryption options.
createDomain_nodeToNodeEncryptionOptions :: Lens.Lens' CreateDomain (Prelude.Maybe NodeToNodeEncryptionOptions)
createDomain_nodeToNodeEncryptionOptions = Lens.lens (\CreateDomain' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@CreateDomain' {} a -> s {nodeToNodeEncryptionOptions = a} :: CreateDomain)

-- | Configuration options for a domain. Specifies the instance type and
-- number of instances in the domain.
createDomain_clusterConfig :: Lens.Lens' CreateDomain (Prelude.Maybe ClusterConfig)
createDomain_clusterConfig = Lens.lens (\CreateDomain' {clusterConfig} -> clusterConfig) (\s@CreateDomain' {} a -> s {clusterConfig = a} :: CreateDomain)

-- | Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>
-- for more information.
createDomain_advancedOptions :: Lens.Lens' CreateDomain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomain_advancedOptions = Lens.lens (\CreateDomain' {advancedOptions} -> advancedOptions) (\s@CreateDomain' {} a -> s {advancedOptions = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | A list of @Tag@ added during domain creation.
createDomain_tagList :: Lens.Lens' CreateDomain (Prelude.Maybe [Tag])
createDomain_tagList = Lens.lens (\CreateDomain' {tagList} -> tagList) (\s@CreateDomain' {} a -> s {tagList = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | Specifies advanced security options.
createDomain_advancedSecurityOptions :: Lens.Lens' CreateDomain (Prelude.Maybe AdvancedSecurityOptionsInput)
createDomain_advancedSecurityOptions = Lens.lens (\CreateDomain' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@CreateDomain' {} a -> s {advancedSecurityOptions = a} :: CreateDomain)

-- | Options to specify the Cognito user and identity pools for OpenSearch
-- Dashboards authentication. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
createDomain_cognitoOptions :: Lens.Lens' CreateDomain (Prelude.Maybe CognitoOptions)
createDomain_cognitoOptions = Lens.lens (\CreateDomain' {cognitoOptions} -> cognitoOptions) (\s@CreateDomain' {} a -> s {cognitoOptions = a} :: CreateDomain)

-- | Options for encryption of data at rest.
createDomain_encryptionAtRestOptions :: Lens.Lens' CreateDomain (Prelude.Maybe EncryptionAtRestOptions)
createDomain_encryptionAtRestOptions = Lens.lens (\CreateDomain' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@CreateDomain' {} a -> s {encryptionAtRestOptions = a} :: CreateDomain)

-- | Options to enable, disable, and specify the type and size of EBS storage
-- volumes.
createDomain_eBSOptions :: Lens.Lens' CreateDomain (Prelude.Maybe EBSOptions)
createDomain_eBSOptions = Lens.lens (\CreateDomain' {eBSOptions} -> eBSOptions) (\s@CreateDomain' {} a -> s {eBSOptions = a} :: CreateDomain)

-- | IAM access policy as a JSON-formatted string.
createDomain_accessPolicies :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_accessPolicies = Lens.lens (\CreateDomain' {accessPolicies} -> accessPolicies) (\s@CreateDomain' {} a -> s {accessPolicies = a} :: CreateDomain)

-- | Options to specify the subnets and security groups for a VPC endpoint.
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
-- .
createDomain_vPCOptions :: Lens.Lens' CreateDomain (Prelude.Maybe VPCOptions)
createDomain_vPCOptions = Lens.lens (\CreateDomain' {vPCOptions} -> vPCOptions) (\s@CreateDomain' {} a -> s {vPCOptions = a} :: CreateDomain)

-- | Specifies Auto-Tune options.
createDomain_autoTuneOptions :: Lens.Lens' CreateDomain (Prelude.Maybe AutoTuneOptionsInput)
createDomain_autoTuneOptions = Lens.lens (\CreateDomain' {autoTuneOptions} -> autoTuneOptions) (\s@CreateDomain' {} a -> s {autoTuneOptions = a} :: CreateDomain)

-- | Options to specify configurations that will be applied to the domain
-- endpoint.
createDomain_domainEndpointOptions :: Lens.Lens' CreateDomain (Prelude.Maybe DomainEndpointOptions)
createDomain_domainEndpointOptions = Lens.lens (\CreateDomain' {domainEndpointOptions} -> domainEndpointOptions) (\s@CreateDomain' {} a -> s {domainEndpointOptions = a} :: CreateDomain)

-- | Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
createDomain_snapshotOptions :: Lens.Lens' CreateDomain (Prelude.Maybe SnapshotOptions)
createDomain_snapshotOptions = Lens.lens (\CreateDomain' {snapshotOptions} -> snapshotOptions) (\s@CreateDomain' {} a -> s {snapshotOptions = a} :: CreateDomain)

-- | Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of OpenSearch log.
createDomain_logPublishingOptions :: Lens.Lens' CreateDomain (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
createDomain_logPublishingOptions = Lens.lens (\CreateDomain' {logPublishingOptions} -> logPublishingOptions) (\s@CreateDomain' {} a -> s {logPublishingOptions = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the Amazon OpenSearch Service domain. For example,
-- \"OpenSearch_1.0\" or \"Elasticsearch_7.9\". For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>
-- .
createDomain_engineVersion :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_engineVersion = Lens.lens (\CreateDomain' {engineVersion} -> engineVersion) (\s@CreateDomain' {} a -> s {engineVersion = a} :: CreateDomain)

-- | The name of the Amazon OpenSearch Service domain you\'re creating.
-- Domain names are unique across the domains owned by an account within an
-- AWS region. Domain names must start with a lowercase letter and can
-- contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
createDomain_domainName :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  service _ = defaultService
  request srv = Request.postJSON srv
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Core..?> "DomainStatus")
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

instance Core.ToHeaders CreateDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NodeToNodeEncryptionOptions" Core..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("ClusterConfig" Core..=) Prelude.<$> clusterConfig,
            ("AdvancedOptions" Core..=)
              Prelude.<$> advancedOptions,
            ("TagList" Core..=) Prelude.<$> tagList,
            ("AdvancedSecurityOptions" Core..=)
              Prelude.<$> advancedSecurityOptions,
            ("CognitoOptions" Core..=)
              Prelude.<$> cognitoOptions,
            ("EncryptionAtRestOptions" Core..=)
              Prelude.<$> encryptionAtRestOptions,
            ("EBSOptions" Core..=) Prelude.<$> eBSOptions,
            ("AccessPolicies" Core..=)
              Prelude.<$> accessPolicies,
            ("VPCOptions" Core..=) Prelude.<$> vPCOptions,
            ("AutoTuneOptions" Core..=)
              Prelude.<$> autoTuneOptions,
            ("DomainEndpointOptions" Core..=)
              Prelude.<$> domainEndpointOptions,
            ("SnapshotOptions" Core..=)
              Prelude.<$> snapshotOptions,
            ("LogPublishingOptions" Core..=)
              Prelude.<$> logPublishingOptions,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath CreateDomain where
  toPath =
    Prelude.const "/2021-01-01/opensearch/domain"

instance Core.ToQuery CreateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CreateDomain@ operation. Contains the status of the
-- newly created Amazon OpenSearch Service domain.
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
