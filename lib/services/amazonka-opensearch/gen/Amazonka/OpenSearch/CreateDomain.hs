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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createDomain_eBSOptions,
    createDomain_engineVersion,
    createDomain_nodeToNodeEncryptionOptions,
    createDomain_accessPolicies,
    createDomain_autoTuneOptions,
    createDomain_logPublishingOptions,
    createDomain_clusterConfig,
    createDomain_advancedSecurityOptions,
    createDomain_tagList,
    createDomain_snapshotOptions,
    createDomain_cognitoOptions,
    createDomain_encryptionAtRestOptions,
    createDomain_vPCOptions,
    createDomain_domainEndpointOptions,
    createDomain_advancedOptions,
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
  { -- | Options to enable, disable, and specify the type and size of EBS storage
    -- volumes.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
    -- engine version for the Amazon OpenSearch Service domain. For example,
    -- \"OpenSearch_1.0\" or \"Elasticsearch_7.9\". For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>
    -- .
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Node-to-node encryption options.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Specifies Auto-Tune options.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsInput,
    -- | Map of @LogType@ and @LogPublishingOption@, each containing options to
    -- publish a given type of OpenSearch log.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | Configuration options for a domain. Specifies the instance type and
    -- number of instances in the domain.
    clusterConfig :: Prelude.Maybe ClusterConfig,
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsInput,
    -- | A list of @Tag@ added during domain creation.
    tagList :: Prelude.Maybe [Tag],
    -- | Option to set time, in UTC format, of the daily automated snapshot.
    -- Default value is 0 hours.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | Options to specify the Cognito user and identity pools for OpenSearch
    -- Dashboards authentication. For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Options for encryption of data at rest.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | Options to specify the subnets and security groups for a VPC endpoint.
    -- For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
    -- .
    vPCOptions :: Prelude.Maybe VPCOptions,
    -- | Options to specify configurations that will be applied to the domain
    -- endpoint.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | Option to allow references to indices in an HTTP request body. Must be
    -- @false@ when configuring access to individual sub-resources. By default,
    -- the value is @true@. See
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>
    -- for more information.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'eBSOptions', 'createDomain_eBSOptions' - Options to enable, disable, and specify the type and size of EBS storage
-- volumes.
--
-- 'engineVersion', 'createDomain_engineVersion' - String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the Amazon OpenSearch Service domain. For example,
-- \"OpenSearch_1.0\" or \"Elasticsearch_7.9\". For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>
-- .
--
-- 'nodeToNodeEncryptionOptions', 'createDomain_nodeToNodeEncryptionOptions' - Node-to-node encryption options.
--
-- 'accessPolicies', 'createDomain_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'autoTuneOptions', 'createDomain_autoTuneOptions' - Specifies Auto-Tune options.
--
-- 'logPublishingOptions', 'createDomain_logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of OpenSearch log.
--
-- 'clusterConfig', 'createDomain_clusterConfig' - Configuration options for a domain. Specifies the instance type and
-- number of instances in the domain.
--
-- 'advancedSecurityOptions', 'createDomain_advancedSecurityOptions' - Specifies advanced security options.
--
-- 'tagList', 'createDomain_tagList' - A list of @Tag@ added during domain creation.
--
-- 'snapshotOptions', 'createDomain_snapshotOptions' - Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
--
-- 'cognitoOptions', 'createDomain_cognitoOptions' - Options to specify the Cognito user and identity pools for OpenSearch
-- Dashboards authentication. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- 'encryptionAtRestOptions', 'createDomain_encryptionAtRestOptions' - Options for encryption of data at rest.
--
-- 'vPCOptions', 'createDomain_vPCOptions' - Options to specify the subnets and security groups for a VPC endpoint.
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
-- .
--
-- 'domainEndpointOptions', 'createDomain_domainEndpointOptions' - Options to specify configurations that will be applied to the domain
-- endpoint.
--
-- 'advancedOptions', 'createDomain_advancedOptions' - Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>
-- for more information.
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
    { eBSOptions = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      nodeToNodeEncryptionOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      clusterConfig = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      tagList = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Options to enable, disable, and specify the type and size of EBS storage
-- volumes.
createDomain_eBSOptions :: Lens.Lens' CreateDomain (Prelude.Maybe EBSOptions)
createDomain_eBSOptions = Lens.lens (\CreateDomain' {eBSOptions} -> eBSOptions) (\s@CreateDomain' {} a -> s {eBSOptions = a} :: CreateDomain)

-- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the Amazon OpenSearch Service domain. For example,
-- \"OpenSearch_1.0\" or \"Elasticsearch_7.9\". For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomains Creating and managing Amazon OpenSearch Service domains>
-- .
createDomain_engineVersion :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_engineVersion = Lens.lens (\CreateDomain' {engineVersion} -> engineVersion) (\s@CreateDomain' {} a -> s {engineVersion = a} :: CreateDomain)

-- | Node-to-node encryption options.
createDomain_nodeToNodeEncryptionOptions :: Lens.Lens' CreateDomain (Prelude.Maybe NodeToNodeEncryptionOptions)
createDomain_nodeToNodeEncryptionOptions = Lens.lens (\CreateDomain' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@CreateDomain' {} a -> s {nodeToNodeEncryptionOptions = a} :: CreateDomain)

-- | IAM access policy as a JSON-formatted string.
createDomain_accessPolicies :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_accessPolicies = Lens.lens (\CreateDomain' {accessPolicies} -> accessPolicies) (\s@CreateDomain' {} a -> s {accessPolicies = a} :: CreateDomain)

-- | Specifies Auto-Tune options.
createDomain_autoTuneOptions :: Lens.Lens' CreateDomain (Prelude.Maybe AutoTuneOptionsInput)
createDomain_autoTuneOptions = Lens.lens (\CreateDomain' {autoTuneOptions} -> autoTuneOptions) (\s@CreateDomain' {} a -> s {autoTuneOptions = a} :: CreateDomain)

-- | Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of OpenSearch log.
createDomain_logPublishingOptions :: Lens.Lens' CreateDomain (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
createDomain_logPublishingOptions = Lens.lens (\CreateDomain' {logPublishingOptions} -> logPublishingOptions) (\s@CreateDomain' {} a -> s {logPublishingOptions = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | Configuration options for a domain. Specifies the instance type and
-- number of instances in the domain.
createDomain_clusterConfig :: Lens.Lens' CreateDomain (Prelude.Maybe ClusterConfig)
createDomain_clusterConfig = Lens.lens (\CreateDomain' {clusterConfig} -> clusterConfig) (\s@CreateDomain' {} a -> s {clusterConfig = a} :: CreateDomain)

-- | Specifies advanced security options.
createDomain_advancedSecurityOptions :: Lens.Lens' CreateDomain (Prelude.Maybe AdvancedSecurityOptionsInput)
createDomain_advancedSecurityOptions = Lens.lens (\CreateDomain' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@CreateDomain' {} a -> s {advancedSecurityOptions = a} :: CreateDomain)

-- | A list of @Tag@ added during domain creation.
createDomain_tagList :: Lens.Lens' CreateDomain (Prelude.Maybe [Tag])
createDomain_tagList = Lens.lens (\CreateDomain' {tagList} -> tagList) (\s@CreateDomain' {} a -> s {tagList = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
createDomain_snapshotOptions :: Lens.Lens' CreateDomain (Prelude.Maybe SnapshotOptions)
createDomain_snapshotOptions = Lens.lens (\CreateDomain' {snapshotOptions} -> snapshotOptions) (\s@CreateDomain' {} a -> s {snapshotOptions = a} :: CreateDomain)

-- | Options to specify the Cognito user and identity pools for OpenSearch
-- Dashboards authentication. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
createDomain_cognitoOptions :: Lens.Lens' CreateDomain (Prelude.Maybe CognitoOptions)
createDomain_cognitoOptions = Lens.lens (\CreateDomain' {cognitoOptions} -> cognitoOptions) (\s@CreateDomain' {} a -> s {cognitoOptions = a} :: CreateDomain)

-- | Options for encryption of data at rest.
createDomain_encryptionAtRestOptions :: Lens.Lens' CreateDomain (Prelude.Maybe EncryptionAtRestOptions)
createDomain_encryptionAtRestOptions = Lens.lens (\CreateDomain' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@CreateDomain' {} a -> s {encryptionAtRestOptions = a} :: CreateDomain)

-- | Options to specify the subnets and security groups for a VPC endpoint.
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>
-- .
createDomain_vPCOptions :: Lens.Lens' CreateDomain (Prelude.Maybe VPCOptions)
createDomain_vPCOptions = Lens.lens (\CreateDomain' {vPCOptions} -> vPCOptions) (\s@CreateDomain' {} a -> s {vPCOptions = a} :: CreateDomain)

-- | Options to specify configurations that will be applied to the domain
-- endpoint.
createDomain_domainEndpointOptions :: Lens.Lens' CreateDomain (Prelude.Maybe DomainEndpointOptions)
createDomain_domainEndpointOptions = Lens.lens (\CreateDomain' {domainEndpointOptions} -> domainEndpointOptions) (\s@CreateDomain' {} a -> s {domainEndpointOptions = a} :: CreateDomain)

-- | Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>
-- for more information.
createDomain_advancedOptions :: Lens.Lens' CreateDomain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomain_advancedOptions = Lens.lens (\CreateDomain' {advancedOptions} -> advancedOptions) (\s@CreateDomain' {} a -> s {advancedOptions = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon OpenSearch Service domain you\'re creating.
-- Domain names are unique across the domains owned by an account within an
-- AWS region. Domain names must start with a lowercase letter and can
-- contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
createDomain_domainName :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Core..?> "DomainStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomain

instance Prelude.NFData CreateDomain

instance Core.ToHeaders CreateDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EBSOptions" Core..=) Prelude.<$> eBSOptions,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
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
            ("TagList" Core..=) Prelude.<$> tagList,
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
              Prelude.<$> advancedOptions,
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

instance Prelude.NFData CreateDomainResponse
