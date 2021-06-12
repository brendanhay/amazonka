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
-- Module      : Network.AWS.ElasticSearch.CreateElasticsearchDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Elasticsearch domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
-- in the /Amazon Elasticsearch Service Developer Guide/.
module Network.AWS.ElasticSearch.CreateElasticsearchDomain
  ( -- * Creating a Request
    CreateElasticsearchDomain (..),
    newCreateElasticsearchDomain,

    -- * Request Lenses
    createElasticsearchDomain_eBSOptions,
    createElasticsearchDomain_snapshotOptions,
    createElasticsearchDomain_elasticsearchClusterConfig,
    createElasticsearchDomain_domainEndpointOptions,
    createElasticsearchDomain_vPCOptions,
    createElasticsearchDomain_autoTuneOptions,
    createElasticsearchDomain_accessPolicies,
    createElasticsearchDomain_encryptionAtRestOptions,
    createElasticsearchDomain_cognitoOptions,
    createElasticsearchDomain_nodeToNodeEncryptionOptions,
    createElasticsearchDomain_elasticsearchVersion,
    createElasticsearchDomain_advancedOptions,
    createElasticsearchDomain_tagList,
    createElasticsearchDomain_advancedSecurityOptions,
    createElasticsearchDomain_logPublishingOptions,
    createElasticsearchDomain_domainName,

    -- * Destructuring the Response
    CreateElasticsearchDomainResponse (..),
    newCreateElasticsearchDomainResponse,

    -- * Response Lenses
    createElasticsearchDomainResponse_domainStatus,
    createElasticsearchDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateElasticsearchDomain' smart constructor.
data CreateElasticsearchDomain = CreateElasticsearchDomain'
  { -- | Options to enable, disable and specify the type and size of EBS storage
    -- volumes.
    eBSOptions :: Core.Maybe EBSOptions,
    -- | Option to set time, in UTC format, of the daily automated snapshot.
    -- Default value is 0 hours.
    snapshotOptions :: Core.Maybe SnapshotOptions,
    -- | Configuration options for an Elasticsearch domain. Specifies the
    -- instance type and number of instances in the domain cluster.
    elasticsearchClusterConfig :: Core.Maybe ElasticsearchClusterConfig,
    -- | Options to specify configuration that will be applied to the domain
    -- endpoint.
    domainEndpointOptions :: Core.Maybe DomainEndpointOptions,
    -- | Options to specify the subnets and security groups for VPC endpoint. For
    -- more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
    -- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
    vPCOptions :: Core.Maybe VPCOptions,
    -- | Specifies Auto-Tune options.
    autoTuneOptions :: Core.Maybe AutoTuneOptionsInput,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Core.Maybe Core.Text,
    -- | Specifies the Encryption At Rest Options.
    encryptionAtRestOptions :: Core.Maybe EncryptionAtRestOptions,
    -- | Options to specify the Cognito user and identity pools for Kibana
    -- authentication. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Core.Maybe CognitoOptions,
    -- | Specifies the NodeToNodeEncryptionOptions.
    nodeToNodeEncryptionOptions :: Core.Maybe NodeToNodeEncryptionOptions,
    -- | String of format X.Y to specify version for the Elasticsearch domain eg.
    -- \"1.5\" or \"2.3\". For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
    -- in the /Amazon Elasticsearch Service Developer Guide/.
    elasticsearchVersion :: Core.Maybe Core.Text,
    -- | Option to allow references to indices in an HTTP request body. Must be
    -- @false@ when configuring access to individual sub-resources. By default,
    -- the value is @true@. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
    -- for more information.
    advancedOptions :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A list of @Tag@ added during domain creation.
    tagList :: Core.Maybe [Tag],
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Core.Maybe AdvancedSecurityOptionsInput,
    -- | Map of @LogType@ and @LogPublishingOption@, each containing options to
    -- publish a given type of Elasticsearch log.
    logPublishingOptions :: Core.Maybe (Core.HashMap LogType LogPublishingOption),
    -- | The name of the Elasticsearch domain that you are creating. Domain names
    -- are unique across the domains owned by an account within an AWS region.
    -- Domain names must start with a lowercase letter and can contain the
    -- following characters: a-z (lowercase), 0-9, and - (hyphen).
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateElasticsearchDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eBSOptions', 'createElasticsearchDomain_eBSOptions' - Options to enable, disable and specify the type and size of EBS storage
-- volumes.
--
-- 'snapshotOptions', 'createElasticsearchDomain_snapshotOptions' - Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
--
-- 'elasticsearchClusterConfig', 'createElasticsearchDomain_elasticsearchClusterConfig' - Configuration options for an Elasticsearch domain. Specifies the
-- instance type and number of instances in the domain cluster.
--
-- 'domainEndpointOptions', 'createElasticsearchDomain_domainEndpointOptions' - Options to specify configuration that will be applied to the domain
-- endpoint.
--
-- 'vPCOptions', 'createElasticsearchDomain_vPCOptions' - Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- 'autoTuneOptions', 'createElasticsearchDomain_autoTuneOptions' - Specifies Auto-Tune options.
--
-- 'accessPolicies', 'createElasticsearchDomain_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'encryptionAtRestOptions', 'createElasticsearchDomain_encryptionAtRestOptions' - Specifies the Encryption At Rest Options.
--
-- 'cognitoOptions', 'createElasticsearchDomain_cognitoOptions' - Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- 'nodeToNodeEncryptionOptions', 'createElasticsearchDomain_nodeToNodeEncryptionOptions' - Specifies the NodeToNodeEncryptionOptions.
--
-- 'elasticsearchVersion', 'createElasticsearchDomain_elasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain eg.
-- \"1.5\" or \"2.3\". For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
-- in the /Amazon Elasticsearch Service Developer Guide/.
--
-- 'advancedOptions', 'createElasticsearchDomain_advancedOptions' - Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
-- for more information.
--
-- 'tagList', 'createElasticsearchDomain_tagList' - A list of @Tag@ added during domain creation.
--
-- 'advancedSecurityOptions', 'createElasticsearchDomain_advancedSecurityOptions' - Specifies advanced security options.
--
-- 'logPublishingOptions', 'createElasticsearchDomain_logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of Elasticsearch log.
--
-- 'domainName', 'createElasticsearchDomain_domainName' - The name of the Elasticsearch domain that you are creating. Domain names
-- are unique across the domains owned by an account within an AWS region.
-- Domain names must start with a lowercase letter and can contain the
-- following characters: a-z (lowercase), 0-9, and - (hyphen).
newCreateElasticsearchDomain ::
  -- | 'domainName'
  Core.Text ->
  CreateElasticsearchDomain
newCreateElasticsearchDomain pDomainName_ =
  CreateElasticsearchDomain'
    { eBSOptions =
        Core.Nothing,
      snapshotOptions = Core.Nothing,
      elasticsearchClusterConfig = Core.Nothing,
      domainEndpointOptions = Core.Nothing,
      vPCOptions = Core.Nothing,
      autoTuneOptions = Core.Nothing,
      accessPolicies = Core.Nothing,
      encryptionAtRestOptions = Core.Nothing,
      cognitoOptions = Core.Nothing,
      nodeToNodeEncryptionOptions = Core.Nothing,
      elasticsearchVersion = Core.Nothing,
      advancedOptions = Core.Nothing,
      tagList = Core.Nothing,
      advancedSecurityOptions = Core.Nothing,
      logPublishingOptions = Core.Nothing,
      domainName = pDomainName_
    }

-- | Options to enable, disable and specify the type and size of EBS storage
-- volumes.
createElasticsearchDomain_eBSOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe EBSOptions)
createElasticsearchDomain_eBSOptions = Lens.lens (\CreateElasticsearchDomain' {eBSOptions} -> eBSOptions) (\s@CreateElasticsearchDomain' {} a -> s {eBSOptions = a} :: CreateElasticsearchDomain)

-- | Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
createElasticsearchDomain_snapshotOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe SnapshotOptions)
createElasticsearchDomain_snapshotOptions = Lens.lens (\CreateElasticsearchDomain' {snapshotOptions} -> snapshotOptions) (\s@CreateElasticsearchDomain' {} a -> s {snapshotOptions = a} :: CreateElasticsearchDomain)

-- | Configuration options for an Elasticsearch domain. Specifies the
-- instance type and number of instances in the domain cluster.
createElasticsearchDomain_elasticsearchClusterConfig :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe ElasticsearchClusterConfig)
createElasticsearchDomain_elasticsearchClusterConfig = Lens.lens (\CreateElasticsearchDomain' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@CreateElasticsearchDomain' {} a -> s {elasticsearchClusterConfig = a} :: CreateElasticsearchDomain)

-- | Options to specify configuration that will be applied to the domain
-- endpoint.
createElasticsearchDomain_domainEndpointOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe DomainEndpointOptions)
createElasticsearchDomain_domainEndpointOptions = Lens.lens (\CreateElasticsearchDomain' {domainEndpointOptions} -> domainEndpointOptions) (\s@CreateElasticsearchDomain' {} a -> s {domainEndpointOptions = a} :: CreateElasticsearchDomain)

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
createElasticsearchDomain_vPCOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe VPCOptions)
createElasticsearchDomain_vPCOptions = Lens.lens (\CreateElasticsearchDomain' {vPCOptions} -> vPCOptions) (\s@CreateElasticsearchDomain' {} a -> s {vPCOptions = a} :: CreateElasticsearchDomain)

-- | Specifies Auto-Tune options.
createElasticsearchDomain_autoTuneOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe AutoTuneOptionsInput)
createElasticsearchDomain_autoTuneOptions = Lens.lens (\CreateElasticsearchDomain' {autoTuneOptions} -> autoTuneOptions) (\s@CreateElasticsearchDomain' {} a -> s {autoTuneOptions = a} :: CreateElasticsearchDomain)

-- | IAM access policy as a JSON-formatted string.
createElasticsearchDomain_accessPolicies :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Core.Text)
createElasticsearchDomain_accessPolicies = Lens.lens (\CreateElasticsearchDomain' {accessPolicies} -> accessPolicies) (\s@CreateElasticsearchDomain' {} a -> s {accessPolicies = a} :: CreateElasticsearchDomain)

-- | Specifies the Encryption At Rest Options.
createElasticsearchDomain_encryptionAtRestOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe EncryptionAtRestOptions)
createElasticsearchDomain_encryptionAtRestOptions = Lens.lens (\CreateElasticsearchDomain' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@CreateElasticsearchDomain' {} a -> s {encryptionAtRestOptions = a} :: CreateElasticsearchDomain)

-- | Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
createElasticsearchDomain_cognitoOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe CognitoOptions)
createElasticsearchDomain_cognitoOptions = Lens.lens (\CreateElasticsearchDomain' {cognitoOptions} -> cognitoOptions) (\s@CreateElasticsearchDomain' {} a -> s {cognitoOptions = a} :: CreateElasticsearchDomain)

-- | Specifies the NodeToNodeEncryptionOptions.
createElasticsearchDomain_nodeToNodeEncryptionOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe NodeToNodeEncryptionOptions)
createElasticsearchDomain_nodeToNodeEncryptionOptions = Lens.lens (\CreateElasticsearchDomain' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@CreateElasticsearchDomain' {} a -> s {nodeToNodeEncryptionOptions = a} :: CreateElasticsearchDomain)

-- | String of format X.Y to specify version for the Elasticsearch domain eg.
-- \"1.5\" or \"2.3\". For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
-- in the /Amazon Elasticsearch Service Developer Guide/.
createElasticsearchDomain_elasticsearchVersion :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Core.Text)
createElasticsearchDomain_elasticsearchVersion = Lens.lens (\CreateElasticsearchDomain' {elasticsearchVersion} -> elasticsearchVersion) (\s@CreateElasticsearchDomain' {} a -> s {elasticsearchVersion = a} :: CreateElasticsearchDomain)

-- | Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
-- for more information.
createElasticsearchDomain_advancedOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe (Core.HashMap Core.Text Core.Text))
createElasticsearchDomain_advancedOptions = Lens.lens (\CreateElasticsearchDomain' {advancedOptions} -> advancedOptions) (\s@CreateElasticsearchDomain' {} a -> s {advancedOptions = a} :: CreateElasticsearchDomain) Core.. Lens.mapping Lens._Coerce

-- | A list of @Tag@ added during domain creation.
createElasticsearchDomain_tagList :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe [Tag])
createElasticsearchDomain_tagList = Lens.lens (\CreateElasticsearchDomain' {tagList} -> tagList) (\s@CreateElasticsearchDomain' {} a -> s {tagList = a} :: CreateElasticsearchDomain) Core.. Lens.mapping Lens._Coerce

-- | Specifies advanced security options.
createElasticsearchDomain_advancedSecurityOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe AdvancedSecurityOptionsInput)
createElasticsearchDomain_advancedSecurityOptions = Lens.lens (\CreateElasticsearchDomain' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@CreateElasticsearchDomain' {} a -> s {advancedSecurityOptions = a} :: CreateElasticsearchDomain)

-- | Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of Elasticsearch log.
createElasticsearchDomain_logPublishingOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe (Core.HashMap LogType LogPublishingOption))
createElasticsearchDomain_logPublishingOptions = Lens.lens (\CreateElasticsearchDomain' {logPublishingOptions} -> logPublishingOptions) (\s@CreateElasticsearchDomain' {} a -> s {logPublishingOptions = a} :: CreateElasticsearchDomain) Core.. Lens.mapping Lens._Coerce

-- | The name of the Elasticsearch domain that you are creating. Domain names
-- are unique across the domains owned by an account within an AWS region.
-- Domain names must start with a lowercase letter and can contain the
-- following characters: a-z (lowercase), 0-9, and - (hyphen).
createElasticsearchDomain_domainName :: Lens.Lens' CreateElasticsearchDomain Core.Text
createElasticsearchDomain_domainName = Lens.lens (\CreateElasticsearchDomain' {domainName} -> domainName) (\s@CreateElasticsearchDomain' {} a -> s {domainName = a} :: CreateElasticsearchDomain)

instance Core.AWSRequest CreateElasticsearchDomain where
  type
    AWSResponse CreateElasticsearchDomain =
      CreateElasticsearchDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateElasticsearchDomainResponse'
            Core.<$> (x Core..?> "DomainStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateElasticsearchDomain

instance Core.NFData CreateElasticsearchDomain

instance Core.ToHeaders CreateElasticsearchDomain where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateElasticsearchDomain where
  toJSON CreateElasticsearchDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EBSOptions" Core..=) Core.<$> eBSOptions,
            ("SnapshotOptions" Core..=) Core.<$> snapshotOptions,
            ("ElasticsearchClusterConfig" Core..=)
              Core.<$> elasticsearchClusterConfig,
            ("DomainEndpointOptions" Core..=)
              Core.<$> domainEndpointOptions,
            ("VPCOptions" Core..=) Core.<$> vPCOptions,
            ("AutoTuneOptions" Core..=) Core.<$> autoTuneOptions,
            ("AccessPolicies" Core..=) Core.<$> accessPolicies,
            ("EncryptionAtRestOptions" Core..=)
              Core.<$> encryptionAtRestOptions,
            ("CognitoOptions" Core..=) Core.<$> cognitoOptions,
            ("NodeToNodeEncryptionOptions" Core..=)
              Core.<$> nodeToNodeEncryptionOptions,
            ("ElasticsearchVersion" Core..=)
              Core.<$> elasticsearchVersion,
            ("AdvancedOptions" Core..=) Core.<$> advancedOptions,
            ("TagList" Core..=) Core.<$> tagList,
            ("AdvancedSecurityOptions" Core..=)
              Core.<$> advancedSecurityOptions,
            ("LogPublishingOptions" Core..=)
              Core.<$> logPublishingOptions,
            Core.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath CreateElasticsearchDomain where
  toPath = Core.const "/2015-01-01/es/domain"

instance Core.ToQuery CreateElasticsearchDomain where
  toQuery = Core.const Core.mempty

-- | The result of a @CreateElasticsearchDomain@ operation. Contains the
-- status of the newly created Elasticsearch domain.
--
-- /See:/ 'newCreateElasticsearchDomainResponse' smart constructor.
data CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse'
  { -- | The status of the newly created Elasticsearch domain.
    domainStatus :: Core.Maybe ElasticsearchDomainStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateElasticsearchDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainStatus', 'createElasticsearchDomainResponse_domainStatus' - The status of the newly created Elasticsearch domain.
--
-- 'httpStatus', 'createElasticsearchDomainResponse_httpStatus' - The response's http status code.
newCreateElasticsearchDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateElasticsearchDomainResponse
newCreateElasticsearchDomainResponse pHttpStatus_ =
  CreateElasticsearchDomainResponse'
    { domainStatus =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the newly created Elasticsearch domain.
createElasticsearchDomainResponse_domainStatus :: Lens.Lens' CreateElasticsearchDomainResponse (Core.Maybe ElasticsearchDomainStatus)
createElasticsearchDomainResponse_domainStatus = Lens.lens (\CreateElasticsearchDomainResponse' {domainStatus} -> domainStatus) (\s@CreateElasticsearchDomainResponse' {} a -> s {domainStatus = a} :: CreateElasticsearchDomainResponse)

-- | The response's http status code.
createElasticsearchDomainResponse_httpStatus :: Lens.Lens' CreateElasticsearchDomainResponse Core.Int
createElasticsearchDomainResponse_httpStatus = Lens.lens (\CreateElasticsearchDomainResponse' {httpStatus} -> httpStatus) (\s@CreateElasticsearchDomainResponse' {} a -> s {httpStatus = a} :: CreateElasticsearchDomainResponse)

instance
  Core.NFData
    CreateElasticsearchDomainResponse
