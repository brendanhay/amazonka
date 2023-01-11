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
-- Module      : Amazonka.ElasticSearch.CreateElasticsearchDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Elasticsearch domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
-- in the /Amazon Elasticsearch Service Developer Guide/.
module Amazonka.ElasticSearch.CreateElasticsearchDomain
  ( -- * Creating a Request
    CreateElasticsearchDomain (..),
    newCreateElasticsearchDomain,

    -- * Request Lenses
    createElasticsearchDomain_accessPolicies,
    createElasticsearchDomain_advancedOptions,
    createElasticsearchDomain_advancedSecurityOptions,
    createElasticsearchDomain_autoTuneOptions,
    createElasticsearchDomain_cognitoOptions,
    createElasticsearchDomain_domainEndpointOptions,
    createElasticsearchDomain_eBSOptions,
    createElasticsearchDomain_elasticsearchClusterConfig,
    createElasticsearchDomain_elasticsearchVersion,
    createElasticsearchDomain_encryptionAtRestOptions,
    createElasticsearchDomain_logPublishingOptions,
    createElasticsearchDomain_nodeToNodeEncryptionOptions,
    createElasticsearchDomain_snapshotOptions,
    createElasticsearchDomain_tagList,
    createElasticsearchDomain_vPCOptions,
    createElasticsearchDomain_domainName,

    -- * Destructuring the Response
    CreateElasticsearchDomainResponse (..),
    newCreateElasticsearchDomainResponse,

    -- * Response Lenses
    createElasticsearchDomainResponse_domainStatus,
    createElasticsearchDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateElasticsearchDomain' smart constructor.
data CreateElasticsearchDomain = CreateElasticsearchDomain'
  { -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Option to allow references to indices in an HTTP request body. Must be
    -- @false@ when configuring access to individual sub-resources. By default,
    -- the value is @true@. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
    -- for more information.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsInput,
    -- | Specifies Auto-Tune options.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsInput,
    -- | Options to specify the Cognito user and identity pools for Kibana
    -- authentication. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Options to specify configuration that will be applied to the domain
    -- endpoint.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | Options to enable, disable and specify the type and size of EBS storage
    -- volumes.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | Configuration options for an Elasticsearch domain. Specifies the
    -- instance type and number of instances in the domain cluster.
    elasticsearchClusterConfig :: Prelude.Maybe ElasticsearchClusterConfig,
    -- | String of format X.Y to specify version for the Elasticsearch domain eg.
    -- \"1.5\" or \"2.3\". For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
    -- in the /Amazon Elasticsearch Service Developer Guide/.
    elasticsearchVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Encryption At Rest Options.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | Map of @LogType@ and @LogPublishingOption@, each containing options to
    -- publish a given type of Elasticsearch log.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | Specifies the NodeToNodeEncryptionOptions.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | Option to set time, in UTC format, of the daily automated snapshot.
    -- Default value is 0 hours.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | A list of @Tag@ added during domain creation.
    tagList :: Prelude.Maybe [Tag],
    -- | Options to specify the subnets and security groups for VPC endpoint. For
    -- more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
    -- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
    vPCOptions :: Prelude.Maybe VPCOptions,
    -- | The name of the Elasticsearch domain that you are creating. Domain names
    -- are unique across the domains owned by an account within an AWS region.
    -- Domain names must start with a lowercase letter and can contain the
    -- following characters: a-z (lowercase), 0-9, and - (hyphen).
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateElasticsearchDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicies', 'createElasticsearchDomain_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'advancedOptions', 'createElasticsearchDomain_advancedOptions' - Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
-- for more information.
--
-- 'advancedSecurityOptions', 'createElasticsearchDomain_advancedSecurityOptions' - Specifies advanced security options.
--
-- 'autoTuneOptions', 'createElasticsearchDomain_autoTuneOptions' - Specifies Auto-Tune options.
--
-- 'cognitoOptions', 'createElasticsearchDomain_cognitoOptions' - Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- 'domainEndpointOptions', 'createElasticsearchDomain_domainEndpointOptions' - Options to specify configuration that will be applied to the domain
-- endpoint.
--
-- 'eBSOptions', 'createElasticsearchDomain_eBSOptions' - Options to enable, disable and specify the type and size of EBS storage
-- volumes.
--
-- 'elasticsearchClusterConfig', 'createElasticsearchDomain_elasticsearchClusterConfig' - Configuration options for an Elasticsearch domain. Specifies the
-- instance type and number of instances in the domain cluster.
--
-- 'elasticsearchVersion', 'createElasticsearchDomain_elasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain eg.
-- \"1.5\" or \"2.3\". For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
-- in the /Amazon Elasticsearch Service Developer Guide/.
--
-- 'encryptionAtRestOptions', 'createElasticsearchDomain_encryptionAtRestOptions' - Specifies the Encryption At Rest Options.
--
-- 'logPublishingOptions', 'createElasticsearchDomain_logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of Elasticsearch log.
--
-- 'nodeToNodeEncryptionOptions', 'createElasticsearchDomain_nodeToNodeEncryptionOptions' - Specifies the NodeToNodeEncryptionOptions.
--
-- 'snapshotOptions', 'createElasticsearchDomain_snapshotOptions' - Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
--
-- 'tagList', 'createElasticsearchDomain_tagList' - A list of @Tag@ added during domain creation.
--
-- 'vPCOptions', 'createElasticsearchDomain_vPCOptions' - Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- 'domainName', 'createElasticsearchDomain_domainName' - The name of the Elasticsearch domain that you are creating. Domain names
-- are unique across the domains owned by an account within an AWS region.
-- Domain names must start with a lowercase letter and can contain the
-- following characters: a-z (lowercase), 0-9, and - (hyphen).
newCreateElasticsearchDomain ::
  -- | 'domainName'
  Prelude.Text ->
  CreateElasticsearchDomain
newCreateElasticsearchDomain pDomainName_ =
  CreateElasticsearchDomain'
    { accessPolicies =
        Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      eBSOptions = Prelude.Nothing,
      elasticsearchClusterConfig = Prelude.Nothing,
      elasticsearchVersion = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      nodeToNodeEncryptionOptions = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      tagList = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | IAM access policy as a JSON-formatted string.
createElasticsearchDomain_accessPolicies :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe Prelude.Text)
createElasticsearchDomain_accessPolicies = Lens.lens (\CreateElasticsearchDomain' {accessPolicies} -> accessPolicies) (\s@CreateElasticsearchDomain' {} a -> s {accessPolicies = a} :: CreateElasticsearchDomain)

-- | Option to allow references to indices in an HTTP request body. Must be
-- @false@ when configuring access to individual sub-resources. By default,
-- the value is @true@. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options>
-- for more information.
createElasticsearchDomain_advancedOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createElasticsearchDomain_advancedOptions = Lens.lens (\CreateElasticsearchDomain' {advancedOptions} -> advancedOptions) (\s@CreateElasticsearchDomain' {} a -> s {advancedOptions = a} :: CreateElasticsearchDomain) Prelude.. Lens.mapping Lens.coerced

-- | Specifies advanced security options.
createElasticsearchDomain_advancedSecurityOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe AdvancedSecurityOptionsInput)
createElasticsearchDomain_advancedSecurityOptions = Lens.lens (\CreateElasticsearchDomain' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@CreateElasticsearchDomain' {} a -> s {advancedSecurityOptions = a} :: CreateElasticsearchDomain)

-- | Specifies Auto-Tune options.
createElasticsearchDomain_autoTuneOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe AutoTuneOptionsInput)
createElasticsearchDomain_autoTuneOptions = Lens.lens (\CreateElasticsearchDomain' {autoTuneOptions} -> autoTuneOptions) (\s@CreateElasticsearchDomain' {} a -> s {autoTuneOptions = a} :: CreateElasticsearchDomain)

-- | Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
createElasticsearchDomain_cognitoOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe CognitoOptions)
createElasticsearchDomain_cognitoOptions = Lens.lens (\CreateElasticsearchDomain' {cognitoOptions} -> cognitoOptions) (\s@CreateElasticsearchDomain' {} a -> s {cognitoOptions = a} :: CreateElasticsearchDomain)

-- | Options to specify configuration that will be applied to the domain
-- endpoint.
createElasticsearchDomain_domainEndpointOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe DomainEndpointOptions)
createElasticsearchDomain_domainEndpointOptions = Lens.lens (\CreateElasticsearchDomain' {domainEndpointOptions} -> domainEndpointOptions) (\s@CreateElasticsearchDomain' {} a -> s {domainEndpointOptions = a} :: CreateElasticsearchDomain)

-- | Options to enable, disable and specify the type and size of EBS storage
-- volumes.
createElasticsearchDomain_eBSOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe EBSOptions)
createElasticsearchDomain_eBSOptions = Lens.lens (\CreateElasticsearchDomain' {eBSOptions} -> eBSOptions) (\s@CreateElasticsearchDomain' {} a -> s {eBSOptions = a} :: CreateElasticsearchDomain)

-- | Configuration options for an Elasticsearch domain. Specifies the
-- instance type and number of instances in the domain cluster.
createElasticsearchDomain_elasticsearchClusterConfig :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe ElasticsearchClusterConfig)
createElasticsearchDomain_elasticsearchClusterConfig = Lens.lens (\CreateElasticsearchDomain' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@CreateElasticsearchDomain' {} a -> s {elasticsearchClusterConfig = a} :: CreateElasticsearchDomain)

-- | String of format X.Y to specify version for the Elasticsearch domain eg.
-- \"1.5\" or \"2.3\". For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains>
-- in the /Amazon Elasticsearch Service Developer Guide/.
createElasticsearchDomain_elasticsearchVersion :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe Prelude.Text)
createElasticsearchDomain_elasticsearchVersion = Lens.lens (\CreateElasticsearchDomain' {elasticsearchVersion} -> elasticsearchVersion) (\s@CreateElasticsearchDomain' {} a -> s {elasticsearchVersion = a} :: CreateElasticsearchDomain)

-- | Specifies the Encryption At Rest Options.
createElasticsearchDomain_encryptionAtRestOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe EncryptionAtRestOptions)
createElasticsearchDomain_encryptionAtRestOptions = Lens.lens (\CreateElasticsearchDomain' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@CreateElasticsearchDomain' {} a -> s {encryptionAtRestOptions = a} :: CreateElasticsearchDomain)

-- | Map of @LogType@ and @LogPublishingOption@, each containing options to
-- publish a given type of Elasticsearch log.
createElasticsearchDomain_logPublishingOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
createElasticsearchDomain_logPublishingOptions = Lens.lens (\CreateElasticsearchDomain' {logPublishingOptions} -> logPublishingOptions) (\s@CreateElasticsearchDomain' {} a -> s {logPublishingOptions = a} :: CreateElasticsearchDomain) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the NodeToNodeEncryptionOptions.
createElasticsearchDomain_nodeToNodeEncryptionOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe NodeToNodeEncryptionOptions)
createElasticsearchDomain_nodeToNodeEncryptionOptions = Lens.lens (\CreateElasticsearchDomain' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@CreateElasticsearchDomain' {} a -> s {nodeToNodeEncryptionOptions = a} :: CreateElasticsearchDomain)

-- | Option to set time, in UTC format, of the daily automated snapshot.
-- Default value is 0 hours.
createElasticsearchDomain_snapshotOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe SnapshotOptions)
createElasticsearchDomain_snapshotOptions = Lens.lens (\CreateElasticsearchDomain' {snapshotOptions} -> snapshotOptions) (\s@CreateElasticsearchDomain' {} a -> s {snapshotOptions = a} :: CreateElasticsearchDomain)

-- | A list of @Tag@ added during domain creation.
createElasticsearchDomain_tagList :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe [Tag])
createElasticsearchDomain_tagList = Lens.lens (\CreateElasticsearchDomain' {tagList} -> tagList) (\s@CreateElasticsearchDomain' {} a -> s {tagList = a} :: CreateElasticsearchDomain) Prelude.. Lens.mapping Lens.coerced

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/
createElasticsearchDomain_vPCOptions :: Lens.Lens' CreateElasticsearchDomain (Prelude.Maybe VPCOptions)
createElasticsearchDomain_vPCOptions = Lens.lens (\CreateElasticsearchDomain' {vPCOptions} -> vPCOptions) (\s@CreateElasticsearchDomain' {} a -> s {vPCOptions = a} :: CreateElasticsearchDomain)

-- | The name of the Elasticsearch domain that you are creating. Domain names
-- are unique across the domains owned by an account within an AWS region.
-- Domain names must start with a lowercase letter and can contain the
-- following characters: a-z (lowercase), 0-9, and - (hyphen).
createElasticsearchDomain_domainName :: Lens.Lens' CreateElasticsearchDomain Prelude.Text
createElasticsearchDomain_domainName = Lens.lens (\CreateElasticsearchDomain' {domainName} -> domainName) (\s@CreateElasticsearchDomain' {} a -> s {domainName = a} :: CreateElasticsearchDomain)

instance Core.AWSRequest CreateElasticsearchDomain where
  type
    AWSResponse CreateElasticsearchDomain =
      CreateElasticsearchDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateElasticsearchDomainResponse'
            Prelude.<$> (x Data..?> "DomainStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateElasticsearchDomain where
  hashWithSalt _salt CreateElasticsearchDomain' {..} =
    _salt `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` elasticsearchClusterConfig
      `Prelude.hashWithSalt` elasticsearchVersion
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` vPCOptions
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateElasticsearchDomain where
  rnf CreateElasticsearchDomain' {..} =
    Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf elasticsearchClusterConfig
      `Prelude.seq` Prelude.rnf elasticsearchVersion
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf vPCOptions
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders CreateElasticsearchDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateElasticsearchDomain where
  toJSON CreateElasticsearchDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessPolicies" Data..=)
              Prelude.<$> accessPolicies,
            ("AdvancedOptions" Data..=)
              Prelude.<$> advancedOptions,
            ("AdvancedSecurityOptions" Data..=)
              Prelude.<$> advancedSecurityOptions,
            ("AutoTuneOptions" Data..=)
              Prelude.<$> autoTuneOptions,
            ("CognitoOptions" Data..=)
              Prelude.<$> cognitoOptions,
            ("DomainEndpointOptions" Data..=)
              Prelude.<$> domainEndpointOptions,
            ("EBSOptions" Data..=) Prelude.<$> eBSOptions,
            ("ElasticsearchClusterConfig" Data..=)
              Prelude.<$> elasticsearchClusterConfig,
            ("ElasticsearchVersion" Data..=)
              Prelude.<$> elasticsearchVersion,
            ("EncryptionAtRestOptions" Data..=)
              Prelude.<$> encryptionAtRestOptions,
            ("LogPublishingOptions" Data..=)
              Prelude.<$> logPublishingOptions,
            ("NodeToNodeEncryptionOptions" Data..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("SnapshotOptions" Data..=)
              Prelude.<$> snapshotOptions,
            ("TagList" Data..=) Prelude.<$> tagList,
            ("VPCOptions" Data..=) Prelude.<$> vPCOptions,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath CreateElasticsearchDomain where
  toPath = Prelude.const "/2015-01-01/es/domain"

instance Data.ToQuery CreateElasticsearchDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CreateElasticsearchDomain@ operation. Contains the
-- status of the newly created Elasticsearch domain.
--
-- /See:/ 'newCreateElasticsearchDomainResponse' smart constructor.
data CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse'
  { -- | The status of the newly created Elasticsearch domain.
    domainStatus :: Prelude.Maybe ElasticsearchDomainStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateElasticsearchDomainResponse
newCreateElasticsearchDomainResponse pHttpStatus_ =
  CreateElasticsearchDomainResponse'
    { domainStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the newly created Elasticsearch domain.
createElasticsearchDomainResponse_domainStatus :: Lens.Lens' CreateElasticsearchDomainResponse (Prelude.Maybe ElasticsearchDomainStatus)
createElasticsearchDomainResponse_domainStatus = Lens.lens (\CreateElasticsearchDomainResponse' {domainStatus} -> domainStatus) (\s@CreateElasticsearchDomainResponse' {} a -> s {domainStatus = a} :: CreateElasticsearchDomainResponse)

-- | The response's http status code.
createElasticsearchDomainResponse_httpStatus :: Lens.Lens' CreateElasticsearchDomainResponse Prelude.Int
createElasticsearchDomainResponse_httpStatus = Lens.lens (\CreateElasticsearchDomainResponse' {httpStatus} -> httpStatus) (\s@CreateElasticsearchDomainResponse' {} a -> s {httpStatus = a} :: CreateElasticsearchDomainResponse)

instance
  Prelude.NFData
    CreateElasticsearchDomainResponse
  where
  rnf CreateElasticsearchDomainResponse' {..} =
    Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf httpStatus
