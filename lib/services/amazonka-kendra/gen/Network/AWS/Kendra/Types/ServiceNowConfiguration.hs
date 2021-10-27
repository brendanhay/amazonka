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
-- Module      : Network.AWS.Kendra.Types.ServiceNowConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ServiceNowConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ServiceNowAuthenticationType
import Network.AWS.Kendra.Types.ServiceNowBuildVersionType
import Network.AWS.Kendra.Types.ServiceNowKnowledgeArticleConfiguration
import Network.AWS.Kendra.Types.ServiceNowServiceCatalogConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information required to connect to a ServiceNow
-- data source.
--
-- /See:/ 'newServiceNowConfiguration' smart constructor.
data ServiceNowConfiguration = ServiceNowConfiguration'
  { -- | Provides configuration information for crawling knowledge articles in
    -- the ServiceNow site.
    knowledgeArticleConfiguration :: Prelude.Maybe ServiceNowKnowledgeArticleConfiguration,
    -- | Determines the type of authentication used to connect to the ServiceNow
    -- instance. If you choose @HTTP_BASIC@, Amazon Kendra is authenticated
    -- using the user name and password provided in the AWS Secrets Manager
    -- secret in the @SecretArn@ field. When you choose @OAUTH2@, Amazon Kendra
    -- is authenticated using the OAuth token and secret provided in the
    -- Secrets Manager secret, and the user name and password are used to
    -- determine which information Amazon Kendra has access to.
    --
    -- When you use @OAUTH2@ authentication, you must generate a token and a
    -- client secret using the ServiceNow console. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
    authenticationType :: Prelude.Maybe ServiceNowAuthenticationType,
    -- | Provides configuration information for crawling service catalogs in the
    -- ServiceNow site.
    serviceCatalogConfiguration :: Prelude.Maybe ServiceNowServiceCatalogConfiguration,
    -- | The ServiceNow instance that the data source connects to. The host
    -- endpoint should look like the following: @{instance}.service-now.com.@
    hostUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Secrets Manager secret that
    -- contains the user name and password required to connect to the
    -- ServiceNow instance.
    secretArn :: Prelude.Text,
    -- | The identifier of the release that the ServiceNow host is running. If
    -- the host is not running the @LONDON@ release, use @OTHERS@.
    serviceNowBuildVersion :: ServiceNowBuildVersionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNowConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeArticleConfiguration', 'serviceNowConfiguration_knowledgeArticleConfiguration' - Provides configuration information for crawling knowledge articles in
-- the ServiceNow site.
--
-- 'authenticationType', 'serviceNowConfiguration_authenticationType' - Determines the type of authentication used to connect to the ServiceNow
-- instance. If you choose @HTTP_BASIC@, Amazon Kendra is authenticated
-- using the user name and password provided in the AWS Secrets Manager
-- secret in the @SecretArn@ field. When you choose @OAUTH2@, Amazon Kendra
-- is authenticated using the OAuth token and secret provided in the
-- Secrets Manager secret, and the user name and password are used to
-- determine which information Amazon Kendra has access to.
--
-- When you use @OAUTH2@ authentication, you must generate a token and a
-- client secret using the ServiceNow console. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
--
-- 'serviceCatalogConfiguration', 'serviceNowConfiguration_serviceCatalogConfiguration' - Provides configuration information for crawling service catalogs in the
-- ServiceNow site.
--
-- 'hostUrl', 'serviceNowConfiguration_hostUrl' - The ServiceNow instance that the data source connects to. The host
-- endpoint should look like the following: @{instance}.service-now.com.@
--
-- 'secretArn', 'serviceNowConfiguration_secretArn' - The Amazon Resource Name (ARN) of the Secrets Manager secret that
-- contains the user name and password required to connect to the
-- ServiceNow instance.
--
-- 'serviceNowBuildVersion', 'serviceNowConfiguration_serviceNowBuildVersion' - The identifier of the release that the ServiceNow host is running. If
-- the host is not running the @LONDON@ release, use @OTHERS@.
newServiceNowConfiguration ::
  -- | 'hostUrl'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'serviceNowBuildVersion'
  ServiceNowBuildVersionType ->
  ServiceNowConfiguration
newServiceNowConfiguration
  pHostUrl_
  pSecretArn_
  pServiceNowBuildVersion_ =
    ServiceNowConfiguration'
      { knowledgeArticleConfiguration =
          Prelude.Nothing,
        authenticationType = Prelude.Nothing,
        serviceCatalogConfiguration = Prelude.Nothing,
        hostUrl = pHostUrl_,
        secretArn = pSecretArn_,
        serviceNowBuildVersion = pServiceNowBuildVersion_
      }

-- | Provides configuration information for crawling knowledge articles in
-- the ServiceNow site.
serviceNowConfiguration_knowledgeArticleConfiguration :: Lens.Lens' ServiceNowConfiguration (Prelude.Maybe ServiceNowKnowledgeArticleConfiguration)
serviceNowConfiguration_knowledgeArticleConfiguration = Lens.lens (\ServiceNowConfiguration' {knowledgeArticleConfiguration} -> knowledgeArticleConfiguration) (\s@ServiceNowConfiguration' {} a -> s {knowledgeArticleConfiguration = a} :: ServiceNowConfiguration)

-- | Determines the type of authentication used to connect to the ServiceNow
-- instance. If you choose @HTTP_BASIC@, Amazon Kendra is authenticated
-- using the user name and password provided in the AWS Secrets Manager
-- secret in the @SecretArn@ field. When you choose @OAUTH2@, Amazon Kendra
-- is authenticated using the OAuth token and secret provided in the
-- Secrets Manager secret, and the user name and password are used to
-- determine which information Amazon Kendra has access to.
--
-- When you use @OAUTH2@ authentication, you must generate a token and a
-- client secret using the ServiceNow console. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
serviceNowConfiguration_authenticationType :: Lens.Lens' ServiceNowConfiguration (Prelude.Maybe ServiceNowAuthenticationType)
serviceNowConfiguration_authenticationType = Lens.lens (\ServiceNowConfiguration' {authenticationType} -> authenticationType) (\s@ServiceNowConfiguration' {} a -> s {authenticationType = a} :: ServiceNowConfiguration)

-- | Provides configuration information for crawling service catalogs in the
-- ServiceNow site.
serviceNowConfiguration_serviceCatalogConfiguration :: Lens.Lens' ServiceNowConfiguration (Prelude.Maybe ServiceNowServiceCatalogConfiguration)
serviceNowConfiguration_serviceCatalogConfiguration = Lens.lens (\ServiceNowConfiguration' {serviceCatalogConfiguration} -> serviceCatalogConfiguration) (\s@ServiceNowConfiguration' {} a -> s {serviceCatalogConfiguration = a} :: ServiceNowConfiguration)

-- | The ServiceNow instance that the data source connects to. The host
-- endpoint should look like the following: @{instance}.service-now.com.@
serviceNowConfiguration_hostUrl :: Lens.Lens' ServiceNowConfiguration Prelude.Text
serviceNowConfiguration_hostUrl = Lens.lens (\ServiceNowConfiguration' {hostUrl} -> hostUrl) (\s@ServiceNowConfiguration' {} a -> s {hostUrl = a} :: ServiceNowConfiguration)

-- | The Amazon Resource Name (ARN) of the Secrets Manager secret that
-- contains the user name and password required to connect to the
-- ServiceNow instance.
serviceNowConfiguration_secretArn :: Lens.Lens' ServiceNowConfiguration Prelude.Text
serviceNowConfiguration_secretArn = Lens.lens (\ServiceNowConfiguration' {secretArn} -> secretArn) (\s@ServiceNowConfiguration' {} a -> s {secretArn = a} :: ServiceNowConfiguration)

-- | The identifier of the release that the ServiceNow host is running. If
-- the host is not running the @LONDON@ release, use @OTHERS@.
serviceNowConfiguration_serviceNowBuildVersion :: Lens.Lens' ServiceNowConfiguration ServiceNowBuildVersionType
serviceNowConfiguration_serviceNowBuildVersion = Lens.lens (\ServiceNowConfiguration' {serviceNowBuildVersion} -> serviceNowBuildVersion) (\s@ServiceNowConfiguration' {} a -> s {serviceNowBuildVersion = a} :: ServiceNowConfiguration)

instance Core.FromJSON ServiceNowConfiguration where
  parseJSON =
    Core.withObject
      "ServiceNowConfiguration"
      ( \x ->
          ServiceNowConfiguration'
            Prelude.<$> (x Core..:? "KnowledgeArticleConfiguration")
            Prelude.<*> (x Core..:? "AuthenticationType")
            Prelude.<*> (x Core..:? "ServiceCatalogConfiguration")
            Prelude.<*> (x Core..: "HostUrl")
            Prelude.<*> (x Core..: "SecretArn")
            Prelude.<*> (x Core..: "ServiceNowBuildVersion")
      )

instance Prelude.Hashable ServiceNowConfiguration

instance Prelude.NFData ServiceNowConfiguration

instance Core.ToJSON ServiceNowConfiguration where
  toJSON ServiceNowConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KnowledgeArticleConfiguration" Core..=)
              Prelude.<$> knowledgeArticleConfiguration,
            ("AuthenticationType" Core..=)
              Prelude.<$> authenticationType,
            ("ServiceCatalogConfiguration" Core..=)
              Prelude.<$> serviceCatalogConfiguration,
            Prelude.Just ("HostUrl" Core..= hostUrl),
            Prelude.Just ("SecretArn" Core..= secretArn),
            Prelude.Just
              ( "ServiceNowBuildVersion"
                  Core..= serviceNowBuildVersion
              )
          ]
      )
