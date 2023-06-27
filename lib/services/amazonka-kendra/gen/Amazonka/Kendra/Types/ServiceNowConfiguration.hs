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
-- Module      : Amazonka.Kendra.Types.ServiceNowConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ServiceNowConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ServiceNowAuthenticationType
import Amazonka.Kendra.Types.ServiceNowBuildVersionType
import Amazonka.Kendra.Types.ServiceNowKnowledgeArticleConfiguration
import Amazonka.Kendra.Types.ServiceNowServiceCatalogConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to ServiceNow as your
-- data source.
--
-- /See:/ 'newServiceNowConfiguration' smart constructor.
data ServiceNowConfiguration = ServiceNowConfiguration'
  { -- | The type of authentication used to connect to the ServiceNow instance.
    -- If you choose @HTTP_BASIC@, Amazon Kendra is authenticated using the
    -- user name and password provided in the Secrets Manager secret in the
    -- @SecretArn@ field. If you choose @OAUTH2@, Amazon Kendra is
    -- authenticated using the credentials of client ID, client secret, user
    -- name and password.
    --
    -- When you use @OAUTH2@ authentication, you must generate a token and a
    -- client secret using the ServiceNow console. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
    authenticationType :: Prelude.Maybe ServiceNowAuthenticationType,
    -- | Configuration information for crawling knowledge articles in the
    -- ServiceNow site.
    knowledgeArticleConfiguration :: Prelude.Maybe ServiceNowKnowledgeArticleConfiguration,
    -- | Configuration information for crawling service catalogs in the
    -- ServiceNow site.
    serviceCatalogConfiguration :: Prelude.Maybe ServiceNowServiceCatalogConfiguration,
    -- | The ServiceNow instance that the data source connects to. The host
    -- endpoint should look like the following: /{instance}.service-now.com./
    hostUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Secrets Manager secret that
    -- contains the user name and password required to connect to the
    -- ServiceNow instance. You can also provide OAuth authentication
    -- credentials of user name, password, client ID, and client secret. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
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
-- 'authenticationType', 'serviceNowConfiguration_authenticationType' - The type of authentication used to connect to the ServiceNow instance.
-- If you choose @HTTP_BASIC@, Amazon Kendra is authenticated using the
-- user name and password provided in the Secrets Manager secret in the
-- @SecretArn@ field. If you choose @OAUTH2@, Amazon Kendra is
-- authenticated using the credentials of client ID, client secret, user
-- name and password.
--
-- When you use @OAUTH2@ authentication, you must generate a token and a
-- client secret using the ServiceNow console. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
--
-- 'knowledgeArticleConfiguration', 'serviceNowConfiguration_knowledgeArticleConfiguration' - Configuration information for crawling knowledge articles in the
-- ServiceNow site.
--
-- 'serviceCatalogConfiguration', 'serviceNowConfiguration_serviceCatalogConfiguration' - Configuration information for crawling service catalogs in the
-- ServiceNow site.
--
-- 'hostUrl', 'serviceNowConfiguration_hostUrl' - The ServiceNow instance that the data source connects to. The host
-- endpoint should look like the following: /{instance}.service-now.com./
--
-- 'secretArn', 'serviceNowConfiguration_secretArn' - The Amazon Resource Name (ARN) of the Secrets Manager secret that
-- contains the user name and password required to connect to the
-- ServiceNow instance. You can also provide OAuth authentication
-- credentials of user name, password, client ID, and client secret. For
-- more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
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
      { authenticationType =
          Prelude.Nothing,
        knowledgeArticleConfiguration = Prelude.Nothing,
        serviceCatalogConfiguration = Prelude.Nothing,
        hostUrl = pHostUrl_,
        secretArn = pSecretArn_,
        serviceNowBuildVersion = pServiceNowBuildVersion_
      }

-- | The type of authentication used to connect to the ServiceNow instance.
-- If you choose @HTTP_BASIC@, Amazon Kendra is authenticated using the
-- user name and password provided in the Secrets Manager secret in the
-- @SecretArn@ field. If you choose @OAUTH2@, Amazon Kendra is
-- authenticated using the credentials of client ID, client secret, user
-- name and password.
--
-- When you use @OAUTH2@ authentication, you must generate a token and a
-- client secret using the ServiceNow console. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
serviceNowConfiguration_authenticationType :: Lens.Lens' ServiceNowConfiguration (Prelude.Maybe ServiceNowAuthenticationType)
serviceNowConfiguration_authenticationType = Lens.lens (\ServiceNowConfiguration' {authenticationType} -> authenticationType) (\s@ServiceNowConfiguration' {} a -> s {authenticationType = a} :: ServiceNowConfiguration)

-- | Configuration information for crawling knowledge articles in the
-- ServiceNow site.
serviceNowConfiguration_knowledgeArticleConfiguration :: Lens.Lens' ServiceNowConfiguration (Prelude.Maybe ServiceNowKnowledgeArticleConfiguration)
serviceNowConfiguration_knowledgeArticleConfiguration = Lens.lens (\ServiceNowConfiguration' {knowledgeArticleConfiguration} -> knowledgeArticleConfiguration) (\s@ServiceNowConfiguration' {} a -> s {knowledgeArticleConfiguration = a} :: ServiceNowConfiguration)

-- | Configuration information for crawling service catalogs in the
-- ServiceNow site.
serviceNowConfiguration_serviceCatalogConfiguration :: Lens.Lens' ServiceNowConfiguration (Prelude.Maybe ServiceNowServiceCatalogConfiguration)
serviceNowConfiguration_serviceCatalogConfiguration = Lens.lens (\ServiceNowConfiguration' {serviceCatalogConfiguration} -> serviceCatalogConfiguration) (\s@ServiceNowConfiguration' {} a -> s {serviceCatalogConfiguration = a} :: ServiceNowConfiguration)

-- | The ServiceNow instance that the data source connects to. The host
-- endpoint should look like the following: /{instance}.service-now.com./
serviceNowConfiguration_hostUrl :: Lens.Lens' ServiceNowConfiguration Prelude.Text
serviceNowConfiguration_hostUrl = Lens.lens (\ServiceNowConfiguration' {hostUrl} -> hostUrl) (\s@ServiceNowConfiguration' {} a -> s {hostUrl = a} :: ServiceNowConfiguration)

-- | The Amazon Resource Name (ARN) of the Secrets Manager secret that
-- contains the user name and password required to connect to the
-- ServiceNow instance. You can also provide OAuth authentication
-- credentials of user name, password, client ID, and client secret. For
-- more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-servicenow.html Using a ServiceNow data source>.
serviceNowConfiguration_secretArn :: Lens.Lens' ServiceNowConfiguration Prelude.Text
serviceNowConfiguration_secretArn = Lens.lens (\ServiceNowConfiguration' {secretArn} -> secretArn) (\s@ServiceNowConfiguration' {} a -> s {secretArn = a} :: ServiceNowConfiguration)

-- | The identifier of the release that the ServiceNow host is running. If
-- the host is not running the @LONDON@ release, use @OTHERS@.
serviceNowConfiguration_serviceNowBuildVersion :: Lens.Lens' ServiceNowConfiguration ServiceNowBuildVersionType
serviceNowConfiguration_serviceNowBuildVersion = Lens.lens (\ServiceNowConfiguration' {serviceNowBuildVersion} -> serviceNowBuildVersion) (\s@ServiceNowConfiguration' {} a -> s {serviceNowBuildVersion = a} :: ServiceNowConfiguration)

instance Data.FromJSON ServiceNowConfiguration where
  parseJSON =
    Data.withObject
      "ServiceNowConfiguration"
      ( \x ->
          ServiceNowConfiguration'
            Prelude.<$> (x Data..:? "AuthenticationType")
            Prelude.<*> (x Data..:? "KnowledgeArticleConfiguration")
            Prelude.<*> (x Data..:? "ServiceCatalogConfiguration")
            Prelude.<*> (x Data..: "HostUrl")
            Prelude.<*> (x Data..: "SecretArn")
            Prelude.<*> (x Data..: "ServiceNowBuildVersion")
      )

instance Prelude.Hashable ServiceNowConfiguration where
  hashWithSalt _salt ServiceNowConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` knowledgeArticleConfiguration
      `Prelude.hashWithSalt` serviceCatalogConfiguration
      `Prelude.hashWithSalt` hostUrl
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` serviceNowBuildVersion

instance Prelude.NFData ServiceNowConfiguration where
  rnf ServiceNowConfiguration' {..} =
    Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf knowledgeArticleConfiguration
      `Prelude.seq` Prelude.rnf serviceCatalogConfiguration
      `Prelude.seq` Prelude.rnf hostUrl
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf serviceNowBuildVersion

instance Data.ToJSON ServiceNowConfiguration where
  toJSON ServiceNowConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthenticationType" Data..=)
              Prelude.<$> authenticationType,
            ("KnowledgeArticleConfiguration" Data..=)
              Prelude.<$> knowledgeArticleConfiguration,
            ("ServiceCatalogConfiguration" Data..=)
              Prelude.<$> serviceCatalogConfiguration,
            Prelude.Just ("HostUrl" Data..= hostUrl),
            Prelude.Just ("SecretArn" Data..= secretArn),
            Prelude.Just
              ( "ServiceNowBuildVersion"
                  Data..= serviceNowBuildVersion
              )
          ]
      )
