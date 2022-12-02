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
-- Module      : Amazonka.Kendra.Types.OnPremiseConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.OnPremiseConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.S3Path
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to GitHub Enterprise
-- Server (on premises).
--
-- /See:/ 'newOnPremiseConfiguration' smart constructor.
data OnPremiseConfiguration = OnPremiseConfiguration'
  { -- | The GitHub host URL or API endpoint URL. For example,
    -- /https:\/\/on-prem-host-url\/api\/v3\//
    hostUrl :: Prelude.Text,
    -- | The name of the organization of the GitHub Enterprise Server
    -- (in-premise) account you want to connect to. You can find your
    -- organization name by logging into GitHub desktop and selecting __Your
    -- organizations__ under your profile picture dropdown.
    organizationName :: Prelude.Text,
    -- | The path to the SSL certificate stored in an Amazon S3 bucket. You use
    -- this to connect to GitHub if you require a secure SSL connection.
    --
    -- You can simply generate a self-signed X509 certificate on any computer
    -- using OpenSSL. For an example of using OpenSSL to create an X509
    -- certificate, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
    sslCertificateS3Path :: S3Path
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnPremiseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostUrl', 'onPremiseConfiguration_hostUrl' - The GitHub host URL or API endpoint URL. For example,
-- /https:\/\/on-prem-host-url\/api\/v3\//
--
-- 'organizationName', 'onPremiseConfiguration_organizationName' - The name of the organization of the GitHub Enterprise Server
-- (in-premise) account you want to connect to. You can find your
-- organization name by logging into GitHub desktop and selecting __Your
-- organizations__ under your profile picture dropdown.
--
-- 'sslCertificateS3Path', 'onPremiseConfiguration_sslCertificateS3Path' - The path to the SSL certificate stored in an Amazon S3 bucket. You use
-- this to connect to GitHub if you require a secure SSL connection.
--
-- You can simply generate a self-signed X509 certificate on any computer
-- using OpenSSL. For an example of using OpenSSL to create an X509
-- certificate, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
newOnPremiseConfiguration ::
  -- | 'hostUrl'
  Prelude.Text ->
  -- | 'organizationName'
  Prelude.Text ->
  -- | 'sslCertificateS3Path'
  S3Path ->
  OnPremiseConfiguration
newOnPremiseConfiguration
  pHostUrl_
  pOrganizationName_
  pSslCertificateS3Path_ =
    OnPremiseConfiguration'
      { hostUrl = pHostUrl_,
        organizationName = pOrganizationName_,
        sslCertificateS3Path = pSslCertificateS3Path_
      }

-- | The GitHub host URL or API endpoint URL. For example,
-- /https:\/\/on-prem-host-url\/api\/v3\//
onPremiseConfiguration_hostUrl :: Lens.Lens' OnPremiseConfiguration Prelude.Text
onPremiseConfiguration_hostUrl = Lens.lens (\OnPremiseConfiguration' {hostUrl} -> hostUrl) (\s@OnPremiseConfiguration' {} a -> s {hostUrl = a} :: OnPremiseConfiguration)

-- | The name of the organization of the GitHub Enterprise Server
-- (in-premise) account you want to connect to. You can find your
-- organization name by logging into GitHub desktop and selecting __Your
-- organizations__ under your profile picture dropdown.
onPremiseConfiguration_organizationName :: Lens.Lens' OnPremiseConfiguration Prelude.Text
onPremiseConfiguration_organizationName = Lens.lens (\OnPremiseConfiguration' {organizationName} -> organizationName) (\s@OnPremiseConfiguration' {} a -> s {organizationName = a} :: OnPremiseConfiguration)

-- | The path to the SSL certificate stored in an Amazon S3 bucket. You use
-- this to connect to GitHub if you require a secure SSL connection.
--
-- You can simply generate a self-signed X509 certificate on any computer
-- using OpenSSL. For an example of using OpenSSL to create an X509
-- certificate, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
onPremiseConfiguration_sslCertificateS3Path :: Lens.Lens' OnPremiseConfiguration S3Path
onPremiseConfiguration_sslCertificateS3Path = Lens.lens (\OnPremiseConfiguration' {sslCertificateS3Path} -> sslCertificateS3Path) (\s@OnPremiseConfiguration' {} a -> s {sslCertificateS3Path = a} :: OnPremiseConfiguration)

instance Data.FromJSON OnPremiseConfiguration where
  parseJSON =
    Data.withObject
      "OnPremiseConfiguration"
      ( \x ->
          OnPremiseConfiguration'
            Prelude.<$> (x Data..: "HostUrl")
            Prelude.<*> (x Data..: "OrganizationName")
            Prelude.<*> (x Data..: "SslCertificateS3Path")
      )

instance Prelude.Hashable OnPremiseConfiguration where
  hashWithSalt _salt OnPremiseConfiguration' {..} =
    _salt `Prelude.hashWithSalt` hostUrl
      `Prelude.hashWithSalt` organizationName
      `Prelude.hashWithSalt` sslCertificateS3Path

instance Prelude.NFData OnPremiseConfiguration where
  rnf OnPremiseConfiguration' {..} =
    Prelude.rnf hostUrl
      `Prelude.seq` Prelude.rnf organizationName
      `Prelude.seq` Prelude.rnf sslCertificateS3Path

instance Data.ToJSON OnPremiseConfiguration where
  toJSON OnPremiseConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HostUrl" Data..= hostUrl),
            Prelude.Just
              ("OrganizationName" Data..= organizationName),
            Prelude.Just
              ( "SslCertificateS3Path"
                  Data..= sslCertificateS3Path
              )
          ]
      )
