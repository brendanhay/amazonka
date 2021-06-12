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
-- Module      : Network.AWS.APIGateway.Types.MutualTlsAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MutualTlsAuthentication where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | If specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your custom domain name.
--
-- /See:/ 'newMutualTlsAuthentication' smart constructor.
data MutualTlsAuthentication = MutualTlsAuthentication'
  { -- | The version of the S3 object that contains your truststore. To specify a
    -- version, you must have versioning enabled for the S3 bucket.
    truststoreVersion :: Core.Maybe Core.Text,
    -- | An Amazon S3 URL that specifies the truststore for mutual TLS
    -- authentication, for example @s3:\/\/bucket-name\/key-name@. The
    -- truststore can contain certificates from public or private certificate
    -- authorities. To update the truststore, upload a new version to S3, and
    -- then update your custom domain name to use the new version. To update
    -- the truststore, you must have permissions to access the S3 object.
    truststoreUri :: Core.Maybe Core.Text,
    -- | A list of warnings that API Gateway returns while processing your
    -- truststore. Invalid certificates produce warnings. Mutual TLS is still
    -- enabled, but some clients might not be able to access your API. To
    -- resolve warnings, upload a new truststore to S3, and then update you
    -- domain name to use the new version.
    truststoreWarnings :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MutualTlsAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truststoreVersion', 'mutualTlsAuthentication_truststoreVersion' - The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
--
-- 'truststoreUri', 'mutualTlsAuthentication_truststoreUri' - An Amazon S3 URL that specifies the truststore for mutual TLS
-- authentication, for example @s3:\/\/bucket-name\/key-name@. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
--
-- 'truststoreWarnings', 'mutualTlsAuthentication_truststoreWarnings' - A list of warnings that API Gateway returns while processing your
-- truststore. Invalid certificates produce warnings. Mutual TLS is still
-- enabled, but some clients might not be able to access your API. To
-- resolve warnings, upload a new truststore to S3, and then update you
-- domain name to use the new version.
newMutualTlsAuthentication ::
  MutualTlsAuthentication
newMutualTlsAuthentication =
  MutualTlsAuthentication'
    { truststoreVersion =
        Core.Nothing,
      truststoreUri = Core.Nothing,
      truststoreWarnings = Core.Nothing
    }

-- | The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
mutualTlsAuthentication_truststoreVersion :: Lens.Lens' MutualTlsAuthentication (Core.Maybe Core.Text)
mutualTlsAuthentication_truststoreVersion = Lens.lens (\MutualTlsAuthentication' {truststoreVersion} -> truststoreVersion) (\s@MutualTlsAuthentication' {} a -> s {truststoreVersion = a} :: MutualTlsAuthentication)

-- | An Amazon S3 URL that specifies the truststore for mutual TLS
-- authentication, for example @s3:\/\/bucket-name\/key-name@. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
mutualTlsAuthentication_truststoreUri :: Lens.Lens' MutualTlsAuthentication (Core.Maybe Core.Text)
mutualTlsAuthentication_truststoreUri = Lens.lens (\MutualTlsAuthentication' {truststoreUri} -> truststoreUri) (\s@MutualTlsAuthentication' {} a -> s {truststoreUri = a} :: MutualTlsAuthentication)

-- | A list of warnings that API Gateway returns while processing your
-- truststore. Invalid certificates produce warnings. Mutual TLS is still
-- enabled, but some clients might not be able to access your API. To
-- resolve warnings, upload a new truststore to S3, and then update you
-- domain name to use the new version.
mutualTlsAuthentication_truststoreWarnings :: Lens.Lens' MutualTlsAuthentication (Core.Maybe [Core.Text])
mutualTlsAuthentication_truststoreWarnings = Lens.lens (\MutualTlsAuthentication' {truststoreWarnings} -> truststoreWarnings) (\s@MutualTlsAuthentication' {} a -> s {truststoreWarnings = a} :: MutualTlsAuthentication) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON MutualTlsAuthentication where
  parseJSON =
    Core.withObject
      "MutualTlsAuthentication"
      ( \x ->
          MutualTlsAuthentication'
            Core.<$> (x Core..:? "truststoreVersion")
            Core.<*> (x Core..:? "truststoreUri")
            Core.<*> ( x Core..:? "truststoreWarnings"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable MutualTlsAuthentication

instance Core.NFData MutualTlsAuthentication
