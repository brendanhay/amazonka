{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | If specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your custom domain name.
--
-- /See:/ 'newMutualTlsAuthentication' smart constructor.
data MutualTlsAuthentication = MutualTlsAuthentication'
  { -- | The version of the S3 object that contains your truststore. To specify a
    -- version, you must have versioning enabled for the S3 bucket.
    truststoreVersion :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 URL that specifies the truststore for mutual TLS
    -- authentication, for example @s3:\/\/bucket-name\/key-name@. The
    -- truststore can contain certificates from public or private certificate
    -- authorities. To update the truststore, upload a new version to S3, and
    -- then update your custom domain name to use the new version. To update
    -- the truststore, you must have permissions to access the S3 object.
    truststoreUri :: Prelude.Maybe Prelude.Text,
    -- | A list of warnings that API Gateway returns while processing your
    -- truststore. Invalid certificates produce warnings. Mutual TLS is still
    -- enabled, but some clients might not be able to access your API. To
    -- resolve warnings, upload a new truststore to S3, and then update you
    -- domain name to use the new version.
    truststoreWarnings :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      truststoreUri = Prelude.Nothing,
      truststoreWarnings = Prelude.Nothing
    }

-- | The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
mutualTlsAuthentication_truststoreVersion :: Lens.Lens' MutualTlsAuthentication (Prelude.Maybe Prelude.Text)
mutualTlsAuthentication_truststoreVersion = Lens.lens (\MutualTlsAuthentication' {truststoreVersion} -> truststoreVersion) (\s@MutualTlsAuthentication' {} a -> s {truststoreVersion = a} :: MutualTlsAuthentication)

-- | An Amazon S3 URL that specifies the truststore for mutual TLS
-- authentication, for example @s3:\/\/bucket-name\/key-name@. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
mutualTlsAuthentication_truststoreUri :: Lens.Lens' MutualTlsAuthentication (Prelude.Maybe Prelude.Text)
mutualTlsAuthentication_truststoreUri = Lens.lens (\MutualTlsAuthentication' {truststoreUri} -> truststoreUri) (\s@MutualTlsAuthentication' {} a -> s {truststoreUri = a} :: MutualTlsAuthentication)

-- | A list of warnings that API Gateway returns while processing your
-- truststore. Invalid certificates produce warnings. Mutual TLS is still
-- enabled, but some clients might not be able to access your API. To
-- resolve warnings, upload a new truststore to S3, and then update you
-- domain name to use the new version.
mutualTlsAuthentication_truststoreWarnings :: Lens.Lens' MutualTlsAuthentication (Prelude.Maybe [Prelude.Text])
mutualTlsAuthentication_truststoreWarnings = Lens.lens (\MutualTlsAuthentication' {truststoreWarnings} -> truststoreWarnings) (\s@MutualTlsAuthentication' {} a -> s {truststoreWarnings = a} :: MutualTlsAuthentication) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON MutualTlsAuthentication where
  parseJSON =
    Prelude.withObject
      "MutualTlsAuthentication"
      ( \x ->
          MutualTlsAuthentication'
            Prelude.<$> (x Prelude..:? "truststoreVersion")
            Prelude.<*> (x Prelude..:? "truststoreUri")
            Prelude.<*> ( x Prelude..:? "truststoreWarnings"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MutualTlsAuthentication

instance Prelude.NFData MutualTlsAuthentication
