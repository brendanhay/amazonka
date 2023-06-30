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
-- Module      : Amazonka.ApiGatewayV2.Types.MutualTlsAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.MutualTlsAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newMutualTlsAuthentication' smart constructor.
data MutualTlsAuthentication = MutualTlsAuthentication'
  { -- | An Amazon S3 URL that specifies the truststore for mutual TLS
    -- authentication, for example, s3:\/\/bucket-name\/key-name. The
    -- truststore can contain certificates from public or private certificate
    -- authorities. To update the truststore, upload a new version to S3, and
    -- then update your custom domain name to use the new version. To update
    -- the truststore, you must have permissions to access the S3 object.
    truststoreUri :: Prelude.Maybe Prelude.Text,
    -- | The version of the S3 object that contains your truststore. To specify a
    -- version, you must have versioning enabled for the S3 bucket.
    truststoreVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of warnings that API Gateway returns while processing your
    -- truststore. Invalid certificates produce warnings. Mutual TLS is still
    -- enabled, but some clients might not be able to access your API. To
    -- resolve warnings, upload a new truststore to S3, and then update you
    -- domain name to use the new version.
    truststoreWarnings :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MutualTlsAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truststoreUri', 'mutualTlsAuthentication_truststoreUri' - An Amazon S3 URL that specifies the truststore for mutual TLS
-- authentication, for example, s3:\/\/bucket-name\/key-name. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
--
-- 'truststoreVersion', 'mutualTlsAuthentication_truststoreVersion' - The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
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
    { truststoreUri =
        Prelude.Nothing,
      truststoreVersion = Prelude.Nothing,
      truststoreWarnings = Prelude.Nothing
    }

-- | An Amazon S3 URL that specifies the truststore for mutual TLS
-- authentication, for example, s3:\/\/bucket-name\/key-name. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
mutualTlsAuthentication_truststoreUri :: Lens.Lens' MutualTlsAuthentication (Prelude.Maybe Prelude.Text)
mutualTlsAuthentication_truststoreUri = Lens.lens (\MutualTlsAuthentication' {truststoreUri} -> truststoreUri) (\s@MutualTlsAuthentication' {} a -> s {truststoreUri = a} :: MutualTlsAuthentication)

-- | The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
mutualTlsAuthentication_truststoreVersion :: Lens.Lens' MutualTlsAuthentication (Prelude.Maybe Prelude.Text)
mutualTlsAuthentication_truststoreVersion = Lens.lens (\MutualTlsAuthentication' {truststoreVersion} -> truststoreVersion) (\s@MutualTlsAuthentication' {} a -> s {truststoreVersion = a} :: MutualTlsAuthentication)

-- | A list of warnings that API Gateway returns while processing your
-- truststore. Invalid certificates produce warnings. Mutual TLS is still
-- enabled, but some clients might not be able to access your API. To
-- resolve warnings, upload a new truststore to S3, and then update you
-- domain name to use the new version.
mutualTlsAuthentication_truststoreWarnings :: Lens.Lens' MutualTlsAuthentication (Prelude.Maybe [Prelude.Text])
mutualTlsAuthentication_truststoreWarnings = Lens.lens (\MutualTlsAuthentication' {truststoreWarnings} -> truststoreWarnings) (\s@MutualTlsAuthentication' {} a -> s {truststoreWarnings = a} :: MutualTlsAuthentication) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MutualTlsAuthentication where
  parseJSON =
    Data.withObject
      "MutualTlsAuthentication"
      ( \x ->
          MutualTlsAuthentication'
            Prelude.<$> (x Data..:? "truststoreUri")
            Prelude.<*> (x Data..:? "truststoreVersion")
            Prelude.<*> ( x
                            Data..:? "truststoreWarnings"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MutualTlsAuthentication where
  hashWithSalt _salt MutualTlsAuthentication' {..} =
    _salt
      `Prelude.hashWithSalt` truststoreUri
      `Prelude.hashWithSalt` truststoreVersion
      `Prelude.hashWithSalt` truststoreWarnings

instance Prelude.NFData MutualTlsAuthentication where
  rnf MutualTlsAuthentication' {..} =
    Prelude.rnf truststoreUri
      `Prelude.seq` Prelude.rnf truststoreVersion
      `Prelude.seq` Prelude.rnf truststoreWarnings
