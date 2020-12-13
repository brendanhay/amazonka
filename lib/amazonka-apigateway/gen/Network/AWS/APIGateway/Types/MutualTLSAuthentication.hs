{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MutualTLSAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MutualTLSAuthentication
  ( MutualTLSAuthentication (..),

    -- * Smart constructor
    mkMutualTLSAuthentication,

    -- * Lenses
    mtaTruststoreWarnings,
    mtaTruststoreURI,
    mtaTruststoreVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your custom domain name.
--
-- /See:/ 'mkMutualTLSAuthentication' smart constructor.
data MutualTLSAuthentication = MutualTLSAuthentication'
  { -- | A list of warnings that API Gateway returns while processing your truststore. Invalid certificates produce warnings. Mutual TLS is still enabled, but some clients might not be able to access your API. To resolve warnings, upload a new truststore to S3, and then update you domain name to use the new version.
    truststoreWarnings :: Lude.Maybe [Lude.Text],
    -- | An Amazon S3 URL that specifies the truststore for mutual TLS authentication, for example @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
    truststoreURI :: Lude.Maybe Lude.Text,
    -- | The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
    truststoreVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MutualTLSAuthentication' with the minimum fields required to make a request.
--
-- * 'truststoreWarnings' - A list of warnings that API Gateway returns while processing your truststore. Invalid certificates produce warnings. Mutual TLS is still enabled, but some clients might not be able to access your API. To resolve warnings, upload a new truststore to S3, and then update you domain name to use the new version.
-- * 'truststoreURI' - An Amazon S3 URL that specifies the truststore for mutual TLS authentication, for example @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
-- * 'truststoreVersion' - The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
mkMutualTLSAuthentication ::
  MutualTLSAuthentication
mkMutualTLSAuthentication =
  MutualTLSAuthentication'
    { truststoreWarnings = Lude.Nothing,
      truststoreURI = Lude.Nothing,
      truststoreVersion = Lude.Nothing
    }

-- | A list of warnings that API Gateway returns while processing your truststore. Invalid certificates produce warnings. Mutual TLS is still enabled, but some clients might not be able to access your API. To resolve warnings, upload a new truststore to S3, and then update you domain name to use the new version.
--
-- /Note:/ Consider using 'truststoreWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaTruststoreWarnings :: Lens.Lens' MutualTLSAuthentication (Lude.Maybe [Lude.Text])
mtaTruststoreWarnings = Lens.lens (truststoreWarnings :: MutualTLSAuthentication -> Lude.Maybe [Lude.Text]) (\s a -> s {truststoreWarnings = a} :: MutualTLSAuthentication)
{-# DEPRECATED mtaTruststoreWarnings "Use generic-lens or generic-optics with 'truststoreWarnings' instead." #-}

-- | An Amazon S3 URL that specifies the truststore for mutual TLS authentication, for example @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
--
-- /Note:/ Consider using 'truststoreURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaTruststoreURI :: Lens.Lens' MutualTLSAuthentication (Lude.Maybe Lude.Text)
mtaTruststoreURI = Lens.lens (truststoreURI :: MutualTLSAuthentication -> Lude.Maybe Lude.Text) (\s a -> s {truststoreURI = a} :: MutualTLSAuthentication)
{-# DEPRECATED mtaTruststoreURI "Use generic-lens or generic-optics with 'truststoreURI' instead." #-}

-- | The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
--
-- /Note:/ Consider using 'truststoreVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaTruststoreVersion :: Lens.Lens' MutualTLSAuthentication (Lude.Maybe Lude.Text)
mtaTruststoreVersion = Lens.lens (truststoreVersion :: MutualTLSAuthentication -> Lude.Maybe Lude.Text) (\s a -> s {truststoreVersion = a} :: MutualTLSAuthentication)
{-# DEPRECATED mtaTruststoreVersion "Use generic-lens or generic-optics with 'truststoreVersion' instead." #-}

instance Lude.FromJSON MutualTLSAuthentication where
  parseJSON =
    Lude.withObject
      "MutualTLSAuthentication"
      ( \x ->
          MutualTLSAuthentication'
            Lude.<$> (x Lude..:? "truststoreWarnings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "truststoreUri")
            Lude.<*> (x Lude..:? "truststoreVersion")
      )
