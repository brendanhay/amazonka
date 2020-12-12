{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MutualTLSAuthenticationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MutualTLSAuthenticationInput
  ( MutualTLSAuthenticationInput (..),

    -- * Smart constructor
    mkMutualTLSAuthenticationInput,

    -- * Lenses
    mtaiTruststoreURI,
    mtaiTruststoreVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your custom domain name.
--
-- /See:/ 'mkMutualTLSAuthenticationInput' smart constructor.
data MutualTLSAuthenticationInput = MutualTLSAuthenticationInput'
  { truststoreURI ::
      Lude.Maybe Lude.Text,
    truststoreVersion ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MutualTLSAuthenticationInput' with the minimum fields required to make a request.
--
-- * 'truststoreURI' - An Amazon S3 resource ARN that specifies the truststore for mutual TLS authentication, for example, @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
-- * 'truststoreVersion' - The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
mkMutualTLSAuthenticationInput ::
  MutualTLSAuthenticationInput
mkMutualTLSAuthenticationInput =
  MutualTLSAuthenticationInput'
    { truststoreURI = Lude.Nothing,
      truststoreVersion = Lude.Nothing
    }

-- | An Amazon S3 resource ARN that specifies the truststore for mutual TLS authentication, for example, @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
--
-- /Note:/ Consider using 'truststoreURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaiTruststoreURI :: Lens.Lens' MutualTLSAuthenticationInput (Lude.Maybe Lude.Text)
mtaiTruststoreURI = Lens.lens (truststoreURI :: MutualTLSAuthenticationInput -> Lude.Maybe Lude.Text) (\s a -> s {truststoreURI = a} :: MutualTLSAuthenticationInput)
{-# DEPRECATED mtaiTruststoreURI "Use generic-lens or generic-optics with 'truststoreURI' instead." #-}

-- | The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
--
-- /Note:/ Consider using 'truststoreVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaiTruststoreVersion :: Lens.Lens' MutualTLSAuthenticationInput (Lude.Maybe Lude.Text)
mtaiTruststoreVersion = Lens.lens (truststoreVersion :: MutualTLSAuthenticationInput -> Lude.Maybe Lude.Text) (\s a -> s {truststoreVersion = a} :: MutualTLSAuthenticationInput)
{-# DEPRECATED mtaiTruststoreVersion "Use generic-lens or generic-optics with 'truststoreVersion' instead." #-}

instance Lude.ToJSON MutualTLSAuthenticationInput where
  toJSON MutualTLSAuthenticationInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("truststoreUri" Lude..=) Lude.<$> truststoreURI,
            ("truststoreVersion" Lude..=) Lude.<$> truststoreVersion
          ]
      )
