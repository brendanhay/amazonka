{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.MutualTlsAuthenticationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.MutualTlsAuthenticationInput
  ( MutualTlsAuthenticationInput (..)
  -- * Smart constructor
  , mkMutualTlsAuthenticationInput
  -- * Lenses
  , mtaiTruststoreUri
  , mtaiTruststoreVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your custom domain name.
--
-- /See:/ 'mkMutualTlsAuthenticationInput' smart constructor.
data MutualTlsAuthenticationInput = MutualTlsAuthenticationInput'
  { truststoreUri :: Core.Maybe Core.Text
    -- ^ An Amazon S3 resource ARN that specifies the truststore for mutual TLS authentication, for example, @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
  , truststoreVersion :: Core.Maybe Core.Text
    -- ^ The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MutualTlsAuthenticationInput' value with any optional fields omitted.
mkMutualTlsAuthenticationInput
    :: MutualTlsAuthenticationInput
mkMutualTlsAuthenticationInput
  = MutualTlsAuthenticationInput'{truststoreUri = Core.Nothing,
                                  truststoreVersion = Core.Nothing}

-- | An Amazon S3 resource ARN that specifies the truststore for mutual TLS authentication, for example, @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
--
-- /Note:/ Consider using 'truststoreUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaiTruststoreUri :: Lens.Lens' MutualTlsAuthenticationInput (Core.Maybe Core.Text)
mtaiTruststoreUri = Lens.field @"truststoreUri"
{-# INLINEABLE mtaiTruststoreUri #-}
{-# DEPRECATED truststoreUri "Use generic-lens or generic-optics with 'truststoreUri' instead"  #-}

-- | The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
--
-- /Note:/ Consider using 'truststoreVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaiTruststoreVersion :: Lens.Lens' MutualTlsAuthenticationInput (Core.Maybe Core.Text)
mtaiTruststoreVersion = Lens.field @"truststoreVersion"
{-# INLINEABLE mtaiTruststoreVersion #-}
{-# DEPRECATED truststoreVersion "Use generic-lens or generic-optics with 'truststoreVersion' instead"  #-}

instance Core.FromJSON MutualTlsAuthenticationInput where
        toJSON MutualTlsAuthenticationInput{..}
          = Core.object
              (Core.catMaybes
                 [("truststoreUri" Core..=) Core.<$> truststoreUri,
                  ("truststoreVersion" Core..=) Core.<$> truststoreVersion])
