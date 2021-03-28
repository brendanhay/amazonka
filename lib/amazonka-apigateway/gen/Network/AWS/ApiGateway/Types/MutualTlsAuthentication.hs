{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.MutualTlsAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.MutualTlsAuthentication
  ( MutualTlsAuthentication (..)
  -- * Smart constructor
  , mkMutualTlsAuthentication
  -- * Lenses
  , mtaTruststoreUri
  , mtaTruststoreVersion
  , mtaTruststoreWarnings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your custom domain name.
--
-- /See:/ 'mkMutualTlsAuthentication' smart constructor.
data MutualTlsAuthentication = MutualTlsAuthentication'
  { truststoreUri :: Core.Maybe Core.Text
    -- ^ An Amazon S3 URL that specifies the truststore for mutual TLS authentication, for example @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
  , truststoreVersion :: Core.Maybe Core.Text
    -- ^ The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
  , truststoreWarnings :: Core.Maybe [Core.Text]
    -- ^ A list of warnings that API Gateway returns while processing your truststore. Invalid certificates produce warnings. Mutual TLS is still enabled, but some clients might not be able to access your API. To resolve warnings, upload a new truststore to S3, and then update you domain name to use the new version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MutualTlsAuthentication' value with any optional fields omitted.
mkMutualTlsAuthentication
    :: MutualTlsAuthentication
mkMutualTlsAuthentication
  = MutualTlsAuthentication'{truststoreUri = Core.Nothing,
                             truststoreVersion = Core.Nothing,
                             truststoreWarnings = Core.Nothing}

-- | An Amazon S3 URL that specifies the truststore for mutual TLS authentication, for example @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
--
-- /Note:/ Consider using 'truststoreUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaTruststoreUri :: Lens.Lens' MutualTlsAuthentication (Core.Maybe Core.Text)
mtaTruststoreUri = Lens.field @"truststoreUri"
{-# INLINEABLE mtaTruststoreUri #-}
{-# DEPRECATED truststoreUri "Use generic-lens or generic-optics with 'truststoreUri' instead"  #-}

-- | The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
--
-- /Note:/ Consider using 'truststoreVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaTruststoreVersion :: Lens.Lens' MutualTlsAuthentication (Core.Maybe Core.Text)
mtaTruststoreVersion = Lens.field @"truststoreVersion"
{-# INLINEABLE mtaTruststoreVersion #-}
{-# DEPRECATED truststoreVersion "Use generic-lens or generic-optics with 'truststoreVersion' instead"  #-}

-- | A list of warnings that API Gateway returns while processing your truststore. Invalid certificates produce warnings. Mutual TLS is still enabled, but some clients might not be able to access your API. To resolve warnings, upload a new truststore to S3, and then update you domain name to use the new version.
--
-- /Note:/ Consider using 'truststoreWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtaTruststoreWarnings :: Lens.Lens' MutualTlsAuthentication (Core.Maybe [Core.Text])
mtaTruststoreWarnings = Lens.field @"truststoreWarnings"
{-# INLINEABLE mtaTruststoreWarnings #-}
{-# DEPRECATED truststoreWarnings "Use generic-lens or generic-optics with 'truststoreWarnings' instead"  #-}

instance Core.FromJSON MutualTlsAuthentication where
        parseJSON
          = Core.withObject "MutualTlsAuthentication" Core.$
              \ x ->
                MutualTlsAuthentication' Core.<$>
                  (x Core..:? "truststoreUri") Core.<*>
                    x Core..:? "truststoreVersion"
                    Core.<*> x Core..:? "truststoreWarnings"
