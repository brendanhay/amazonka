{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.TlsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.TlsConfig
  ( TlsConfig (..)
  -- * Smart constructor
  , mkTlsConfig
  -- * Lenses
  , tcInsecureSkipVerification
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkTlsConfig' smart constructor.
newtype TlsConfig = TlsConfig'
  { insecureSkipVerification :: Core.Maybe Core.Bool
    -- ^ Specifies whether or not API Gateway skips verification that the certificate for an integration endpoint is issued by a <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority> . This isn’t recommended, but it enables you to use certificates that are signed by private certificate authorities, or certificates that are self-signed. If enabled, API Gateway still performs basic certificate validation, which includes checking the certificate's expiration date, hostname, and presence of a root certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@ integrations.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TlsConfig' value with any optional fields omitted.
mkTlsConfig
    :: TlsConfig
mkTlsConfig = TlsConfig'{insecureSkipVerification = Core.Nothing}

-- | Specifies whether or not API Gateway skips verification that the certificate for an integration endpoint is issued by a <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority> . This isn’t recommended, but it enables you to use certificates that are signed by private certificate authorities, or certificates that are self-signed. If enabled, API Gateway still performs basic certificate validation, which includes checking the certificate's expiration date, hostname, and presence of a root certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@ integrations.
--
-- /Note:/ Consider using 'insecureSkipVerification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInsecureSkipVerification :: Lens.Lens' TlsConfig (Core.Maybe Core.Bool)
tcInsecureSkipVerification = Lens.field @"insecureSkipVerification"
{-# INLINEABLE tcInsecureSkipVerification #-}
{-# DEPRECATED insecureSkipVerification "Use generic-lens or generic-optics with 'insecureSkipVerification' instead"  #-}

instance Core.FromJSON TlsConfig where
        toJSON TlsConfig{..}
          = Core.object
              (Core.catMaybes
                 [("insecureSkipVerification" Core..=) Core.<$>
                    insecureSkipVerification])

instance Core.FromJSON TlsConfig where
        parseJSON
          = Core.withObject "TlsConfig" Core.$
              \ x -> TlsConfig' Core.<$> (x Core..:? "insecureSkipVerification")
