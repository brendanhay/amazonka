-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.TLSConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.TLSConfig
  ( TLSConfig (..),

    -- * Smart constructor
    mkTLSConfig,

    -- * Lenses
    tcInsecureSkipVerification,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkTLSConfig' smart constructor.
newtype TLSConfig = TLSConfig'
  { insecureSkipVerification ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TLSConfig' with the minimum fields required to make a request.
--
-- * 'insecureSkipVerification' - Specifies whether or not API Gateway skips verification that the certificate for an integration endpoint is issued by a <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority> . This isn’t recommended, but it enables you to use certificates that are signed by private certificate authorities, or certificates that are self-signed. If enabled, API Gateway still performs basic certificate validation, which includes checking the certificate's expiration date, hostname, and presence of a root certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@ integrations.
mkTLSConfig ::
  TLSConfig
mkTLSConfig = TLSConfig' {insecureSkipVerification = Lude.Nothing}

-- | Specifies whether or not API Gateway skips verification that the certificate for an integration endpoint is issued by a <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority> . This isn’t recommended, but it enables you to use certificates that are signed by private certificate authorities, or certificates that are self-signed. If enabled, API Gateway still performs basic certificate validation, which includes checking the certificate's expiration date, hostname, and presence of a root certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@ integrations.
--
-- /Note:/ Consider using 'insecureSkipVerification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInsecureSkipVerification :: Lens.Lens' TLSConfig (Lude.Maybe Lude.Bool)
tcInsecureSkipVerification = Lens.lens (insecureSkipVerification :: TLSConfig -> Lude.Maybe Lude.Bool) (\s a -> s {insecureSkipVerification = a} :: TLSConfig)
{-# DEPRECATED tcInsecureSkipVerification "Use generic-lens or generic-optics with 'insecureSkipVerification' instead." #-}

instance Lude.FromJSON TLSConfig where
  parseJSON =
    Lude.withObject
      "TLSConfig"
      ( \x ->
          TLSConfig' Lude.<$> (x Lude..:? "insecureSkipVerification")
      )

instance Lude.ToJSON TLSConfig where
  toJSON TLSConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("insecureSkipVerification" Lude..=)
              Lude.<$> insecureSkipVerification
          ]
      )
