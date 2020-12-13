{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FederatedAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FederatedAuthenticationRequest
  ( FederatedAuthenticationRequest (..),

    -- * Smart constructor
    mkFederatedAuthenticationRequest,

    -- * Lenses
    farSAMLProviderARN,
    farSelfServiceSAMLProviderARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The IAM SAML identity provider used for federated authentication.
--
-- /See:/ 'mkFederatedAuthenticationRequest' smart constructor.
data FederatedAuthenticationRequest = FederatedAuthenticationRequest'
  { -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
    sAMLProviderARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
    selfServiceSAMLProviderARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FederatedAuthenticationRequest' with the minimum fields required to make a request.
--
-- * 'sAMLProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider.
-- * 'selfServiceSAMLProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
mkFederatedAuthenticationRequest ::
  FederatedAuthenticationRequest
mkFederatedAuthenticationRequest =
  FederatedAuthenticationRequest'
    { sAMLProviderARN = Lude.Nothing,
      selfServiceSAMLProviderARN = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- /Note:/ Consider using 'sAMLProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farSAMLProviderARN :: Lens.Lens' FederatedAuthenticationRequest (Lude.Maybe Lude.Text)
farSAMLProviderARN = Lens.lens (sAMLProviderARN :: FederatedAuthenticationRequest -> Lude.Maybe Lude.Text) (\s a -> s {sAMLProviderARN = a} :: FederatedAuthenticationRequest)
{-# DEPRECATED farSAMLProviderARN "Use generic-lens or generic-optics with 'sAMLProviderARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
--
-- /Note:/ Consider using 'selfServiceSAMLProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farSelfServiceSAMLProviderARN :: Lens.Lens' FederatedAuthenticationRequest (Lude.Maybe Lude.Text)
farSelfServiceSAMLProviderARN = Lens.lens (selfServiceSAMLProviderARN :: FederatedAuthenticationRequest -> Lude.Maybe Lude.Text) (\s a -> s {selfServiceSAMLProviderARN = a} :: FederatedAuthenticationRequest)
{-# DEPRECATED farSelfServiceSAMLProviderARN "Use generic-lens or generic-optics with 'selfServiceSAMLProviderARN' instead." #-}

instance Lude.ToQuery FederatedAuthenticationRequest where
  toQuery FederatedAuthenticationRequest' {..} =
    Lude.mconcat
      [ "SAMLProviderArn" Lude.=: sAMLProviderARN,
        "SelfServiceSAMLProviderArn" Lude.=: selfServiceSAMLProviderARN
      ]
