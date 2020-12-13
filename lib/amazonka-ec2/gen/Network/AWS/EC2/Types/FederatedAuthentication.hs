{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FederatedAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FederatedAuthentication
  ( FederatedAuthentication (..),

    -- * Smart constructor
    mkFederatedAuthentication,

    -- * Lenses
    faSamlProviderARN,
    faSelfServiceSamlProviderARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the IAM SAML identity providers used for federated authentication.
--
-- /See:/ 'mkFederatedAuthentication' smart constructor.
data FederatedAuthentication = FederatedAuthentication'
  { -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
    samlProviderARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
    selfServiceSamlProviderARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FederatedAuthentication' with the minimum fields required to make a request.
--
-- * 'samlProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider.
-- * 'selfServiceSamlProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
mkFederatedAuthentication ::
  FederatedAuthentication
mkFederatedAuthentication =
  FederatedAuthentication'
    { samlProviderARN = Lude.Nothing,
      selfServiceSamlProviderARN = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- /Note:/ Consider using 'samlProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faSamlProviderARN :: Lens.Lens' FederatedAuthentication (Lude.Maybe Lude.Text)
faSamlProviderARN = Lens.lens (samlProviderARN :: FederatedAuthentication -> Lude.Maybe Lude.Text) (\s a -> s {samlProviderARN = a} :: FederatedAuthentication)
{-# DEPRECATED faSamlProviderARN "Use generic-lens or generic-optics with 'samlProviderARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
--
-- /Note:/ Consider using 'selfServiceSamlProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faSelfServiceSamlProviderARN :: Lens.Lens' FederatedAuthentication (Lude.Maybe Lude.Text)
faSelfServiceSamlProviderARN = Lens.lens (selfServiceSamlProviderARN :: FederatedAuthentication -> Lude.Maybe Lude.Text) (\s a -> s {selfServiceSamlProviderARN = a} :: FederatedAuthentication)
{-# DEPRECATED faSelfServiceSamlProviderARN "Use generic-lens or generic-optics with 'selfServiceSamlProviderARN' instead." #-}

instance Lude.FromXML FederatedAuthentication where
  parseXML x =
    FederatedAuthentication'
      Lude.<$> (x Lude..@? "samlProviderArn")
      Lude.<*> (x Lude..@? "selfServiceSamlProviderArn")
