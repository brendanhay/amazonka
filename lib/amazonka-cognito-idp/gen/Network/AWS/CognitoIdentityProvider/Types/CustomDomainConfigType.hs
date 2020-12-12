{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
  ( CustomDomainConfigType (..),

    -- * Smart constructor
    mkCustomDomainConfigType,

    -- * Lenses
    cdctCertificateARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
-- /See:/ 'mkCustomDomainConfigType' smart constructor.
newtype CustomDomainConfigType = CustomDomainConfigType'
  { certificateARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomDomainConfigType' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL certificate. You use this certificate for the subdomain of your custom domain.
mkCustomDomainConfigType ::
  -- | 'certificateARN'
  Lude.Text ->
  CustomDomainConfigType
mkCustomDomainConfigType pCertificateARN_ =
  CustomDomainConfigType' {certificateARN = pCertificateARN_}

-- | The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL certificate. You use this certificate for the subdomain of your custom domain.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdctCertificateARN :: Lens.Lens' CustomDomainConfigType Lude.Text
cdctCertificateARN = Lens.lens (certificateARN :: CustomDomainConfigType -> Lude.Text) (\s a -> s {certificateARN = a} :: CustomDomainConfigType)
{-# DEPRECATED cdctCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.FromJSON CustomDomainConfigType where
  parseJSON =
    Lude.withObject
      "CustomDomainConfigType"
      ( \x ->
          CustomDomainConfigType' Lude.<$> (x Lude..: "CertificateArn")
      )

instance Lude.ToJSON CustomDomainConfigType where
  toJSON CustomDomainConfigType' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateArn" Lude..= certificateARN)]
      )
