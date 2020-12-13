{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
  ( ProviderUserIdentifierType (..),

    -- * Smart constructor
    mkProviderUserIdentifierType,

    -- * Lenses
    puitProviderAttributeValue,
    puitProviderAttributeName,
    puitProviderName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for information about an identity provider for a user pool.
--
-- /See:/ 'mkProviderUserIdentifierType' smart constructor.
data ProviderUserIdentifierType = ProviderUserIdentifierType'
  { -- | The value of the provider attribute to link to, for example, @xxxxx_account@ .
    providerAttributeValue :: Lude.Maybe Lude.Text,
    -- | The name of the provider attribute to link to, for example, @NameID@ .
    providerAttributeName :: Lude.Maybe Lude.Text,
    -- | The name of the provider, for example, Facebook, Google, or Login with Amazon.
    providerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProviderUserIdentifierType' with the minimum fields required to make a request.
--
-- * 'providerAttributeValue' - The value of the provider attribute to link to, for example, @xxxxx_account@ .
-- * 'providerAttributeName' - The name of the provider attribute to link to, for example, @NameID@ .
-- * 'providerName' - The name of the provider, for example, Facebook, Google, or Login with Amazon.
mkProviderUserIdentifierType ::
  ProviderUserIdentifierType
mkProviderUserIdentifierType =
  ProviderUserIdentifierType'
    { providerAttributeValue =
        Lude.Nothing,
      providerAttributeName = Lude.Nothing,
      providerName = Lude.Nothing
    }

-- | The value of the provider attribute to link to, for example, @xxxxx_account@ .
--
-- /Note:/ Consider using 'providerAttributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puitProviderAttributeValue :: Lens.Lens' ProviderUserIdentifierType (Lude.Maybe Lude.Text)
puitProviderAttributeValue = Lens.lens (providerAttributeValue :: ProviderUserIdentifierType -> Lude.Maybe Lude.Text) (\s a -> s {providerAttributeValue = a} :: ProviderUserIdentifierType)
{-# DEPRECATED puitProviderAttributeValue "Use generic-lens or generic-optics with 'providerAttributeValue' instead." #-}

-- | The name of the provider attribute to link to, for example, @NameID@ .
--
-- /Note:/ Consider using 'providerAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puitProviderAttributeName :: Lens.Lens' ProviderUserIdentifierType (Lude.Maybe Lude.Text)
puitProviderAttributeName = Lens.lens (providerAttributeName :: ProviderUserIdentifierType -> Lude.Maybe Lude.Text) (\s a -> s {providerAttributeName = a} :: ProviderUserIdentifierType)
{-# DEPRECATED puitProviderAttributeName "Use generic-lens or generic-optics with 'providerAttributeName' instead." #-}

-- | The name of the provider, for example, Facebook, Google, or Login with Amazon.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puitProviderName :: Lens.Lens' ProviderUserIdentifierType (Lude.Maybe Lude.Text)
puitProviderName = Lens.lens (providerName :: ProviderUserIdentifierType -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: ProviderUserIdentifierType)
{-# DEPRECATED puitProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Lude.ToJSON ProviderUserIdentifierType where
  toJSON ProviderUserIdentifierType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProviderAttributeValue" Lude..=)
              Lude.<$> providerAttributeValue,
            ("ProviderAttributeName" Lude..=) Lude.<$> providerAttributeName,
            ("ProviderName" Lude..=) Lude.<$> providerName
          ]
      )
