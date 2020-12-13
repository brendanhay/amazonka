{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
  ( ProviderDescription (..),

    -- * Smart constructor
    mkProviderDescription,

    -- * Lenses
    pdLastModifiedDate,
    pdProviderType,
    pdCreationDate,
    pdProviderName,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for identity provider details.
--
-- /See:/ 'mkProviderDescription' smart constructor.
data ProviderDescription = ProviderDescription'
  { -- | The date the provider was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The identity provider type.
    providerType :: Lude.Maybe IdentityProviderTypeType,
    -- | The date the provider was added to the user pool.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The identity provider name.
    providerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProviderDescription' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date the provider was last modified.
-- * 'providerType' - The identity provider type.
-- * 'creationDate' - The date the provider was added to the user pool.
-- * 'providerName' - The identity provider name.
mkProviderDescription ::
  ProviderDescription
mkProviderDescription =
  ProviderDescription'
    { lastModifiedDate = Lude.Nothing,
      providerType = Lude.Nothing,
      creationDate = Lude.Nothing,
      providerName = Lude.Nothing
    }

-- | The date the provider was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdLastModifiedDate :: Lens.Lens' ProviderDescription (Lude.Maybe Lude.Timestamp)
pdLastModifiedDate = Lens.lens (lastModifiedDate :: ProviderDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: ProviderDescription)
{-# DEPRECATED pdLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The identity provider type.
--
-- /Note:/ Consider using 'providerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProviderType :: Lens.Lens' ProviderDescription (Lude.Maybe IdentityProviderTypeType)
pdProviderType = Lens.lens (providerType :: ProviderDescription -> Lude.Maybe IdentityProviderTypeType) (\s a -> s {providerType = a} :: ProviderDescription)
{-# DEPRECATED pdProviderType "Use generic-lens or generic-optics with 'providerType' instead." #-}

-- | The date the provider was added to the user pool.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreationDate :: Lens.Lens' ProviderDescription (Lude.Maybe Lude.Timestamp)
pdCreationDate = Lens.lens (creationDate :: ProviderDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ProviderDescription)
{-# DEPRECATED pdCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProviderName :: Lens.Lens' ProviderDescription (Lude.Maybe Lude.Text)
pdProviderName = Lens.lens (providerName :: ProviderDescription -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: ProviderDescription)
{-# DEPRECATED pdProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Lude.FromJSON ProviderDescription where
  parseJSON =
    Lude.withObject
      "ProviderDescription"
      ( \x ->
          ProviderDescription'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "ProviderType")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "ProviderName")
      )
