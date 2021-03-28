{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
  ( ProviderDescription (..)
  -- * Smart constructor
  , mkProviderDescription
  -- * Lenses
  , pdCreationDate
  , pdLastModifiedDate
  , pdProviderName
  , pdProviderType
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ProviderName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for identity provider details.
--
-- /See:/ 'mkProviderDescription' smart constructor.
data ProviderDescription = ProviderDescription'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the provider was added to the user pool.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the provider was last modified.
  , providerName :: Core.Maybe Types.ProviderName
    -- ^ The identity provider name.
  , providerType :: Core.Maybe Types.IdentityProviderTypeType
    -- ^ The identity provider type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProviderDescription' value with any optional fields omitted.
mkProviderDescription
    :: ProviderDescription
mkProviderDescription
  = ProviderDescription'{creationDate = Core.Nothing,
                         lastModifiedDate = Core.Nothing, providerName = Core.Nothing,
                         providerType = Core.Nothing}

-- | The date the provider was added to the user pool.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreationDate :: Lens.Lens' ProviderDescription (Core.Maybe Core.NominalDiffTime)
pdCreationDate = Lens.field @"creationDate"
{-# INLINEABLE pdCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date the provider was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdLastModifiedDate :: Lens.Lens' ProviderDescription (Core.Maybe Core.NominalDiffTime)
pdLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE pdLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProviderName :: Lens.Lens' ProviderDescription (Core.Maybe Types.ProviderName)
pdProviderName = Lens.field @"providerName"
{-# INLINEABLE pdProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

-- | The identity provider type.
--
-- /Note:/ Consider using 'providerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProviderType :: Lens.Lens' ProviderDescription (Core.Maybe Types.IdentityProviderTypeType)
pdProviderType = Lens.field @"providerType"
{-# INLINEABLE pdProviderType #-}
{-# DEPRECATED providerType "Use generic-lens or generic-optics with 'providerType' instead"  #-}

instance Core.FromJSON ProviderDescription where
        parseJSON
          = Core.withObject "ProviderDescription" Core.$
              \ x ->
                ProviderDescription' Core.<$>
                  (x Core..:? "CreationDate") Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "ProviderName"
                    Core.<*> x Core..:? "ProviderType"
