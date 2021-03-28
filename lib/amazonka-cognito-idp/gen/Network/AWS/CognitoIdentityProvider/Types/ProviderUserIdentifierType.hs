{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
  ( ProviderUserIdentifierType (..)
  -- * Smart constructor
  , mkProviderUserIdentifierType
  -- * Lenses
  , puitProviderAttributeName
  , puitProviderAttributeValue
  , puitProviderName
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.ProviderAttributeName as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ProviderAttributeValue as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ProviderName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for information about an identity provider for a user pool.
--
-- /See:/ 'mkProviderUserIdentifierType' smart constructor.
data ProviderUserIdentifierType = ProviderUserIdentifierType'
  { providerAttributeName :: Core.Maybe Types.ProviderAttributeName
    -- ^ The name of the provider attribute to link to, for example, @NameID@ .
  , providerAttributeValue :: Core.Maybe Types.ProviderAttributeValue
    -- ^ The value of the provider attribute to link to, for example, @xxxxx_account@ .
  , providerName :: Core.Maybe Types.ProviderName
    -- ^ The name of the provider, for example, Facebook, Google, or Login with Amazon.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProviderUserIdentifierType' value with any optional fields omitted.
mkProviderUserIdentifierType
    :: ProviderUserIdentifierType
mkProviderUserIdentifierType
  = ProviderUserIdentifierType'{providerAttributeName = Core.Nothing,
                                providerAttributeValue = Core.Nothing, providerName = Core.Nothing}

-- | The name of the provider attribute to link to, for example, @NameID@ .
--
-- /Note:/ Consider using 'providerAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puitProviderAttributeName :: Lens.Lens' ProviderUserIdentifierType (Core.Maybe Types.ProviderAttributeName)
puitProviderAttributeName = Lens.field @"providerAttributeName"
{-# INLINEABLE puitProviderAttributeName #-}
{-# DEPRECATED providerAttributeName "Use generic-lens or generic-optics with 'providerAttributeName' instead"  #-}

-- | The value of the provider attribute to link to, for example, @xxxxx_account@ .
--
-- /Note:/ Consider using 'providerAttributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puitProviderAttributeValue :: Lens.Lens' ProviderUserIdentifierType (Core.Maybe Types.ProviderAttributeValue)
puitProviderAttributeValue = Lens.field @"providerAttributeValue"
{-# INLINEABLE puitProviderAttributeValue #-}
{-# DEPRECATED providerAttributeValue "Use generic-lens or generic-optics with 'providerAttributeValue' instead"  #-}

-- | The name of the provider, for example, Facebook, Google, or Login with Amazon.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puitProviderName :: Lens.Lens' ProviderUserIdentifierType (Core.Maybe Types.ProviderName)
puitProviderName = Lens.field @"providerName"
{-# INLINEABLE puitProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

instance Core.FromJSON ProviderUserIdentifierType where
        toJSON ProviderUserIdentifierType{..}
          = Core.object
              (Core.catMaybes
                 [("ProviderAttributeName" Core..=) Core.<$> providerAttributeName,
                  ("ProviderAttributeValue" Core..=) Core.<$> providerAttributeValue,
                  ("ProviderName" Core..=) Core.<$> providerName])
