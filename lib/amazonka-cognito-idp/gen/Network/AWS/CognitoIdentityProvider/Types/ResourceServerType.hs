{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
  ( ResourceServerType (..)
  -- * Smart constructor
  , mkResourceServerType
  -- * Lenses
  , rstIdentifier
  , rstName
  , rstScopes
  , rstUserPoolId
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.ResourceServerIdentifierType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ResourceServerNameType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for information about a resource server for a user pool.
--
-- /See:/ 'mkResourceServerType' smart constructor.
data ResourceServerType = ResourceServerType'
  { identifier :: Core.Maybe Types.ResourceServerIdentifierType
    -- ^ The identifier for the resource server.
  , name :: Core.Maybe Types.ResourceServerNameType
    -- ^ The name of the resource server.
  , scopes :: Core.Maybe [Types.ResourceServerScopeType]
    -- ^ A list of scopes that are defined for the resource server.
  , userPoolId :: Core.Maybe Types.UserPoolId
    -- ^ The user pool ID for the user pool that hosts the resource server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceServerType' value with any optional fields omitted.
mkResourceServerType
    :: ResourceServerType
mkResourceServerType
  = ResourceServerType'{identifier = Core.Nothing,
                        name = Core.Nothing, scopes = Core.Nothing,
                        userPoolId = Core.Nothing}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstIdentifier :: Lens.Lens' ResourceServerType (Core.Maybe Types.ResourceServerIdentifierType)
rstIdentifier = Lens.field @"identifier"
{-# INLINEABLE rstIdentifier #-}
{-# DEPRECATED identifier "Use generic-lens or generic-optics with 'identifier' instead"  #-}

-- | The name of the resource server.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstName :: Lens.Lens' ResourceServerType (Core.Maybe Types.ResourceServerNameType)
rstName = Lens.field @"name"
{-# INLINEABLE rstName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of scopes that are defined for the resource server.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstScopes :: Lens.Lens' ResourceServerType (Core.Maybe [Types.ResourceServerScopeType])
rstScopes = Lens.field @"scopes"
{-# INLINEABLE rstScopes #-}
{-# DEPRECATED scopes "Use generic-lens or generic-optics with 'scopes' instead"  #-}

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstUserPoolId :: Lens.Lens' ResourceServerType (Core.Maybe Types.UserPoolId)
rstUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE rstUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.FromJSON ResourceServerType where
        parseJSON
          = Core.withObject "ResourceServerType" Core.$
              \ x ->
                ResourceServerType' Core.<$>
                  (x Core..:? "Identifier") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "Scopes"
                    Core.<*> x Core..:? "UserPoolId"
