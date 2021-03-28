{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
  ( ResourceServerScopeType (..)
  -- * Smart constructor
  , mkResourceServerScopeType
  -- * Lenses
  , rsstScopeName
  , rsstScopeDescription
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeDescriptionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeNameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A resource server scope.
--
-- /See:/ 'mkResourceServerScopeType' smart constructor.
data ResourceServerScopeType = ResourceServerScopeType'
  { scopeName :: Types.ResourceServerScopeNameType
    -- ^ The name of the scope.
  , scopeDescription :: Types.ResourceServerScopeDescriptionType
    -- ^ A description of the scope.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceServerScopeType' value with any optional fields omitted.
mkResourceServerScopeType
    :: Types.ResourceServerScopeNameType -- ^ 'scopeName'
    -> Types.ResourceServerScopeDescriptionType -- ^ 'scopeDescription'
    -> ResourceServerScopeType
mkResourceServerScopeType scopeName scopeDescription
  = ResourceServerScopeType'{scopeName, scopeDescription}

-- | The name of the scope.
--
-- /Note:/ Consider using 'scopeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsstScopeName :: Lens.Lens' ResourceServerScopeType Types.ResourceServerScopeNameType
rsstScopeName = Lens.field @"scopeName"
{-# INLINEABLE rsstScopeName #-}
{-# DEPRECATED scopeName "Use generic-lens or generic-optics with 'scopeName' instead"  #-}

-- | A description of the scope.
--
-- /Note:/ Consider using 'scopeDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsstScopeDescription :: Lens.Lens' ResourceServerScopeType Types.ResourceServerScopeDescriptionType
rsstScopeDescription = Lens.field @"scopeDescription"
{-# INLINEABLE rsstScopeDescription #-}
{-# DEPRECATED scopeDescription "Use generic-lens or generic-optics with 'scopeDescription' instead"  #-}

instance Core.FromJSON ResourceServerScopeType where
        toJSON ResourceServerScopeType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ScopeName" Core..= scopeName),
                  Core.Just ("ScopeDescription" Core..= scopeDescription)])

instance Core.FromJSON ResourceServerScopeType where
        parseJSON
          = Core.withObject "ResourceServerScopeType" Core.$
              \ x ->
                ResourceServerScopeType' Core.<$>
                  (x Core..: "ScopeName") Core.<*> x Core..: "ScopeDescription"
