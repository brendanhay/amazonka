{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
  ( AdvancedSecurityOptions (..)
  -- * Smart constructor
  , mkAdvancedSecurityOptions
  -- * Lenses
  , asoEnabled
  , asoInternalUserDatabaseEnabled
  , asoSAMLOptions
  ) where

import qualified Network.AWS.ElasticSearch.Types.SAMLOptionsOutput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the advanced security configuration: whether advanced security is enabled, whether the internal database option is enabled.
--
-- /See:/ 'mkAdvancedSecurityOptions' smart constructor.
data AdvancedSecurityOptions = AdvancedSecurityOptions'
  { enabled :: Core.Maybe Core.Bool
    -- ^ True if advanced security is enabled.
  , internalUserDatabaseEnabled :: Core.Maybe Core.Bool
    -- ^ True if the internal user database is enabled.
  , sAMLOptions :: Core.Maybe Types.SAMLOptionsOutput
    -- ^ Describes the SAML application configured for a domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdvancedSecurityOptions' value with any optional fields omitted.
mkAdvancedSecurityOptions
    :: AdvancedSecurityOptions
mkAdvancedSecurityOptions
  = AdvancedSecurityOptions'{enabled = Core.Nothing,
                             internalUserDatabaseEnabled = Core.Nothing,
                             sAMLOptions = Core.Nothing}

-- | True if advanced security is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoEnabled :: Lens.Lens' AdvancedSecurityOptions (Core.Maybe Core.Bool)
asoEnabled = Lens.field @"enabled"
{-# INLINEABLE asoEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | True if the internal user database is enabled.
--
-- /Note:/ Consider using 'internalUserDatabaseEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoInternalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptions (Core.Maybe Core.Bool)
asoInternalUserDatabaseEnabled = Lens.field @"internalUserDatabaseEnabled"
{-# INLINEABLE asoInternalUserDatabaseEnabled #-}
{-# DEPRECATED internalUserDatabaseEnabled "Use generic-lens or generic-optics with 'internalUserDatabaseEnabled' instead"  #-}

-- | Describes the SAML application configured for a domain.
--
-- /Note:/ Consider using 'sAMLOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoSAMLOptions :: Lens.Lens' AdvancedSecurityOptions (Core.Maybe Types.SAMLOptionsOutput)
asoSAMLOptions = Lens.field @"sAMLOptions"
{-# INLINEABLE asoSAMLOptions #-}
{-# DEPRECATED sAMLOptions "Use generic-lens or generic-optics with 'sAMLOptions' instead"  #-}

instance Core.FromJSON AdvancedSecurityOptions where
        parseJSON
          = Core.withObject "AdvancedSecurityOptions" Core.$
              \ x ->
                AdvancedSecurityOptions' Core.<$>
                  (x Core..:? "Enabled") Core.<*>
                    x Core..:? "InternalUserDatabaseEnabled"
                    Core.<*> x Core..:? "SAMLOptions"
