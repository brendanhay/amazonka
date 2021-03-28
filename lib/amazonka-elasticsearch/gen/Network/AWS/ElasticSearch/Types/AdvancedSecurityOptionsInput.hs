{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
  ( AdvancedSecurityOptionsInput (..)
  -- * Smart constructor
  , mkAdvancedSecurityOptionsInput
  -- * Lenses
  , asoiEnabled
  , asoiInternalUserDatabaseEnabled
  , asoiMasterUserOptions
  , asoiSAMLOptions
  ) where

import qualified Network.AWS.ElasticSearch.Types.MasterUserOptions as Types
import qualified Network.AWS.ElasticSearch.Types.SAMLOptionsInput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the advanced security configuration: whether advanced security is enabled, whether the internal database option is enabled, master username and password (if internal database is enabled), and master user ARN (if IAM is enabled).
--
-- /See:/ 'mkAdvancedSecurityOptionsInput' smart constructor.
data AdvancedSecurityOptionsInput = AdvancedSecurityOptionsInput'
  { enabled :: Core.Maybe Core.Bool
    -- ^ True if advanced security is enabled.
  , internalUserDatabaseEnabled :: Core.Maybe Core.Bool
    -- ^ True if the internal user database is enabled.
  , masterUserOptions :: Core.Maybe Types.MasterUserOptions
    -- ^ Credentials for the master user: username and password, ARN, or both.
  , sAMLOptions :: Core.Maybe Types.SAMLOptionsInput
    -- ^ Specifies the SAML application configuration for the domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdvancedSecurityOptionsInput' value with any optional fields omitted.
mkAdvancedSecurityOptionsInput
    :: AdvancedSecurityOptionsInput
mkAdvancedSecurityOptionsInput
  = AdvancedSecurityOptionsInput'{enabled = Core.Nothing,
                                  internalUserDatabaseEnabled = Core.Nothing,
                                  masterUserOptions = Core.Nothing, sAMLOptions = Core.Nothing}

-- | True if advanced security is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe Core.Bool)
asoiEnabled = Lens.field @"enabled"
{-# INLINEABLE asoiEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | True if the internal user database is enabled.
--
-- /Note:/ Consider using 'internalUserDatabaseEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiInternalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe Core.Bool)
asoiInternalUserDatabaseEnabled = Lens.field @"internalUserDatabaseEnabled"
{-# INLINEABLE asoiInternalUserDatabaseEnabled #-}
{-# DEPRECATED internalUserDatabaseEnabled "Use generic-lens or generic-optics with 'internalUserDatabaseEnabled' instead"  #-}

-- | Credentials for the master user: username and password, ARN, or both.
--
-- /Note:/ Consider using 'masterUserOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiMasterUserOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe Types.MasterUserOptions)
asoiMasterUserOptions = Lens.field @"masterUserOptions"
{-# INLINEABLE asoiMasterUserOptions #-}
{-# DEPRECATED masterUserOptions "Use generic-lens or generic-optics with 'masterUserOptions' instead"  #-}

-- | Specifies the SAML application configuration for the domain.
--
-- /Note:/ Consider using 'sAMLOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiSAMLOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe Types.SAMLOptionsInput)
asoiSAMLOptions = Lens.field @"sAMLOptions"
{-# INLINEABLE asoiSAMLOptions #-}
{-# DEPRECATED sAMLOptions "Use generic-lens or generic-optics with 'sAMLOptions' instead"  #-}

instance Core.FromJSON AdvancedSecurityOptionsInput where
        toJSON AdvancedSecurityOptionsInput{..}
          = Core.object
              (Core.catMaybes
                 [("Enabled" Core..=) Core.<$> enabled,
                  ("InternalUserDatabaseEnabled" Core..=) Core.<$>
                    internalUserDatabaseEnabled,
                  ("MasterUserOptions" Core..=) Core.<$> masterUserOptions,
                  ("SAMLOptions" Core..=) Core.<$> sAMLOptions])
