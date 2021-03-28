{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.OrganizationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.OrganizationSummary
  ( OrganizationSummary (..)
  -- * Smart constructor
  , mkOrganizationSummary
  -- * Lenses
  , osAlias
  , osDefaultMailDomain
  , osErrorMessage
  , osOrganizationId
  , osState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.DefaultMailDomain as Types
import qualified Network.AWS.WorkMail.Types.OrganizationId as Types
import qualified Network.AWS.WorkMail.Types.OrganizationName as Types

-- | The representation of an organization.
--
-- /See:/ 'mkOrganizationSummary' smart constructor.
data OrganizationSummary = OrganizationSummary'
  { alias :: Core.Maybe Types.OrganizationName
    -- ^ The alias associated with the organization.
  , defaultMailDomain :: Core.Maybe Types.DefaultMailDomain
    -- ^ The default email domain associated with the organization.
  , errorMessage :: Core.Maybe Core.Text
    -- ^ The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
  , organizationId :: Core.Maybe Types.OrganizationId
    -- ^ The identifier associated with the organization.
  , state :: Core.Maybe Core.Text
    -- ^ The state associated with the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationSummary' value with any optional fields omitted.
mkOrganizationSummary
    :: OrganizationSummary
mkOrganizationSummary
  = OrganizationSummary'{alias = Core.Nothing,
                         defaultMailDomain = Core.Nothing, errorMessage = Core.Nothing,
                         organizationId = Core.Nothing, state = Core.Nothing}

-- | The alias associated with the organization.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAlias :: Lens.Lens' OrganizationSummary (Core.Maybe Types.OrganizationName)
osAlias = Lens.field @"alias"
{-# INLINEABLE osAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

-- | The default email domain associated with the organization.
--
-- /Note:/ Consider using 'defaultMailDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDefaultMailDomain :: Lens.Lens' OrganizationSummary (Core.Maybe Types.DefaultMailDomain)
osDefaultMailDomain = Lens.field @"defaultMailDomain"
{-# INLINEABLE osDefaultMailDomain #-}
{-# DEPRECATED defaultMailDomain "Use generic-lens or generic-optics with 'defaultMailDomain' instead"  #-}

-- | The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osErrorMessage :: Lens.Lens' OrganizationSummary (Core.Maybe Core.Text)
osErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE osErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The identifier associated with the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOrganizationId :: Lens.Lens' OrganizationSummary (Core.Maybe Types.OrganizationId)
osOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE osOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The state associated with the organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osState :: Lens.Lens' OrganizationSummary (Core.Maybe Core.Text)
osState = Lens.field @"state"
{-# INLINEABLE osState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON OrganizationSummary where
        parseJSON
          = Core.withObject "OrganizationSummary" Core.$
              \ x ->
                OrganizationSummary' Core.<$>
                  (x Core..:? "Alias") Core.<*> x Core..:? "DefaultMailDomain"
                    Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "OrganizationId"
                    Core.<*> x Core..:? "State"
