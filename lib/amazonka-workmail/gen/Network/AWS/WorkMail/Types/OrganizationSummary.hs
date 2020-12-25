{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.OrganizationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.OrganizationSummary
  ( OrganizationSummary (..),

    -- * Smart constructor
    mkOrganizationSummary,

    -- * Lenses
    osAlias,
    osDefaultMailDomain,
    osErrorMessage,
    osOrganizationId,
    osState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.DefaultMailDomain as Types
import qualified Network.AWS.WorkMail.Types.ErrorMessage as Types
import qualified Network.AWS.WorkMail.Types.OrganizationId as Types
import qualified Network.AWS.WorkMail.Types.OrganizationName as Types
import qualified Network.AWS.WorkMail.Types.State as Types

-- | The representation of an organization.
--
-- /See:/ 'mkOrganizationSummary' smart constructor.
data OrganizationSummary = OrganizationSummary'
  { -- | The alias associated with the organization.
    alias :: Core.Maybe Types.OrganizationName,
    -- | The default email domain associated with the organization.
    defaultMailDomain :: Core.Maybe Types.DefaultMailDomain,
    -- | The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The identifier associated with the organization.
    organizationId :: Core.Maybe Types.OrganizationId,
    -- | The state associated with the organization.
    state :: Core.Maybe Types.State
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationSummary' value with any optional fields omitted.
mkOrganizationSummary ::
  OrganizationSummary
mkOrganizationSummary =
  OrganizationSummary'
    { alias = Core.Nothing,
      defaultMailDomain = Core.Nothing,
      errorMessage = Core.Nothing,
      organizationId = Core.Nothing,
      state = Core.Nothing
    }

-- | The alias associated with the organization.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAlias :: Lens.Lens' OrganizationSummary (Core.Maybe Types.OrganizationName)
osAlias = Lens.field @"alias"
{-# DEPRECATED osAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The default email domain associated with the organization.
--
-- /Note:/ Consider using 'defaultMailDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDefaultMailDomain :: Lens.Lens' OrganizationSummary (Core.Maybe Types.DefaultMailDomain)
osDefaultMailDomain = Lens.field @"defaultMailDomain"
{-# DEPRECATED osDefaultMailDomain "Use generic-lens or generic-optics with 'defaultMailDomain' instead." #-}

-- | The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osErrorMessage :: Lens.Lens' OrganizationSummary (Core.Maybe Types.ErrorMessage)
osErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED osErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The identifier associated with the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOrganizationId :: Lens.Lens' OrganizationSummary (Core.Maybe Types.OrganizationId)
osOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED osOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The state associated with the organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osState :: Lens.Lens' OrganizationSummary (Core.Maybe Types.State)
osState = Lens.field @"state"
{-# DEPRECATED osState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON OrganizationSummary where
  parseJSON =
    Core.withObject "OrganizationSummary" Core.$
      \x ->
        OrganizationSummary'
          Core.<$> (x Core..:? "Alias")
          Core.<*> (x Core..:? "DefaultMailDomain")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "OrganizationId")
          Core.<*> (x Core..:? "State")
