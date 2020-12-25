{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.OrganizationsDecisionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.OrganizationsDecisionDetail
  ( OrganizationsDecisionDetail (..),

    -- * Smart constructor
    mkOrganizationsDecisionDetail,

    -- * Lenses
    oddAllowedByOrganizations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the effect that Organizations has on a policy simulation.
--
-- /See:/ 'mkOrganizationsDecisionDetail' smart constructor.
newtype OrganizationsDecisionDetail = OrganizationsDecisionDetail'
  { -- | Specifies whether the simulated operation is allowed by the Organizations service control policies that impact the simulated user's account.
    allowedByOrganizations :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationsDecisionDetail' value with any optional fields omitted.
mkOrganizationsDecisionDetail ::
  OrganizationsDecisionDetail
mkOrganizationsDecisionDetail =
  OrganizationsDecisionDetail'
    { allowedByOrganizations =
        Core.Nothing
    }

-- | Specifies whether the simulated operation is allowed by the Organizations service control policies that impact the simulated user's account.
--
-- /Note:/ Consider using 'allowedByOrganizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddAllowedByOrganizations :: Lens.Lens' OrganizationsDecisionDetail (Core.Maybe Core.Bool)
oddAllowedByOrganizations = Lens.field @"allowedByOrganizations"
{-# DEPRECATED oddAllowedByOrganizations "Use generic-lens or generic-optics with 'allowedByOrganizations' instead." #-}

instance Core.FromXML OrganizationsDecisionDetail where
  parseXML x =
    OrganizationsDecisionDetail'
      Core.<$> (x Core..@? "AllowedByOrganizations")
