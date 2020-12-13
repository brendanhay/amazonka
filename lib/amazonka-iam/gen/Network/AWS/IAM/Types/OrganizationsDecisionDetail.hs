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
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the effect that Organizations has on a policy simulation.
--
-- /See:/ 'mkOrganizationsDecisionDetail' smart constructor.
newtype OrganizationsDecisionDetail = OrganizationsDecisionDetail'
  { -- | Specifies whether the simulated operation is allowed by the Organizations service control policies that impact the simulated user's account.
    allowedByOrganizations :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationsDecisionDetail' with the minimum fields required to make a request.
--
-- * 'allowedByOrganizations' - Specifies whether the simulated operation is allowed by the Organizations service control policies that impact the simulated user's account.
mkOrganizationsDecisionDetail ::
  OrganizationsDecisionDetail
mkOrganizationsDecisionDetail =
  OrganizationsDecisionDetail'
    { allowedByOrganizations =
        Lude.Nothing
    }

-- | Specifies whether the simulated operation is allowed by the Organizations service control policies that impact the simulated user's account.
--
-- /Note:/ Consider using 'allowedByOrganizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddAllowedByOrganizations :: Lens.Lens' OrganizationsDecisionDetail (Lude.Maybe Lude.Bool)
oddAllowedByOrganizations = Lens.lens (allowedByOrganizations :: OrganizationsDecisionDetail -> Lude.Maybe Lude.Bool) (\s a -> s {allowedByOrganizations = a} :: OrganizationsDecisionDetail)
{-# DEPRECATED oddAllowedByOrganizations "Use generic-lens or generic-optics with 'allowedByOrganizations' instead." #-}

instance Lude.FromXML OrganizationsDecisionDetail where
  parseXML x =
    OrganizationsDecisionDetail'
      Lude.<$> (x Lude..@? "AllowedByOrganizations")
