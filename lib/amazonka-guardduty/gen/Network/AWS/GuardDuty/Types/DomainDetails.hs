{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DomainDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.DomainDetails
  ( DomainDetails (..)
  -- * Smart constructor
  , mkDomainDetails
  -- * Lenses
  , ddDomain
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the domain.
--
-- /See:/ 'mkDomainDetails' smart constructor.
newtype DomainDetails = DomainDetails'
  { domain :: Core.Maybe Core.Text
    -- ^ The domain information for the AWS API call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DomainDetails' value with any optional fields omitted.
mkDomainDetails
    :: DomainDetails
mkDomainDetails = DomainDetails'{domain = Core.Nothing}

-- | The domain information for the AWS API call.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomain :: Lens.Lens' DomainDetails (Core.Maybe Core.Text)
ddDomain = Lens.field @"domain"
{-# INLINEABLE ddDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

instance Core.FromJSON DomainDetails where
        parseJSON
          = Core.withObject "DomainDetails" Core.$
              \ x -> DomainDetails' Core.<$> (x Core..:? "domain")
