{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainTransferability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainTransferability
  ( DomainTransferability (..),

    -- * Smart constructor
    mkDomainTransferability,

    -- * Lenses
    dtTransferable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53Domains.Types.Transferable as Types

-- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
--
-- /See:/ 'mkDomainTransferability' smart constructor.
newtype DomainTransferability = DomainTransferability'
  { transferable :: Core.Maybe Types.Transferable
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DomainTransferability' value with any optional fields omitted.
mkDomainTransferability ::
  DomainTransferability
mkDomainTransferability =
  DomainTransferability' {transferable = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transferable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTransferable :: Lens.Lens' DomainTransferability (Core.Maybe Types.Transferable)
dtTransferable = Lens.field @"transferable"
{-# DEPRECATED dtTransferable "Use generic-lens or generic-optics with 'transferable' instead." #-}

instance Core.FromJSON DomainTransferability where
  parseJSON =
    Core.withObject "DomainTransferability" Core.$
      \x -> DomainTransferability' Core.<$> (x Core..:? "Transferable")
