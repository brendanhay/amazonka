{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrincipalIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrincipalIdFormat
  ( PrincipalIdFormat (..)
  -- * Smart constructor
  , mkPrincipalIdFormat
  -- * Lenses
  , pifArn
  , pifStatuses
  ) where

import qualified Network.AWS.EC2.Types.IdFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | PrincipalIdFormat description
--
-- /See:/ 'mkPrincipalIdFormat' smart constructor.
data PrincipalIdFormat = PrincipalIdFormat'
  { arn :: Core.Maybe Core.Text
    -- ^ PrincipalIdFormatARN description
  , statuses :: Core.Maybe [Types.IdFormat]
    -- ^ PrincipalIdFormatStatuses description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PrincipalIdFormat' value with any optional fields omitted.
mkPrincipalIdFormat
    :: PrincipalIdFormat
mkPrincipalIdFormat
  = PrincipalIdFormat'{arn = Core.Nothing, statuses = Core.Nothing}

-- | PrincipalIdFormatARN description
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pifArn :: Lens.Lens' PrincipalIdFormat (Core.Maybe Core.Text)
pifArn = Lens.field @"arn"
{-# INLINEABLE pifArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | PrincipalIdFormatStatuses description
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pifStatuses :: Lens.Lens' PrincipalIdFormat (Core.Maybe [Types.IdFormat])
pifStatuses = Lens.field @"statuses"
{-# INLINEABLE pifStatuses #-}
{-# DEPRECATED statuses "Use generic-lens or generic-optics with 'statuses' instead"  #-}

instance Core.FromXML PrincipalIdFormat where
        parseXML x
          = PrincipalIdFormat' Core.<$>
              (x Core..@? "arn") Core.<*>
                x Core..@? "statusSet" Core..<@> Core.parseXMLList "item"
