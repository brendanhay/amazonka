{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
  ( OriginGroupFailoverCriteria (..),

    -- * Smart constructor
    mkOriginGroupFailoverCriteria,

    -- * Lenses
    ogfcStatusCodes,
  )
where

import qualified Network.AWS.CloudFront.Types.StatusCodes as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type that includes information about the failover criteria for an origin group, including the status codes for which CloudFront will failover from the primary origin to the second origin.
--
-- /See:/ 'mkOriginGroupFailoverCriteria' smart constructor.
newtype OriginGroupFailoverCriteria = OriginGroupFailoverCriteria'
  { -- | The status codes that, when returned from the primary origin, will trigger CloudFront to failover to the second origin.
    statusCodes :: Types.StatusCodes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OriginGroupFailoverCriteria' value with any optional fields omitted.
mkOriginGroupFailoverCriteria ::
  -- | 'statusCodes'
  Types.StatusCodes ->
  OriginGroupFailoverCriteria
mkOriginGroupFailoverCriteria statusCodes =
  OriginGroupFailoverCriteria' {statusCodes}

-- | The status codes that, when returned from the primary origin, will trigger CloudFront to failover to the second origin.
--
-- /Note:/ Consider using 'statusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogfcStatusCodes :: Lens.Lens' OriginGroupFailoverCriteria Types.StatusCodes
ogfcStatusCodes = Lens.field @"statusCodes"
{-# DEPRECATED ogfcStatusCodes "Use generic-lens or generic-optics with 'statusCodes' instead." #-}

instance Core.ToXML OriginGroupFailoverCriteria where
  toXML OriginGroupFailoverCriteria {..} =
    Core.toXMLNode "StatusCodes" statusCodes

instance Core.FromXML OriginGroupFailoverCriteria where
  parseXML x =
    OriginGroupFailoverCriteria' Core.<$> (x Core..@ "StatusCodes")
