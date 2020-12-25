{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.StatusCodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.StatusCodes
  ( StatusCodes (..),

    -- * Smart constructor
    mkStatusCodes,

    -- * Lenses
    scStatus2xx,
    scStatus3xx,
    scStatus4xx,
    scStatus5xx,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html Status Code Definitions> .
--
-- /See:/ 'mkStatusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { -- | The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
    status2xx :: Core.Maybe Core.Int,
    -- | The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
    status3xx :: Core.Maybe Core.Int,
    -- | The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
    status4xx :: Core.Maybe Core.Int,
    -- | The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
    status5xx :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StatusCodes' value with any optional fields omitted.
mkStatusCodes ::
  StatusCodes
mkStatusCodes =
  StatusCodes'
    { status2xx = Core.Nothing,
      status3xx = Core.Nothing,
      status4xx = Core.Nothing,
      status5xx = Core.Nothing
    }

-- | The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
--
-- /Note:/ Consider using 'status2xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus2xx :: Lens.Lens' StatusCodes (Core.Maybe Core.Int)
scStatus2xx = Lens.field @"status2xx"
{-# DEPRECATED scStatus2xx "Use generic-lens or generic-optics with 'status2xx' instead." #-}

-- | The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
--
-- /Note:/ Consider using 'status3xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus3xx :: Lens.Lens' StatusCodes (Core.Maybe Core.Int)
scStatus3xx = Lens.field @"status3xx"
{-# DEPRECATED scStatus3xx "Use generic-lens or generic-optics with 'status3xx' instead." #-}

-- | The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
--
-- /Note:/ Consider using 'status4xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus4xx :: Lens.Lens' StatusCodes (Core.Maybe Core.Int)
scStatus4xx = Lens.field @"status4xx"
{-# DEPRECATED scStatus4xx "Use generic-lens or generic-optics with 'status4xx' instead." #-}

-- | The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
--
-- /Note:/ Consider using 'status5xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus5xx :: Lens.Lens' StatusCodes (Core.Maybe Core.Int)
scStatus5xx = Lens.field @"status5xx"
{-# DEPRECATED scStatus5xx "Use generic-lens or generic-optics with 'status5xx' instead." #-}

instance Core.FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      Core.<$> (x Core..@? "Status2xx")
      Core.<*> (x Core..@? "Status3xx")
      Core.<*> (x Core..@? "Status4xx")
      Core.<*> (x Core..@? "Status5xx")
