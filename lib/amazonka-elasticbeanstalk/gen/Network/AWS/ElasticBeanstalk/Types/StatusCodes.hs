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
import qualified Network.AWS.Prelude as Lude

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html Status Code Definitions> .
--
-- /See:/ 'mkStatusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { status2xx :: Lude.Maybe Lude.Int,
    status3xx :: Lude.Maybe Lude.Int,
    status4xx :: Lude.Maybe Lude.Int,
    status5xx :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StatusCodes' with the minimum fields required to make a request.
--
-- * 'status2xx' - The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
-- * 'status3xx' - The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
-- * 'status4xx' - The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
-- * 'status5xx' - The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
mkStatusCodes ::
  StatusCodes
mkStatusCodes =
  StatusCodes'
    { status2xx = Lude.Nothing,
      status3xx = Lude.Nothing,
      status4xx = Lude.Nothing,
      status5xx = Lude.Nothing
    }

-- | The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
--
-- /Note:/ Consider using 'status2xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus2xx :: Lens.Lens' StatusCodes (Lude.Maybe Lude.Int)
scStatus2xx = Lens.lens (status2xx :: StatusCodes -> Lude.Maybe Lude.Int) (\s a -> s {status2xx = a} :: StatusCodes)
{-# DEPRECATED scStatus2xx "Use generic-lens or generic-optics with 'status2xx' instead." #-}

-- | The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
--
-- /Note:/ Consider using 'status3xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus3xx :: Lens.Lens' StatusCodes (Lude.Maybe Lude.Int)
scStatus3xx = Lens.lens (status3xx :: StatusCodes -> Lude.Maybe Lude.Int) (\s a -> s {status3xx = a} :: StatusCodes)
{-# DEPRECATED scStatus3xx "Use generic-lens or generic-optics with 'status3xx' instead." #-}

-- | The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
--
-- /Note:/ Consider using 'status4xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus4xx :: Lens.Lens' StatusCodes (Lude.Maybe Lude.Int)
scStatus4xx = Lens.lens (status4xx :: StatusCodes -> Lude.Maybe Lude.Int) (\s a -> s {status4xx = a} :: StatusCodes)
{-# DEPRECATED scStatus4xx "Use generic-lens or generic-optics with 'status4xx' instead." #-}

-- | The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
--
-- /Note:/ Consider using 'status5xx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus5xx :: Lens.Lens' StatusCodes (Lude.Maybe Lude.Int)
scStatus5xx = Lens.lens (status5xx :: StatusCodes -> Lude.Maybe Lude.Int) (\s a -> s {status5xx = a} :: StatusCodes)
{-# DEPRECATED scStatus5xx "Use generic-lens or generic-optics with 'status5xx' instead." #-}

instance Lude.FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      Lude.<$> (x Lude..@? "Status2xx")
      Lude.<*> (x Lude..@? "Status3xx")
      Lude.<*> (x Lude..@? "Status4xx")
      Lude.<*> (x Lude..@? "Status5xx")
