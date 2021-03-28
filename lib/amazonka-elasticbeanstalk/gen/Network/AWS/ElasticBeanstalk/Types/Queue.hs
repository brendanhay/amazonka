{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Queue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Queue
  ( Queue (..)
  -- * Smart constructor
  , mkQueue
  -- * Lenses
  , qName
  , qURL
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a queue.
--
-- /See:/ 'mkQueue' smart constructor.
data Queue = Queue'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the queue.
  , url :: Core.Maybe Core.Text
    -- ^ The URL of the queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Queue' value with any optional fields omitted.
mkQueue
    :: Queue
mkQueue = Queue'{name = Core.Nothing, url = Core.Nothing}

-- | The name of the queue.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qName :: Lens.Lens' Queue (Core.Maybe Core.Text)
qName = Lens.field @"name"
{-# INLINEABLE qName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qURL :: Lens.Lens' Queue (Core.Maybe Core.Text)
qURL = Lens.field @"url"
{-# INLINEABLE qURL #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromXML Queue where
        parseXML x
          = Queue' Core.<$> (x Core..@? "Name") Core.<*> x Core..@? "URL"
