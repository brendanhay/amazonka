{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Queue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Queue
  ( Queue (..),

    -- * Smart constructor
    mkQueue,

    -- * Lenses
    qName,
    qURL,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a queue.
--
-- /See:/ 'mkQueue' smart constructor.
data Queue = Queue'
  { -- | The name of the queue.
    name :: Core.Maybe Types.String,
    -- | The URL of the queue.
    url :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Queue' value with any optional fields omitted.
mkQueue ::
  Queue
mkQueue = Queue' {name = Core.Nothing, url = Core.Nothing}

-- | The name of the queue.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qName :: Lens.Lens' Queue (Core.Maybe Types.String)
qName = Lens.field @"name"
{-# DEPRECATED qName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qURL :: Lens.Lens' Queue (Core.Maybe Types.String)
qURL = Lens.field @"url"
{-# DEPRECATED qURL "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromXML Queue where
  parseXML x =
    Queue' Core.<$> (x Core..@? "Name") Core.<*> (x Core..@? "URL")
