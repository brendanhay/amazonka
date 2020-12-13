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
    qURL,
    qName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a queue.
--
-- /See:/ 'mkQueue' smart constructor.
data Queue = Queue'
  { -- | The URL of the queue.
    url :: Lude.Maybe Lude.Text,
    -- | The name of the queue.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Queue' with the minimum fields required to make a request.
--
-- * 'url' - The URL of the queue.
-- * 'name' - The name of the queue.
mkQueue ::
  Queue
mkQueue = Queue' {url = Lude.Nothing, name = Lude.Nothing}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qURL :: Lens.Lens' Queue (Lude.Maybe Lude.Text)
qURL = Lens.lens (url :: Queue -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Queue)
{-# DEPRECATED qURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The name of the queue.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qName :: Lens.Lens' Queue (Lude.Maybe Lude.Text)
qName = Lens.lens (name :: Queue -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Queue)
{-# DEPRECATED qName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML Queue where
  parseXML x =
    Queue' Lude.<$> (x Lude..@? "URL") Lude.<*> (x Lude..@? "Name")
