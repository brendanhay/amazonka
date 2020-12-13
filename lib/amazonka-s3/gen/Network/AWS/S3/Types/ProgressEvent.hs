{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ProgressEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ProgressEvent
  ( ProgressEvent (..),

    -- * Smart constructor
    mkProgressEvent,

    -- * Lenses
    peDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Progress

-- | This data type contains information about the progress event of an operation.
--
-- /See:/ 'mkProgressEvent' smart constructor.
newtype ProgressEvent = ProgressEvent'
  { -- | The Progress event details.
    details :: Lude.Maybe Progress
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProgressEvent' with the minimum fields required to make a request.
--
-- * 'details' - The Progress event details.
mkProgressEvent ::
  ProgressEvent
mkProgressEvent = ProgressEvent' {details = Lude.Nothing}

-- | The Progress event details.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peDetails :: Lens.Lens' ProgressEvent (Lude.Maybe Progress)
peDetails = Lens.lens (details :: ProgressEvent -> Lude.Maybe Progress) (\s a -> s {details = a} :: ProgressEvent)
{-# DEPRECATED peDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.FromXML ProgressEvent where
  parseXML x = ProgressEvent' Lude.<$> (x Lude..@? "Details")
