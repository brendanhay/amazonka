{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentStatus
  ( DatasetContentStatus (..),

    -- * Smart constructor
    mkDatasetContentStatus,

    -- * Lenses
    dcsState,
    dcsReason,
  )
where

import Network.AWS.IoTAnalytics.Types.DatasetContentState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The state of the data set contents and the reason they are in this state.
--
-- /See:/ 'mkDatasetContentStatus' smart constructor.
data DatasetContentStatus = DatasetContentStatus'
  { state ::
      Lude.Maybe DatasetContentState,
    reason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetContentStatus' with the minimum fields required to make a request.
--
-- * 'reason' - The reason the data set contents are in this state.
-- * 'state' - The state of the data set contents. Can be one of READY, CREATING, SUCCEEDED, or FAILED.
mkDatasetContentStatus ::
  DatasetContentStatus
mkDatasetContentStatus =
  DatasetContentStatus'
    { state = Lude.Nothing,
      reason = Lude.Nothing
    }

-- | The state of the data set contents. Can be one of READY, CREATING, SUCCEEDED, or FAILED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsState :: Lens.Lens' DatasetContentStatus (Lude.Maybe DatasetContentState)
dcsState = Lens.lens (state :: DatasetContentStatus -> Lude.Maybe DatasetContentState) (\s a -> s {state = a} :: DatasetContentStatus)
{-# DEPRECATED dcsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason the data set contents are in this state.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsReason :: Lens.Lens' DatasetContentStatus (Lude.Maybe Lude.Text)
dcsReason = Lens.lens (reason :: DatasetContentStatus -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: DatasetContentStatus)
{-# DEPRECATED dcsReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.FromJSON DatasetContentStatus where
  parseJSON =
    Lude.withObject
      "DatasetContentStatus"
      ( \x ->
          DatasetContentStatus'
            Lude.<$> (x Lude..:? "state") Lude.<*> (x Lude..:? "reason")
      )
