{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
  ( ActivityStartedEventDetails (..),

    -- * Smart constructor
    mkActivityStartedEventDetails,

    -- * Lenses
    asedWorkerName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the start of an activity during an execution.
--
-- /See:/ 'mkActivityStartedEventDetails' smart constructor.
newtype ActivityStartedEventDetails = ActivityStartedEventDetails'
  { workerName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityStartedEventDetails' with the minimum fields required to make a request.
--
-- * 'workerName' - The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
mkActivityStartedEventDetails ::
  ActivityStartedEventDetails
mkActivityStartedEventDetails =
  ActivityStartedEventDetails' {workerName = Lude.Nothing}

-- | The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
--
-- /Note:/ Consider using 'workerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedWorkerName :: Lens.Lens' ActivityStartedEventDetails (Lude.Maybe Lude.Text)
asedWorkerName = Lens.lens (workerName :: ActivityStartedEventDetails -> Lude.Maybe Lude.Text) (\s a -> s {workerName = a} :: ActivityStartedEventDetails)
{-# DEPRECATED asedWorkerName "Use generic-lens or generic-optics with 'workerName' instead." #-}

instance Lude.FromJSON ActivityStartedEventDetails where
  parseJSON =
    Lude.withObject
      "ActivityStartedEventDetails"
      ( \x ->
          ActivityStartedEventDetails' Lude.<$> (x Lude..:? "workerName")
      )
