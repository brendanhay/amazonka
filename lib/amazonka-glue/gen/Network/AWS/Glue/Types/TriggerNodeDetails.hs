{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerNodeDetails
  ( TriggerNodeDetails (..),

    -- * Smart constructor
    mkTriggerNodeDetails,

    -- * Lenses
    tndTrigger,
  )
where

import Network.AWS.Glue.Types.Trigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a Trigger node present in the workflow.
--
-- /See:/ 'mkTriggerNodeDetails' smart constructor.
newtype TriggerNodeDetails = TriggerNodeDetails'
  { -- | The information of the trigger represented by the trigger node.
    trigger :: Lude.Maybe Trigger
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TriggerNodeDetails' with the minimum fields required to make a request.
--
-- * 'trigger' - The information of the trigger represented by the trigger node.
mkTriggerNodeDetails ::
  TriggerNodeDetails
mkTriggerNodeDetails = TriggerNodeDetails' {trigger = Lude.Nothing}

-- | The information of the trigger represented by the trigger node.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tndTrigger :: Lens.Lens' TriggerNodeDetails (Lude.Maybe Trigger)
tndTrigger = Lens.lens (trigger :: TriggerNodeDetails -> Lude.Maybe Trigger) (\s a -> s {trigger = a} :: TriggerNodeDetails)
{-# DEPRECATED tndTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

instance Lude.FromJSON TriggerNodeDetails where
  parseJSON =
    Lude.withObject
      "TriggerNodeDetails"
      (\x -> TriggerNodeDetails' Lude.<$> (x Lude..:? "Trigger"))
