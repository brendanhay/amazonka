{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionHistoryDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionHistoryDetails
  ( ActionHistoryDetails (..),

    -- * Smart constructor
    mkActionHistoryDetails,

    -- * Lenses
    ahdMessage,
    ahdAction,
  )
where

import qualified Network.AWS.Budgets.Types.Action as Types
import qualified Network.AWS.Budgets.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description of details of the event.
--
-- /See:/ 'mkActionHistoryDetails' smart constructor.
data ActionHistoryDetails = ActionHistoryDetails'
  { message :: Types.GenericString,
    -- | The budget action resource.
    action :: Types.Action
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionHistoryDetails' value with any optional fields omitted.
mkActionHistoryDetails ::
  -- | 'message'
  Types.GenericString ->
  -- | 'action'
  Types.Action ->
  ActionHistoryDetails
mkActionHistoryDetails message action =
  ActionHistoryDetails' {message, action}

-- | Undocumented field.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahdMessage :: Lens.Lens' ActionHistoryDetails Types.GenericString
ahdMessage = Lens.field @"message"
{-# DEPRECATED ahdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The budget action resource.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahdAction :: Lens.Lens' ActionHistoryDetails Types.Action
ahdAction = Lens.field @"action"
{-# DEPRECATED ahdAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Core.FromJSON ActionHistoryDetails where
  parseJSON =
    Core.withObject "ActionHistoryDetails" Core.$
      \x ->
        ActionHistoryDetails'
          Core.<$> (x Core..: "Message") Core.<*> (x Core..: "Action")
