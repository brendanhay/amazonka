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

import Network.AWS.Budgets.Types.Action
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The description of details of the event.
--
-- /See:/ 'mkActionHistoryDetails' smart constructor.
data ActionHistoryDetails = ActionHistoryDetails'
  { message ::
      Lude.Text,
    action :: Action
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionHistoryDetails' with the minimum fields required to make a request.
--
-- * 'action' - The budget action resource.
-- * 'message' - Undocumented field.
mkActionHistoryDetails ::
  -- | 'message'
  Lude.Text ->
  -- | 'action'
  Action ->
  ActionHistoryDetails
mkActionHistoryDetails pMessage_ pAction_ =
  ActionHistoryDetails' {message = pMessage_, action = pAction_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahdMessage :: Lens.Lens' ActionHistoryDetails Lude.Text
ahdMessage = Lens.lens (message :: ActionHistoryDetails -> Lude.Text) (\s a -> s {message = a} :: ActionHistoryDetails)
{-# DEPRECATED ahdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The budget action resource.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahdAction :: Lens.Lens' ActionHistoryDetails Action
ahdAction = Lens.lens (action :: ActionHistoryDetails -> Action) (\s a -> s {action = a} :: ActionHistoryDetails)
{-# DEPRECATED ahdAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.FromJSON ActionHistoryDetails where
  parseJSON =
    Lude.withObject
      "ActionHistoryDetails"
      ( \x ->
          ActionHistoryDetails'
            Lude.<$> (x Lude..: "Message") Lude.<*> (x Lude..: "Action")
      )
