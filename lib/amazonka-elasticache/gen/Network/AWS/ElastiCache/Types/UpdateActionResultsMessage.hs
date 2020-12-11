-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
  ( UpdateActionResultsMessage (..),

    -- * Smart constructor
    mkUpdateActionResultsMessage,

    -- * Lenses
    uarmUnprocessedUpdateActions,
    uarmProcessedUpdateActions,
  )
where

import Network.AWS.ElastiCache.Types.ProcessedUpdateAction
import Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkUpdateActionResultsMessage' smart constructor.
data UpdateActionResultsMessage = UpdateActionResultsMessage'
  { unprocessedUpdateActions ::
      Lude.Maybe [UnprocessedUpdateAction],
    processedUpdateActions ::
      Lude.Maybe [ProcessedUpdateAction]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateActionResultsMessage' with the minimum fields required to make a request.
--
-- * 'processedUpdateActions' - Update actions that have been processed successfully
-- * 'unprocessedUpdateActions' - Update actions that haven't been processed successfully
mkUpdateActionResultsMessage ::
  UpdateActionResultsMessage
mkUpdateActionResultsMessage =
  UpdateActionResultsMessage'
    { unprocessedUpdateActions =
        Lude.Nothing,
      processedUpdateActions = Lude.Nothing
    }

-- | Update actions that haven't been processed successfully
--
-- /Note:/ Consider using 'unprocessedUpdateActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarmUnprocessedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Lude.Maybe [UnprocessedUpdateAction])
uarmUnprocessedUpdateActions = Lens.lens (unprocessedUpdateActions :: UpdateActionResultsMessage -> Lude.Maybe [UnprocessedUpdateAction]) (\s a -> s {unprocessedUpdateActions = a} :: UpdateActionResultsMessage)
{-# DEPRECATED uarmUnprocessedUpdateActions "Use generic-lens or generic-optics with 'unprocessedUpdateActions' instead." #-}

-- | Update actions that have been processed successfully
--
-- /Note:/ Consider using 'processedUpdateActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarmProcessedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Lude.Maybe [ProcessedUpdateAction])
uarmProcessedUpdateActions = Lens.lens (processedUpdateActions :: UpdateActionResultsMessage -> Lude.Maybe [ProcessedUpdateAction]) (\s a -> s {processedUpdateActions = a} :: UpdateActionResultsMessage)
{-# DEPRECATED uarmProcessedUpdateActions "Use generic-lens or generic-optics with 'processedUpdateActions' instead." #-}

instance Lude.FromXML UpdateActionResultsMessage where
  parseXML x =
    UpdateActionResultsMessage'
      Lude.<$> ( x Lude..@? "UnprocessedUpdateActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "UnprocessedUpdateAction")
               )
      Lude.<*> ( x Lude..@? "ProcessedUpdateActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ProcessedUpdateAction")
               )
