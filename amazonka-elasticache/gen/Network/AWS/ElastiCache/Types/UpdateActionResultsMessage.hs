{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateActionResultsMessage where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.ProcessedUpdateAction
import Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newUpdateActionResultsMessage' smart constructor.
data UpdateActionResultsMessage = UpdateActionResultsMessage'
  { -- | Update actions that have been processed successfully
    processedUpdateActions :: Core.Maybe [ProcessedUpdateAction],
    -- | Update actions that haven\'t been processed successfully
    unprocessedUpdateActions :: Core.Maybe [UnprocessedUpdateAction]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateActionResultsMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processedUpdateActions', 'updateActionResultsMessage_processedUpdateActions' - Update actions that have been processed successfully
--
-- 'unprocessedUpdateActions', 'updateActionResultsMessage_unprocessedUpdateActions' - Update actions that haven\'t been processed successfully
newUpdateActionResultsMessage ::
  UpdateActionResultsMessage
newUpdateActionResultsMessage =
  UpdateActionResultsMessage'
    { processedUpdateActions =
        Core.Nothing,
      unprocessedUpdateActions = Core.Nothing
    }

-- | Update actions that have been processed successfully
updateActionResultsMessage_processedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Core.Maybe [ProcessedUpdateAction])
updateActionResultsMessage_processedUpdateActions = Lens.lens (\UpdateActionResultsMessage' {processedUpdateActions} -> processedUpdateActions) (\s@UpdateActionResultsMessage' {} a -> s {processedUpdateActions = a} :: UpdateActionResultsMessage) Core.. Lens.mapping Lens._Coerce

-- | Update actions that haven\'t been processed successfully
updateActionResultsMessage_unprocessedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Core.Maybe [UnprocessedUpdateAction])
updateActionResultsMessage_unprocessedUpdateActions = Lens.lens (\UpdateActionResultsMessage' {unprocessedUpdateActions} -> unprocessedUpdateActions) (\s@UpdateActionResultsMessage' {} a -> s {unprocessedUpdateActions = a} :: UpdateActionResultsMessage) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML UpdateActionResultsMessage where
  parseXML x =
    UpdateActionResultsMessage'
      Core.<$> ( x Core..@? "ProcessedUpdateActions"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ProcessedUpdateAction")
               )
      Core.<*> ( x Core..@? "UnprocessedUpdateActions"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "UnprocessedUpdateAction")
               )

instance Core.Hashable UpdateActionResultsMessage

instance Core.NFData UpdateActionResultsMessage
