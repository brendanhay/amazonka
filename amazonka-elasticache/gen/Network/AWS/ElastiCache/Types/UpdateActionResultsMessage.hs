{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElastiCache.Types.ProcessedUpdateAction
import Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newUpdateActionResultsMessage' smart constructor.
data UpdateActionResultsMessage = UpdateActionResultsMessage'
  { -- | Update actions that have been processed successfully
    processedUpdateActions :: Prelude.Maybe [ProcessedUpdateAction],
    -- | Update actions that haven\'t been processed successfully
    unprocessedUpdateActions :: Prelude.Maybe [UnprocessedUpdateAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      unprocessedUpdateActions = Prelude.Nothing
    }

-- | Update actions that have been processed successfully
updateActionResultsMessage_processedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Prelude.Maybe [ProcessedUpdateAction])
updateActionResultsMessage_processedUpdateActions = Lens.lens (\UpdateActionResultsMessage' {processedUpdateActions} -> processedUpdateActions) (\s@UpdateActionResultsMessage' {} a -> s {processedUpdateActions = a} :: UpdateActionResultsMessage) Prelude.. Lens.mapping Prelude._Coerce

-- | Update actions that haven\'t been processed successfully
updateActionResultsMessage_unprocessedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Prelude.Maybe [UnprocessedUpdateAction])
updateActionResultsMessage_unprocessedUpdateActions = Lens.lens (\UpdateActionResultsMessage' {unprocessedUpdateActions} -> unprocessedUpdateActions) (\s@UpdateActionResultsMessage' {} a -> s {unprocessedUpdateActions = a} :: UpdateActionResultsMessage) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML UpdateActionResultsMessage where
  parseXML x =
    UpdateActionResultsMessage'
      Prelude.<$> ( x Prelude..@? "ProcessedUpdateActions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "ProcessedUpdateAction")
                  )
      Prelude.<*> ( x Prelude..@? "UnprocessedUpdateActions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "UnprocessedUpdateAction")
                  )

instance Prelude.Hashable UpdateActionResultsMessage

instance Prelude.NFData UpdateActionResultsMessage
