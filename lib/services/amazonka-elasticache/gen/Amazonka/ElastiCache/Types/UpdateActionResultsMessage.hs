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
-- Module      : Amazonka.ElastiCache.Types.UpdateActionResultsMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.UpdateActionResultsMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.ProcessedUpdateAction
import Amazonka.ElastiCache.Types.UnprocessedUpdateAction
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUpdateActionResultsMessage' smart constructor.
data UpdateActionResultsMessage = UpdateActionResultsMessage'
  { -- | Update actions that have been processed successfully
    processedUpdateActions :: Prelude.Maybe [ProcessedUpdateAction],
    -- | Update actions that haven\'t been processed successfully
    unprocessedUpdateActions :: Prelude.Maybe [UnprocessedUpdateAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
updateActionResultsMessage_processedUpdateActions = Lens.lens (\UpdateActionResultsMessage' {processedUpdateActions} -> processedUpdateActions) (\s@UpdateActionResultsMessage' {} a -> s {processedUpdateActions = a} :: UpdateActionResultsMessage) Prelude.. Lens.mapping Lens.coerced

-- | Update actions that haven\'t been processed successfully
updateActionResultsMessage_unprocessedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Prelude.Maybe [UnprocessedUpdateAction])
updateActionResultsMessage_unprocessedUpdateActions = Lens.lens (\UpdateActionResultsMessage' {unprocessedUpdateActions} -> unprocessedUpdateActions) (\s@UpdateActionResultsMessage' {} a -> s {unprocessedUpdateActions = a} :: UpdateActionResultsMessage) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML UpdateActionResultsMessage where
  parseXML x =
    UpdateActionResultsMessage'
      Prelude.<$> ( x Data..@? "ProcessedUpdateActions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ProcessedUpdateAction")
                  )
      Prelude.<*> ( x Data..@? "UnprocessedUpdateActions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "UnprocessedUpdateAction")
                  )

instance Prelude.Hashable UpdateActionResultsMessage where
  hashWithSalt _salt UpdateActionResultsMessage' {..} =
    _salt `Prelude.hashWithSalt` processedUpdateActions
      `Prelude.hashWithSalt` unprocessedUpdateActions

instance Prelude.NFData UpdateActionResultsMessage where
  rnf UpdateActionResultsMessage' {..} =
    Prelude.rnf processedUpdateActions
      `Prelude.seq` Prelude.rnf unprocessedUpdateActions
