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
-- Module      : Amazonka.LexV2Models.Types.ConversationLevelTestResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLevelTestResults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLevelTestResultItem
import qualified Amazonka.Prelude as Prelude

-- | The test set results data at the conversation level.
--
-- /See:/ 'newConversationLevelTestResults' smart constructor.
data ConversationLevelTestResults = ConversationLevelTestResults'
  { -- | The item list in the test set results data at the conversation level.
    items :: [ConversationLevelTestResultItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLevelTestResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'conversationLevelTestResults_items' - The item list in the test set results data at the conversation level.
newConversationLevelTestResults ::
  ConversationLevelTestResults
newConversationLevelTestResults =
  ConversationLevelTestResults'
    { items =
        Prelude.mempty
    }

-- | The item list in the test set results data at the conversation level.
conversationLevelTestResults_items :: Lens.Lens' ConversationLevelTestResults [ConversationLevelTestResultItem]
conversationLevelTestResults_items = Lens.lens (\ConversationLevelTestResults' {items} -> items) (\s@ConversationLevelTestResults' {} a -> s {items = a} :: ConversationLevelTestResults) Prelude.. Lens.coerced

instance Data.FromJSON ConversationLevelTestResults where
  parseJSON =
    Data.withObject
      "ConversationLevelTestResults"
      ( \x ->
          ConversationLevelTestResults'
            Prelude.<$> (x Data..:? "items" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ConversationLevelTestResults
  where
  hashWithSalt _salt ConversationLevelTestResults' {..} =
    _salt `Prelude.hashWithSalt` items

instance Prelude.NFData ConversationLevelTestResults where
  rnf ConversationLevelTestResults' {..} =
    Prelude.rnf items
