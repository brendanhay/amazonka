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
-- Module      : Amazonka.LexV2Models.Types.TestSetGenerationDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetGenerationDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLogsDataSource
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the data source from which the test set is
-- generated.
--
-- /See:/ 'newTestSetGenerationDataSource' smart constructor.
data TestSetGenerationDataSource = TestSetGenerationDataSource'
  { -- | Contains information about the bot from which the conversation logs are
    -- sourced.
    conversationLogsDataSource :: Prelude.Maybe ConversationLogsDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetGenerationDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversationLogsDataSource', 'testSetGenerationDataSource_conversationLogsDataSource' - Contains information about the bot from which the conversation logs are
-- sourced.
newTestSetGenerationDataSource ::
  TestSetGenerationDataSource
newTestSetGenerationDataSource =
  TestSetGenerationDataSource'
    { conversationLogsDataSource =
        Prelude.Nothing
    }

-- | Contains information about the bot from which the conversation logs are
-- sourced.
testSetGenerationDataSource_conversationLogsDataSource :: Lens.Lens' TestSetGenerationDataSource (Prelude.Maybe ConversationLogsDataSource)
testSetGenerationDataSource_conversationLogsDataSource = Lens.lens (\TestSetGenerationDataSource' {conversationLogsDataSource} -> conversationLogsDataSource) (\s@TestSetGenerationDataSource' {} a -> s {conversationLogsDataSource = a} :: TestSetGenerationDataSource)

instance Data.FromJSON TestSetGenerationDataSource where
  parseJSON =
    Data.withObject
      "TestSetGenerationDataSource"
      ( \x ->
          TestSetGenerationDataSource'
            Prelude.<$> (x Data..:? "conversationLogsDataSource")
      )

instance Prelude.Hashable TestSetGenerationDataSource where
  hashWithSalt _salt TestSetGenerationDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` conversationLogsDataSource

instance Prelude.NFData TestSetGenerationDataSource where
  rnf TestSetGenerationDataSource' {..} =
    Prelude.rnf conversationLogsDataSource

instance Data.ToJSON TestSetGenerationDataSource where
  toJSON TestSetGenerationDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("conversationLogsDataSource" Data..=)
              Prelude.<$> conversationLogsDataSource
          ]
      )
