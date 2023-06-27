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
-- Module      : Amazonka.LexV2Models.Types.ConversationLogsDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLogsDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLogsDataSourceFilterBy
import qualified Amazonka.Prelude as Prelude

-- | The data source that uses conversation logs.
--
-- /See:/ 'newConversationLogsDataSource' smart constructor.
data ConversationLogsDataSource = ConversationLogsDataSource'
  { -- | The bot Id from the conversation logs.
    botId :: Prelude.Text,
    -- | The bot alias Id from the conversation logs.
    botAliasId :: Prelude.Text,
    -- | The locale Id of the conversation log.
    localeId :: Prelude.Text,
    -- | The filter for the data source of the conversation log.
    filter' :: ConversationLogsDataSourceFilterBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLogsDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'conversationLogsDataSource_botId' - The bot Id from the conversation logs.
--
-- 'botAliasId', 'conversationLogsDataSource_botAliasId' - The bot alias Id from the conversation logs.
--
-- 'localeId', 'conversationLogsDataSource_localeId' - The locale Id of the conversation log.
--
-- 'filter'', 'conversationLogsDataSource_filter' - The filter for the data source of the conversation log.
newConversationLogsDataSource ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'filter''
  ConversationLogsDataSourceFilterBy ->
  ConversationLogsDataSource
newConversationLogsDataSource
  pBotId_
  pBotAliasId_
  pLocaleId_
  pFilter_ =
    ConversationLogsDataSource'
      { botId = pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_,
        filter' = pFilter_
      }

-- | The bot Id from the conversation logs.
conversationLogsDataSource_botId :: Lens.Lens' ConversationLogsDataSource Prelude.Text
conversationLogsDataSource_botId = Lens.lens (\ConversationLogsDataSource' {botId} -> botId) (\s@ConversationLogsDataSource' {} a -> s {botId = a} :: ConversationLogsDataSource)

-- | The bot alias Id from the conversation logs.
conversationLogsDataSource_botAliasId :: Lens.Lens' ConversationLogsDataSource Prelude.Text
conversationLogsDataSource_botAliasId = Lens.lens (\ConversationLogsDataSource' {botAliasId} -> botAliasId) (\s@ConversationLogsDataSource' {} a -> s {botAliasId = a} :: ConversationLogsDataSource)

-- | The locale Id of the conversation log.
conversationLogsDataSource_localeId :: Lens.Lens' ConversationLogsDataSource Prelude.Text
conversationLogsDataSource_localeId = Lens.lens (\ConversationLogsDataSource' {localeId} -> localeId) (\s@ConversationLogsDataSource' {} a -> s {localeId = a} :: ConversationLogsDataSource)

-- | The filter for the data source of the conversation log.
conversationLogsDataSource_filter :: Lens.Lens' ConversationLogsDataSource ConversationLogsDataSourceFilterBy
conversationLogsDataSource_filter = Lens.lens (\ConversationLogsDataSource' {filter'} -> filter') (\s@ConversationLogsDataSource' {} a -> s {filter' = a} :: ConversationLogsDataSource)

instance Data.FromJSON ConversationLogsDataSource where
  parseJSON =
    Data.withObject
      "ConversationLogsDataSource"
      ( \x ->
          ConversationLogsDataSource'
            Prelude.<$> (x Data..: "botId")
            Prelude.<*> (x Data..: "botAliasId")
            Prelude.<*> (x Data..: "localeId")
            Prelude.<*> (x Data..: "filter")
      )

instance Prelude.Hashable ConversationLogsDataSource where
  hashWithSalt _salt ConversationLogsDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botAliasId
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData ConversationLogsDataSource where
  rnf ConversationLogsDataSource' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf filter'

instance Data.ToJSON ConversationLogsDataSource where
  toJSON ConversationLogsDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botId" Data..= botId),
            Prelude.Just ("botAliasId" Data..= botAliasId),
            Prelude.Just ("localeId" Data..= localeId),
            Prelude.Just ("filter" Data..= filter')
          ]
      )
