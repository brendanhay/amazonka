{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.BatchCreateCustomVocabularyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Batch create custom vocabulary item for the specified locale in the
-- specified bot.
module Amazonka.LexV2Models.BatchCreateCustomVocabularyItem
  ( -- * Creating a Request
    BatchCreateCustomVocabularyItem (..),
    newBatchCreateCustomVocabularyItem,

    -- * Request Lenses
    batchCreateCustomVocabularyItem_botId,
    batchCreateCustomVocabularyItem_botVersion,
    batchCreateCustomVocabularyItem_localeId,
    batchCreateCustomVocabularyItem_customVocabularyItemList,

    -- * Destructuring the Response
    BatchCreateCustomVocabularyItemResponse (..),
    newBatchCreateCustomVocabularyItemResponse,

    -- * Response Lenses
    batchCreateCustomVocabularyItemResponse_botId,
    batchCreateCustomVocabularyItemResponse_botVersion,
    batchCreateCustomVocabularyItemResponse_errors,
    batchCreateCustomVocabularyItemResponse_localeId,
    batchCreateCustomVocabularyItemResponse_resources,
    batchCreateCustomVocabularyItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCreateCustomVocabularyItem' smart constructor.
data BatchCreateCustomVocabularyItem = BatchCreateCustomVocabularyItem'
  { -- | The unique identifier of the bot to batch create the custom vocabulary
    -- item for.
    botId :: Prelude.Text,
    -- | The bot version of the bot to batch create the custom vocabulary item
    -- for.
    botVersion :: Prelude.Text,
    -- | The unique locale identifier of the bot to batch create the custom
    -- vocabulary item for.
    localeId :: Prelude.Text,
    -- | The custom vocabulary item list of the bot to batch create the custom
    -- vocabulary item for.
    customVocabularyItemList :: Prelude.NonEmpty NewCustomVocabularyItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateCustomVocabularyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'batchCreateCustomVocabularyItem_botId' - The unique identifier of the bot to batch create the custom vocabulary
-- item for.
--
-- 'botVersion', 'batchCreateCustomVocabularyItem_botVersion' - The bot version of the bot to batch create the custom vocabulary item
-- for.
--
-- 'localeId', 'batchCreateCustomVocabularyItem_localeId' - The unique locale identifier of the bot to batch create the custom
-- vocabulary item for.
--
-- 'customVocabularyItemList', 'batchCreateCustomVocabularyItem_customVocabularyItemList' - The custom vocabulary item list of the bot to batch create the custom
-- vocabulary item for.
newBatchCreateCustomVocabularyItem ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'customVocabularyItemList'
  Prelude.NonEmpty NewCustomVocabularyItem ->
  BatchCreateCustomVocabularyItem
newBatchCreateCustomVocabularyItem
  pBotId_
  pBotVersion_
  pLocaleId_
  pCustomVocabularyItemList_ =
    BatchCreateCustomVocabularyItem'
      { botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        customVocabularyItemList =
          Lens.coerced
            Lens.# pCustomVocabularyItemList_
      }

-- | The unique identifier of the bot to batch create the custom vocabulary
-- item for.
batchCreateCustomVocabularyItem_botId :: Lens.Lens' BatchCreateCustomVocabularyItem Prelude.Text
batchCreateCustomVocabularyItem_botId = Lens.lens (\BatchCreateCustomVocabularyItem' {botId} -> botId) (\s@BatchCreateCustomVocabularyItem' {} a -> s {botId = a} :: BatchCreateCustomVocabularyItem)

-- | The bot version of the bot to batch create the custom vocabulary item
-- for.
batchCreateCustomVocabularyItem_botVersion :: Lens.Lens' BatchCreateCustomVocabularyItem Prelude.Text
batchCreateCustomVocabularyItem_botVersion = Lens.lens (\BatchCreateCustomVocabularyItem' {botVersion} -> botVersion) (\s@BatchCreateCustomVocabularyItem' {} a -> s {botVersion = a} :: BatchCreateCustomVocabularyItem)

-- | The unique locale identifier of the bot to batch create the custom
-- vocabulary item for.
batchCreateCustomVocabularyItem_localeId :: Lens.Lens' BatchCreateCustomVocabularyItem Prelude.Text
batchCreateCustomVocabularyItem_localeId = Lens.lens (\BatchCreateCustomVocabularyItem' {localeId} -> localeId) (\s@BatchCreateCustomVocabularyItem' {} a -> s {localeId = a} :: BatchCreateCustomVocabularyItem)

-- | The custom vocabulary item list of the bot to batch create the custom
-- vocabulary item for.
batchCreateCustomVocabularyItem_customVocabularyItemList :: Lens.Lens' BatchCreateCustomVocabularyItem (Prelude.NonEmpty NewCustomVocabularyItem)
batchCreateCustomVocabularyItem_customVocabularyItemList = Lens.lens (\BatchCreateCustomVocabularyItem' {customVocabularyItemList} -> customVocabularyItemList) (\s@BatchCreateCustomVocabularyItem' {} a -> s {customVocabularyItemList = a} :: BatchCreateCustomVocabularyItem) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchCreateCustomVocabularyItem
  where
  type
    AWSResponse BatchCreateCustomVocabularyItem =
      BatchCreateCustomVocabularyItemResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreateCustomVocabularyItemResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchCreateCustomVocabularyItem
  where
  hashWithSalt
    _salt
    BatchCreateCustomVocabularyItem' {..} =
      _salt
        `Prelude.hashWithSalt` botId
        `Prelude.hashWithSalt` botVersion
        `Prelude.hashWithSalt` localeId
        `Prelude.hashWithSalt` customVocabularyItemList

instance
  Prelude.NFData
    BatchCreateCustomVocabularyItem
  where
  rnf BatchCreateCustomVocabularyItem' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botVersion `Prelude.seq`
        Prelude.rnf localeId `Prelude.seq`
          Prelude.rnf customVocabularyItemList

instance
  Data.ToHeaders
    BatchCreateCustomVocabularyItem
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchCreateCustomVocabularyItem where
  toJSON BatchCreateCustomVocabularyItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "customVocabularyItemList"
                  Data..= customVocabularyItemList
              )
          ]
      )

instance Data.ToPath BatchCreateCustomVocabularyItem where
  toPath BatchCreateCustomVocabularyItem' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/customvocabulary/DEFAULT/batchcreate"
      ]

instance Data.ToQuery BatchCreateCustomVocabularyItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCreateCustomVocabularyItemResponse' smart constructor.
data BatchCreateCustomVocabularyItemResponse = BatchCreateCustomVocabularyItemResponse'
  { -- | The unique identifier of the bot to batch create response for the custom
    -- vocabulary item.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The bot version of the bot to batch create the custom vocabulary item
    -- response for.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The errors of the action to batch create the custom vocabulary item
    -- response for a bot.
    errors :: Prelude.Maybe [FailedCustomVocabularyItem],
    -- | The unique locale identifier of the bot to batch create the custom
    -- vocabulary item response for.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The resources of the action to batch create the custom vocabulary item
    -- response for a bot.
    resources :: Prelude.Maybe [CustomVocabularyItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateCustomVocabularyItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'batchCreateCustomVocabularyItemResponse_botId' - The unique identifier of the bot to batch create response for the custom
-- vocabulary item.
--
-- 'botVersion', 'batchCreateCustomVocabularyItemResponse_botVersion' - The bot version of the bot to batch create the custom vocabulary item
-- response for.
--
-- 'errors', 'batchCreateCustomVocabularyItemResponse_errors' - The errors of the action to batch create the custom vocabulary item
-- response for a bot.
--
-- 'localeId', 'batchCreateCustomVocabularyItemResponse_localeId' - The unique locale identifier of the bot to batch create the custom
-- vocabulary item response for.
--
-- 'resources', 'batchCreateCustomVocabularyItemResponse_resources' - The resources of the action to batch create the custom vocabulary item
-- response for a bot.
--
-- 'httpStatus', 'batchCreateCustomVocabularyItemResponse_httpStatus' - The response's http status code.
newBatchCreateCustomVocabularyItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchCreateCustomVocabularyItemResponse
newBatchCreateCustomVocabularyItemResponse
  pHttpStatus_ =
    BatchCreateCustomVocabularyItemResponse'
      { botId =
          Prelude.Nothing,
        botVersion = Prelude.Nothing,
        errors = Prelude.Nothing,
        localeId = Prelude.Nothing,
        resources = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique identifier of the bot to batch create response for the custom
-- vocabulary item.
batchCreateCustomVocabularyItemResponse_botId :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchCreateCustomVocabularyItemResponse_botId = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {botId} -> botId) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {botId = a} :: BatchCreateCustomVocabularyItemResponse)

-- | The bot version of the bot to batch create the custom vocabulary item
-- response for.
batchCreateCustomVocabularyItemResponse_botVersion :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchCreateCustomVocabularyItemResponse_botVersion = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {botVersion} -> botVersion) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {botVersion = a} :: BatchCreateCustomVocabularyItemResponse)

-- | The errors of the action to batch create the custom vocabulary item
-- response for a bot.
batchCreateCustomVocabularyItemResponse_errors :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe [FailedCustomVocabularyItem])
batchCreateCustomVocabularyItemResponse_errors = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {errors} -> errors) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {errors = a} :: BatchCreateCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique locale identifier of the bot to batch create the custom
-- vocabulary item response for.
batchCreateCustomVocabularyItemResponse_localeId :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchCreateCustomVocabularyItemResponse_localeId = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {localeId} -> localeId) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {localeId = a} :: BatchCreateCustomVocabularyItemResponse)

-- | The resources of the action to batch create the custom vocabulary item
-- response for a bot.
batchCreateCustomVocabularyItemResponse_resources :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe [CustomVocabularyItem])
batchCreateCustomVocabularyItemResponse_resources = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {resources} -> resources) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {resources = a} :: BatchCreateCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateCustomVocabularyItemResponse_httpStatus :: Lens.Lens' BatchCreateCustomVocabularyItemResponse Prelude.Int
batchCreateCustomVocabularyItemResponse_httpStatus = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {httpStatus} -> httpStatus) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {httpStatus = a} :: BatchCreateCustomVocabularyItemResponse)

instance
  Prelude.NFData
    BatchCreateCustomVocabularyItemResponse
  where
  rnf BatchCreateCustomVocabularyItemResponse' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botVersion `Prelude.seq`
        Prelude.rnf errors `Prelude.seq`
          Prelude.rnf localeId `Prelude.seq`
            Prelude.rnf resources `Prelude.seq`
              Prelude.rnf httpStatus
