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
-- Module      : Amazonka.LexV2Models.BatchDeleteCustomVocabularyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Batch delete custom vocabulary item for the specified locale in the
-- specified bot.
module Amazonka.LexV2Models.BatchDeleteCustomVocabularyItem
  ( -- * Creating a Request
    BatchDeleteCustomVocabularyItem (..),
    newBatchDeleteCustomVocabularyItem,

    -- * Request Lenses
    batchDeleteCustomVocabularyItem_botId,
    batchDeleteCustomVocabularyItem_botVersion,
    batchDeleteCustomVocabularyItem_localeId,
    batchDeleteCustomVocabularyItem_customVocabularyItemList,

    -- * Destructuring the Response
    BatchDeleteCustomVocabularyItemResponse (..),
    newBatchDeleteCustomVocabularyItemResponse,

    -- * Response Lenses
    batchDeleteCustomVocabularyItemResponse_botId,
    batchDeleteCustomVocabularyItemResponse_botVersion,
    batchDeleteCustomVocabularyItemResponse_errors,
    batchDeleteCustomVocabularyItemResponse_localeId,
    batchDeleteCustomVocabularyItemResponse_resources,
    batchDeleteCustomVocabularyItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteCustomVocabularyItem' smart constructor.
data BatchDeleteCustomVocabularyItem = BatchDeleteCustomVocabularyItem'
  { -- | The unique identifier of the bot to batch delete request for the custom
    -- vocabulary item.
    botId :: Prelude.Text,
    -- | The version of the bot to batch delete request for the custom vocabulary
    -- item.
    botVersion :: Prelude.Text,
    -- | The locale identifier of the bot to batch delete request for the custom
    -- vocabulary item.
    localeId :: Prelude.Text,
    -- | The custom vocabulary list to batch delete request for the custom
    -- vocabulary item.
    customVocabularyItemList :: Prelude.NonEmpty CustomVocabularyEntryId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteCustomVocabularyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'batchDeleteCustomVocabularyItem_botId' - The unique identifier of the bot to batch delete request for the custom
-- vocabulary item.
--
-- 'botVersion', 'batchDeleteCustomVocabularyItem_botVersion' - The version of the bot to batch delete request for the custom vocabulary
-- item.
--
-- 'localeId', 'batchDeleteCustomVocabularyItem_localeId' - The locale identifier of the bot to batch delete request for the custom
-- vocabulary item.
--
-- 'customVocabularyItemList', 'batchDeleteCustomVocabularyItem_customVocabularyItemList' - The custom vocabulary list to batch delete request for the custom
-- vocabulary item.
newBatchDeleteCustomVocabularyItem ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'customVocabularyItemList'
  Prelude.NonEmpty CustomVocabularyEntryId ->
  BatchDeleteCustomVocabularyItem
newBatchDeleteCustomVocabularyItem
  pBotId_
  pBotVersion_
  pLocaleId_
  pCustomVocabularyItemList_ =
    BatchDeleteCustomVocabularyItem'
      { botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        customVocabularyItemList =
          Lens.coerced
            Lens.# pCustomVocabularyItemList_
      }

-- | The unique identifier of the bot to batch delete request for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItem_botId :: Lens.Lens' BatchDeleteCustomVocabularyItem Prelude.Text
batchDeleteCustomVocabularyItem_botId = Lens.lens (\BatchDeleteCustomVocabularyItem' {botId} -> botId) (\s@BatchDeleteCustomVocabularyItem' {} a -> s {botId = a} :: BatchDeleteCustomVocabularyItem)

-- | The version of the bot to batch delete request for the custom vocabulary
-- item.
batchDeleteCustomVocabularyItem_botVersion :: Lens.Lens' BatchDeleteCustomVocabularyItem Prelude.Text
batchDeleteCustomVocabularyItem_botVersion = Lens.lens (\BatchDeleteCustomVocabularyItem' {botVersion} -> botVersion) (\s@BatchDeleteCustomVocabularyItem' {} a -> s {botVersion = a} :: BatchDeleteCustomVocabularyItem)

-- | The locale identifier of the bot to batch delete request for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItem_localeId :: Lens.Lens' BatchDeleteCustomVocabularyItem Prelude.Text
batchDeleteCustomVocabularyItem_localeId = Lens.lens (\BatchDeleteCustomVocabularyItem' {localeId} -> localeId) (\s@BatchDeleteCustomVocabularyItem' {} a -> s {localeId = a} :: BatchDeleteCustomVocabularyItem)

-- | The custom vocabulary list to batch delete request for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItem_customVocabularyItemList :: Lens.Lens' BatchDeleteCustomVocabularyItem (Prelude.NonEmpty CustomVocabularyEntryId)
batchDeleteCustomVocabularyItem_customVocabularyItemList = Lens.lens (\BatchDeleteCustomVocabularyItem' {customVocabularyItemList} -> customVocabularyItemList) (\s@BatchDeleteCustomVocabularyItem' {} a -> s {customVocabularyItemList = a} :: BatchDeleteCustomVocabularyItem) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchDeleteCustomVocabularyItem
  where
  type
    AWSResponse BatchDeleteCustomVocabularyItem =
      BatchDeleteCustomVocabularyItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteCustomVocabularyItemResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchDeleteCustomVocabularyItem
  where
  hashWithSalt
    _salt
    BatchDeleteCustomVocabularyItem' {..} =
      _salt `Prelude.hashWithSalt` botId
        `Prelude.hashWithSalt` botVersion
        `Prelude.hashWithSalt` localeId
        `Prelude.hashWithSalt` customVocabularyItemList

instance
  Prelude.NFData
    BatchDeleteCustomVocabularyItem
  where
  rnf BatchDeleteCustomVocabularyItem' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf customVocabularyItemList

instance
  Data.ToHeaders
    BatchDeleteCustomVocabularyItem
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

instance Data.ToJSON BatchDeleteCustomVocabularyItem where
  toJSON BatchDeleteCustomVocabularyItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "customVocabularyItemList"
                  Data..= customVocabularyItemList
              )
          ]
      )

instance Data.ToPath BatchDeleteCustomVocabularyItem where
  toPath BatchDeleteCustomVocabularyItem' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/customvocabulary/DEFAULT/batchdelete"
      ]

instance Data.ToQuery BatchDeleteCustomVocabularyItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteCustomVocabularyItemResponse' smart constructor.
data BatchDeleteCustomVocabularyItemResponse = BatchDeleteCustomVocabularyItemResponse'
  { -- | The unique identifier of the bot to batch delete response for the custom
    -- vocabulary item.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot to batch delete response for the custom
    -- vocabulary item.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The errors of the action to batch delete response for the custom
    -- vocabulary item.
    errors :: Prelude.Maybe [FailedCustomVocabularyItem],
    -- | The locale identifier of the bot to batch delete response for the custom
    -- vocabulary item.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The resources of the action to batch delete response for the custom
    -- vocabulary item.
    resources :: Prelude.Maybe [CustomVocabularyItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteCustomVocabularyItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'batchDeleteCustomVocabularyItemResponse_botId' - The unique identifier of the bot to batch delete response for the custom
-- vocabulary item.
--
-- 'botVersion', 'batchDeleteCustomVocabularyItemResponse_botVersion' - The version of the bot to batch delete response for the custom
-- vocabulary item.
--
-- 'errors', 'batchDeleteCustomVocabularyItemResponse_errors' - The errors of the action to batch delete response for the custom
-- vocabulary item.
--
-- 'localeId', 'batchDeleteCustomVocabularyItemResponse_localeId' - The locale identifier of the bot to batch delete response for the custom
-- vocabulary item.
--
-- 'resources', 'batchDeleteCustomVocabularyItemResponse_resources' - The resources of the action to batch delete response for the custom
-- vocabulary item.
--
-- 'httpStatus', 'batchDeleteCustomVocabularyItemResponse_httpStatus' - The response's http status code.
newBatchDeleteCustomVocabularyItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteCustomVocabularyItemResponse
newBatchDeleteCustomVocabularyItemResponse
  pHttpStatus_ =
    BatchDeleteCustomVocabularyItemResponse'
      { botId =
          Prelude.Nothing,
        botVersion = Prelude.Nothing,
        errors = Prelude.Nothing,
        localeId = Prelude.Nothing,
        resources = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique identifier of the bot to batch delete response for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItemResponse_botId :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchDeleteCustomVocabularyItemResponse_botId = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {botId} -> botId) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {botId = a} :: BatchDeleteCustomVocabularyItemResponse)

-- | The version of the bot to batch delete response for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItemResponse_botVersion :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchDeleteCustomVocabularyItemResponse_botVersion = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {botVersion} -> botVersion) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {botVersion = a} :: BatchDeleteCustomVocabularyItemResponse)

-- | The errors of the action to batch delete response for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItemResponse_errors :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe [FailedCustomVocabularyItem])
batchDeleteCustomVocabularyItemResponse_errors = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {errors} -> errors) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {errors = a} :: BatchDeleteCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The locale identifier of the bot to batch delete response for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItemResponse_localeId :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchDeleteCustomVocabularyItemResponse_localeId = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {localeId} -> localeId) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {localeId = a} :: BatchDeleteCustomVocabularyItemResponse)

-- | The resources of the action to batch delete response for the custom
-- vocabulary item.
batchDeleteCustomVocabularyItemResponse_resources :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe [CustomVocabularyItem])
batchDeleteCustomVocabularyItemResponse_resources = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {resources} -> resources) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {resources = a} :: BatchDeleteCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteCustomVocabularyItemResponse_httpStatus :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse Prelude.Int
batchDeleteCustomVocabularyItemResponse_httpStatus = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {httpStatus = a} :: BatchDeleteCustomVocabularyItemResponse)

instance
  Prelude.NFData
    BatchDeleteCustomVocabularyItemResponse
  where
  rnf BatchDeleteCustomVocabularyItemResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
