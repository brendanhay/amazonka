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
-- Module      : Amazonka.LexV2Models.BatchUpdateCustomVocabularyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Batch update custom vocabulary item for the specified locale in the
-- specified bot.
module Amazonka.LexV2Models.BatchUpdateCustomVocabularyItem
  ( -- * Creating a Request
    BatchUpdateCustomVocabularyItem (..),
    newBatchUpdateCustomVocabularyItem,

    -- * Request Lenses
    batchUpdateCustomVocabularyItem_botId,
    batchUpdateCustomVocabularyItem_botVersion,
    batchUpdateCustomVocabularyItem_localeId,
    batchUpdateCustomVocabularyItem_customVocabularyItemList,

    -- * Destructuring the Response
    BatchUpdateCustomVocabularyItemResponse (..),
    newBatchUpdateCustomVocabularyItemResponse,

    -- * Response Lenses
    batchUpdateCustomVocabularyItemResponse_botId,
    batchUpdateCustomVocabularyItemResponse_botVersion,
    batchUpdateCustomVocabularyItemResponse_errors,
    batchUpdateCustomVocabularyItemResponse_localeId,
    batchUpdateCustomVocabularyItemResponse_resources,
    batchUpdateCustomVocabularyItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateCustomVocabularyItem' smart constructor.
data BatchUpdateCustomVocabularyItem = BatchUpdateCustomVocabularyItem'
  { -- | The unique identifier of the bot to the batch update request for the
    -- custom vocabulary item.
    botId :: Prelude.Text,
    -- | The bot version of the bot to the batch update request for the custom
    -- vocabulary item.
    botVersion :: Prelude.Text,
    -- | The locale identifier of the bot to the batch update request for the
    -- custom vocabulary item.
    localeId :: Prelude.Text,
    -- | The custom vocabulary item list of the bot to the batch update request
    -- for the custom vocabulary item.
    customVocabularyItemList :: Prelude.NonEmpty CustomVocabularyItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateCustomVocabularyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'batchUpdateCustomVocabularyItem_botId' - The unique identifier of the bot to the batch update request for the
-- custom vocabulary item.
--
-- 'botVersion', 'batchUpdateCustomVocabularyItem_botVersion' - The bot version of the bot to the batch update request for the custom
-- vocabulary item.
--
-- 'localeId', 'batchUpdateCustomVocabularyItem_localeId' - The locale identifier of the bot to the batch update request for the
-- custom vocabulary item.
--
-- 'customVocabularyItemList', 'batchUpdateCustomVocabularyItem_customVocabularyItemList' - The custom vocabulary item list of the bot to the batch update request
-- for the custom vocabulary item.
newBatchUpdateCustomVocabularyItem ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'customVocabularyItemList'
  Prelude.NonEmpty CustomVocabularyItem ->
  BatchUpdateCustomVocabularyItem
newBatchUpdateCustomVocabularyItem
  pBotId_
  pBotVersion_
  pLocaleId_
  pCustomVocabularyItemList_ =
    BatchUpdateCustomVocabularyItem'
      { botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        customVocabularyItemList =
          Lens.coerced
            Lens.# pCustomVocabularyItemList_
      }

-- | The unique identifier of the bot to the batch update request for the
-- custom vocabulary item.
batchUpdateCustomVocabularyItem_botId :: Lens.Lens' BatchUpdateCustomVocabularyItem Prelude.Text
batchUpdateCustomVocabularyItem_botId = Lens.lens (\BatchUpdateCustomVocabularyItem' {botId} -> botId) (\s@BatchUpdateCustomVocabularyItem' {} a -> s {botId = a} :: BatchUpdateCustomVocabularyItem)

-- | The bot version of the bot to the batch update request for the custom
-- vocabulary item.
batchUpdateCustomVocabularyItem_botVersion :: Lens.Lens' BatchUpdateCustomVocabularyItem Prelude.Text
batchUpdateCustomVocabularyItem_botVersion = Lens.lens (\BatchUpdateCustomVocabularyItem' {botVersion} -> botVersion) (\s@BatchUpdateCustomVocabularyItem' {} a -> s {botVersion = a} :: BatchUpdateCustomVocabularyItem)

-- | The locale identifier of the bot to the batch update request for the
-- custom vocabulary item.
batchUpdateCustomVocabularyItem_localeId :: Lens.Lens' BatchUpdateCustomVocabularyItem Prelude.Text
batchUpdateCustomVocabularyItem_localeId = Lens.lens (\BatchUpdateCustomVocabularyItem' {localeId} -> localeId) (\s@BatchUpdateCustomVocabularyItem' {} a -> s {localeId = a} :: BatchUpdateCustomVocabularyItem)

-- | The custom vocabulary item list of the bot to the batch update request
-- for the custom vocabulary item.
batchUpdateCustomVocabularyItem_customVocabularyItemList :: Lens.Lens' BatchUpdateCustomVocabularyItem (Prelude.NonEmpty CustomVocabularyItem)
batchUpdateCustomVocabularyItem_customVocabularyItemList = Lens.lens (\BatchUpdateCustomVocabularyItem' {customVocabularyItemList} -> customVocabularyItemList) (\s@BatchUpdateCustomVocabularyItem' {} a -> s {customVocabularyItemList = a} :: BatchUpdateCustomVocabularyItem) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchUpdateCustomVocabularyItem
  where
  type
    AWSResponse BatchUpdateCustomVocabularyItem =
      BatchUpdateCustomVocabularyItemResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateCustomVocabularyItemResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchUpdateCustomVocabularyItem
  where
  hashWithSalt
    _salt
    BatchUpdateCustomVocabularyItem' {..} =
      _salt `Prelude.hashWithSalt` botId
        `Prelude.hashWithSalt` botVersion
        `Prelude.hashWithSalt` localeId
        `Prelude.hashWithSalt` customVocabularyItemList

instance
  Prelude.NFData
    BatchUpdateCustomVocabularyItem
  where
  rnf BatchUpdateCustomVocabularyItem' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf customVocabularyItemList

instance
  Data.ToHeaders
    BatchUpdateCustomVocabularyItem
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

instance Data.ToJSON BatchUpdateCustomVocabularyItem where
  toJSON BatchUpdateCustomVocabularyItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "customVocabularyItemList"
                  Data..= customVocabularyItemList
              )
          ]
      )

instance Data.ToPath BatchUpdateCustomVocabularyItem where
  toPath BatchUpdateCustomVocabularyItem' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/customvocabulary/DEFAULT/batchupdate"
      ]

instance Data.ToQuery BatchUpdateCustomVocabularyItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateCustomVocabularyItemResponse' smart constructor.
data BatchUpdateCustomVocabularyItemResponse = BatchUpdateCustomVocabularyItemResponse'
  { -- | The unique identifier of the bot to the batch update response for the
    -- custom vocabulary item.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The bot version of the bot to the batch update response for the custom
    -- vocabulary item.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The errors of the action to batch update response for the custom
    -- vocabulary item.
    errors :: Prelude.Maybe [FailedCustomVocabularyItem],
    -- | The locale identifier of the bot to the batch update response for the
    -- custom vocabulary item.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The resources of the action to batch update response for the custom
    -- vocabulary item.
    resources :: Prelude.Maybe [CustomVocabularyItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateCustomVocabularyItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'batchUpdateCustomVocabularyItemResponse_botId' - The unique identifier of the bot to the batch update response for the
-- custom vocabulary item.
--
-- 'botVersion', 'batchUpdateCustomVocabularyItemResponse_botVersion' - The bot version of the bot to the batch update response for the custom
-- vocabulary item.
--
-- 'errors', 'batchUpdateCustomVocabularyItemResponse_errors' - The errors of the action to batch update response for the custom
-- vocabulary item.
--
-- 'localeId', 'batchUpdateCustomVocabularyItemResponse_localeId' - The locale identifier of the bot to the batch update response for the
-- custom vocabulary item.
--
-- 'resources', 'batchUpdateCustomVocabularyItemResponse_resources' - The resources of the action to batch update response for the custom
-- vocabulary item.
--
-- 'httpStatus', 'batchUpdateCustomVocabularyItemResponse_httpStatus' - The response's http status code.
newBatchUpdateCustomVocabularyItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateCustomVocabularyItemResponse
newBatchUpdateCustomVocabularyItemResponse
  pHttpStatus_ =
    BatchUpdateCustomVocabularyItemResponse'
      { botId =
          Prelude.Nothing,
        botVersion = Prelude.Nothing,
        errors = Prelude.Nothing,
        localeId = Prelude.Nothing,
        resources = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique identifier of the bot to the batch update response for the
-- custom vocabulary item.
batchUpdateCustomVocabularyItemResponse_botId :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchUpdateCustomVocabularyItemResponse_botId = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {botId} -> botId) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {botId = a} :: BatchUpdateCustomVocabularyItemResponse)

-- | The bot version of the bot to the batch update response for the custom
-- vocabulary item.
batchUpdateCustomVocabularyItemResponse_botVersion :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchUpdateCustomVocabularyItemResponse_botVersion = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {botVersion} -> botVersion) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {botVersion = a} :: BatchUpdateCustomVocabularyItemResponse)

-- | The errors of the action to batch update response for the custom
-- vocabulary item.
batchUpdateCustomVocabularyItemResponse_errors :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe [FailedCustomVocabularyItem])
batchUpdateCustomVocabularyItemResponse_errors = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {errors} -> errors) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {errors = a} :: BatchUpdateCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The locale identifier of the bot to the batch update response for the
-- custom vocabulary item.
batchUpdateCustomVocabularyItemResponse_localeId :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchUpdateCustomVocabularyItemResponse_localeId = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {localeId} -> localeId) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {localeId = a} :: BatchUpdateCustomVocabularyItemResponse)

-- | The resources of the action to batch update response for the custom
-- vocabulary item.
batchUpdateCustomVocabularyItemResponse_resources :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe [CustomVocabularyItem])
batchUpdateCustomVocabularyItemResponse_resources = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {resources} -> resources) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {resources = a} :: BatchUpdateCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateCustomVocabularyItemResponse_httpStatus :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse Prelude.Int
batchUpdateCustomVocabularyItemResponse_httpStatus = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {httpStatus = a} :: BatchUpdateCustomVocabularyItemResponse)

instance
  Prelude.NFData
    BatchUpdateCustomVocabularyItemResponse
  where
  rnf BatchUpdateCustomVocabularyItemResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
