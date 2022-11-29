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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    batchUpdateCustomVocabularyItemResponse_botVersion,
    batchUpdateCustomVocabularyItemResponse_localeId,
    batchUpdateCustomVocabularyItemResponse_botId,
    batchUpdateCustomVocabularyItemResponse_errors,
    batchUpdateCustomVocabularyItemResponse_resources,
    batchUpdateCustomVocabularyItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "resources" Core..!@ Prelude.mempty)
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
  Core.ToHeaders
    BatchUpdateCustomVocabularyItem
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchUpdateCustomVocabularyItem where
  toJSON BatchUpdateCustomVocabularyItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "customVocabularyItemList"
                  Core..= customVocabularyItemList
              )
          ]
      )

instance Core.ToPath BatchUpdateCustomVocabularyItem where
  toPath BatchUpdateCustomVocabularyItem' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/customvocabulary/DEFAULT/batchupdate"
      ]

instance Core.ToQuery BatchUpdateCustomVocabularyItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateCustomVocabularyItemResponse' smart constructor.
data BatchUpdateCustomVocabularyItemResponse = BatchUpdateCustomVocabularyItemResponse'
  { -- | The bot version of the bot to the batch update response for the custom
    -- vocabulary item.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The locale identifier of the bot to the batch update response for the
    -- custom vocabulary item.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot to the batch update response for the
    -- custom vocabulary item.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The errors of the action to batch update response for the custom
    -- vocabulary item.
    errors :: Prelude.Maybe [FailedCustomVocabularyItem],
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
-- 'botVersion', 'batchUpdateCustomVocabularyItemResponse_botVersion' - The bot version of the bot to the batch update response for the custom
-- vocabulary item.
--
-- 'localeId', 'batchUpdateCustomVocabularyItemResponse_localeId' - The locale identifier of the bot to the batch update response for the
-- custom vocabulary item.
--
-- 'botId', 'batchUpdateCustomVocabularyItemResponse_botId' - The unique identifier of the bot to the batch update response for the
-- custom vocabulary item.
--
-- 'errors', 'batchUpdateCustomVocabularyItemResponse_errors' - The errors of the action to batch update response for the custom
-- vocabulary item.
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
      { botVersion =
          Prelude.Nothing,
        localeId = Prelude.Nothing,
        botId = Prelude.Nothing,
        errors = Prelude.Nothing,
        resources = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The bot version of the bot to the batch update response for the custom
-- vocabulary item.
batchUpdateCustomVocabularyItemResponse_botVersion :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchUpdateCustomVocabularyItemResponse_botVersion = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {botVersion} -> botVersion) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {botVersion = a} :: BatchUpdateCustomVocabularyItemResponse)

-- | The locale identifier of the bot to the batch update response for the
-- custom vocabulary item.
batchUpdateCustomVocabularyItemResponse_localeId :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchUpdateCustomVocabularyItemResponse_localeId = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {localeId} -> localeId) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {localeId = a} :: BatchUpdateCustomVocabularyItemResponse)

-- | The unique identifier of the bot to the batch update response for the
-- custom vocabulary item.
batchUpdateCustomVocabularyItemResponse_botId :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchUpdateCustomVocabularyItemResponse_botId = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {botId} -> botId) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {botId = a} :: BatchUpdateCustomVocabularyItemResponse)

-- | The errors of the action to batch update response for the custom
-- vocabulary item.
batchUpdateCustomVocabularyItemResponse_errors :: Lens.Lens' BatchUpdateCustomVocabularyItemResponse (Prelude.Maybe [FailedCustomVocabularyItem])
batchUpdateCustomVocabularyItemResponse_errors = Lens.lens (\BatchUpdateCustomVocabularyItemResponse' {errors} -> errors) (\s@BatchUpdateCustomVocabularyItemResponse' {} a -> s {errors = a} :: BatchUpdateCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

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
    Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
