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
-- Delete a batch of custom vocabulary items for a given bot locale\'s
-- custom vocabulary.
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
  { -- | The identifier of the bot associated with this custom vocabulary.
    botId :: Prelude.Text,
    -- | The identifier of the version of the bot associated with this custom
    -- vocabulary.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale where this custom vocabulary
    -- is used. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
    -- .
    localeId :: Prelude.Text,
    -- | A list of custom vocabulary items requested to be deleted. Each entry
    -- must contain the unique custom vocabulary entry identifier.
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
-- 'botId', 'batchDeleteCustomVocabularyItem_botId' - The identifier of the bot associated with this custom vocabulary.
--
-- 'botVersion', 'batchDeleteCustomVocabularyItem_botVersion' - The identifier of the version of the bot associated with this custom
-- vocabulary.
--
-- 'localeId', 'batchDeleteCustomVocabularyItem_localeId' - The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
--
-- 'customVocabularyItemList', 'batchDeleteCustomVocabularyItem_customVocabularyItemList' - A list of custom vocabulary items requested to be deleted. Each entry
-- must contain the unique custom vocabulary entry identifier.
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

-- | The identifier of the bot associated with this custom vocabulary.
batchDeleteCustomVocabularyItem_botId :: Lens.Lens' BatchDeleteCustomVocabularyItem Prelude.Text
batchDeleteCustomVocabularyItem_botId = Lens.lens (\BatchDeleteCustomVocabularyItem' {botId} -> botId) (\s@BatchDeleteCustomVocabularyItem' {} a -> s {botId = a} :: BatchDeleteCustomVocabularyItem)

-- | The identifier of the version of the bot associated with this custom
-- vocabulary.
batchDeleteCustomVocabularyItem_botVersion :: Lens.Lens' BatchDeleteCustomVocabularyItem Prelude.Text
batchDeleteCustomVocabularyItem_botVersion = Lens.lens (\BatchDeleteCustomVocabularyItem' {botVersion} -> botVersion) (\s@BatchDeleteCustomVocabularyItem' {} a -> s {botVersion = a} :: BatchDeleteCustomVocabularyItem)

-- | The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
batchDeleteCustomVocabularyItem_localeId :: Lens.Lens' BatchDeleteCustomVocabularyItem Prelude.Text
batchDeleteCustomVocabularyItem_localeId = Lens.lens (\BatchDeleteCustomVocabularyItem' {localeId} -> localeId) (\s@BatchDeleteCustomVocabularyItem' {} a -> s {localeId = a} :: BatchDeleteCustomVocabularyItem)

-- | A list of custom vocabulary items requested to be deleted. Each entry
-- must contain the unique custom vocabulary entry identifier.
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
      _salt
        `Prelude.hashWithSalt` botId
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
  { -- | The identifier of the bot associated with this custom vocabulary.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the version of the bot associated with this custom
    -- vocabulary.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of custom vocabulary items that failed to delete during the
    -- operation. The reason for the error is contained within each error
    -- object.
    errors :: Prelude.Maybe [FailedCustomVocabularyItem],
    -- | The identifier of the language and locale where this custom vocabulary
    -- is used. The string must match one of the supported locales. For more
    -- information, see Supported languages
    -- (https:\/\/docs.aws.amazon.com\/lexv2\/latest\/dg\/how-languages.html).
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A list of custom vocabulary items that were successfully deleted during
    -- the operation.
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
-- 'botId', 'batchDeleteCustomVocabularyItemResponse_botId' - The identifier of the bot associated with this custom vocabulary.
--
-- 'botVersion', 'batchDeleteCustomVocabularyItemResponse_botVersion' - The identifier of the version of the bot associated with this custom
-- vocabulary.
--
-- 'errors', 'batchDeleteCustomVocabularyItemResponse_errors' - A list of custom vocabulary items that failed to delete during the
-- operation. The reason for the error is contained within each error
-- object.
--
-- 'localeId', 'batchDeleteCustomVocabularyItemResponse_localeId' - The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see Supported languages
-- (https:\/\/docs.aws.amazon.com\/lexv2\/latest\/dg\/how-languages.html).
--
-- 'resources', 'batchDeleteCustomVocabularyItemResponse_resources' - A list of custom vocabulary items that were successfully deleted during
-- the operation.
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

-- | The identifier of the bot associated with this custom vocabulary.
batchDeleteCustomVocabularyItemResponse_botId :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchDeleteCustomVocabularyItemResponse_botId = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {botId} -> botId) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {botId = a} :: BatchDeleteCustomVocabularyItemResponse)

-- | The identifier of the version of the bot associated with this custom
-- vocabulary.
batchDeleteCustomVocabularyItemResponse_botVersion :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchDeleteCustomVocabularyItemResponse_botVersion = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {botVersion} -> botVersion) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {botVersion = a} :: BatchDeleteCustomVocabularyItemResponse)

-- | A list of custom vocabulary items that failed to delete during the
-- operation. The reason for the error is contained within each error
-- object.
batchDeleteCustomVocabularyItemResponse_errors :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe [FailedCustomVocabularyItem])
batchDeleteCustomVocabularyItemResponse_errors = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {errors} -> errors) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {errors = a} :: BatchDeleteCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see Supported languages
-- (https:\/\/docs.aws.amazon.com\/lexv2\/latest\/dg\/how-languages.html).
batchDeleteCustomVocabularyItemResponse_localeId :: Lens.Lens' BatchDeleteCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchDeleteCustomVocabularyItemResponse_localeId = Lens.lens (\BatchDeleteCustomVocabularyItemResponse' {localeId} -> localeId) (\s@BatchDeleteCustomVocabularyItemResponse' {} a -> s {localeId = a} :: BatchDeleteCustomVocabularyItemResponse)

-- | A list of custom vocabulary items that were successfully deleted during
-- the operation.
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
