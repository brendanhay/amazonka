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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a batch of custom vocabulary items for a given bot locale\'s
-- custom vocabulary.
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
    -- | A list of new custom vocabulary items. Each entry must contain a phrase
    -- and can optionally contain a displayAs and\/or a weight.
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
-- 'botId', 'batchCreateCustomVocabularyItem_botId' - The identifier of the bot associated with this custom vocabulary.
--
-- 'botVersion', 'batchCreateCustomVocabularyItem_botVersion' - The identifier of the version of the bot associated with this custom
-- vocabulary.
--
-- 'localeId', 'batchCreateCustomVocabularyItem_localeId' - The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
--
-- 'customVocabularyItemList', 'batchCreateCustomVocabularyItem_customVocabularyItemList' - A list of new custom vocabulary items. Each entry must contain a phrase
-- and can optionally contain a displayAs and\/or a weight.
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

-- | The identifier of the bot associated with this custom vocabulary.
batchCreateCustomVocabularyItem_botId :: Lens.Lens' BatchCreateCustomVocabularyItem Prelude.Text
batchCreateCustomVocabularyItem_botId = Lens.lens (\BatchCreateCustomVocabularyItem' {botId} -> botId) (\s@BatchCreateCustomVocabularyItem' {} a -> s {botId = a} :: BatchCreateCustomVocabularyItem)

-- | The identifier of the version of the bot associated with this custom
-- vocabulary.
batchCreateCustomVocabularyItem_botVersion :: Lens.Lens' BatchCreateCustomVocabularyItem Prelude.Text
batchCreateCustomVocabularyItem_botVersion = Lens.lens (\BatchCreateCustomVocabularyItem' {botVersion} -> botVersion) (\s@BatchCreateCustomVocabularyItem' {} a -> s {botVersion = a} :: BatchCreateCustomVocabularyItem)

-- | The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
batchCreateCustomVocabularyItem_localeId :: Lens.Lens' BatchCreateCustomVocabularyItem Prelude.Text
batchCreateCustomVocabularyItem_localeId = Lens.lens (\BatchCreateCustomVocabularyItem' {localeId} -> localeId) (\s@BatchCreateCustomVocabularyItem' {} a -> s {localeId = a} :: BatchCreateCustomVocabularyItem)

-- | A list of new custom vocabulary items. Each entry must contain a phrase
-- and can optionally contain a displayAs and\/or a weight.
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
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf customVocabularyItemList

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
  { -- | The identifier of the bot associated with this custom vocabulary.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the version of the bot associated with this custom
    -- vocabulary.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of custom vocabulary items that failed to create during the
    -- operation. The reason for the error is contained within each error
    -- object.
    errors :: Prelude.Maybe [FailedCustomVocabularyItem],
    -- | The identifier of the language and locale where this custom vocabulary
    -- is used. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
    -- .
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A list of custom vocabulary items that were successfully created during
    -- the operation.
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
-- 'botId', 'batchCreateCustomVocabularyItemResponse_botId' - The identifier of the bot associated with this custom vocabulary.
--
-- 'botVersion', 'batchCreateCustomVocabularyItemResponse_botVersion' - The identifier of the version of the bot associated with this custom
-- vocabulary.
--
-- 'errors', 'batchCreateCustomVocabularyItemResponse_errors' - A list of custom vocabulary items that failed to create during the
-- operation. The reason for the error is contained within each error
-- object.
--
-- 'localeId', 'batchCreateCustomVocabularyItemResponse_localeId' - The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
--
-- 'resources', 'batchCreateCustomVocabularyItemResponse_resources' - A list of custom vocabulary items that were successfully created during
-- the operation.
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

-- | The identifier of the bot associated with this custom vocabulary.
batchCreateCustomVocabularyItemResponse_botId :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchCreateCustomVocabularyItemResponse_botId = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {botId} -> botId) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {botId = a} :: BatchCreateCustomVocabularyItemResponse)

-- | The identifier of the version of the bot associated with this custom
-- vocabulary.
batchCreateCustomVocabularyItemResponse_botVersion :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchCreateCustomVocabularyItemResponse_botVersion = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {botVersion} -> botVersion) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {botVersion = a} :: BatchCreateCustomVocabularyItemResponse)

-- | A list of custom vocabulary items that failed to create during the
-- operation. The reason for the error is contained within each error
-- object.
batchCreateCustomVocabularyItemResponse_errors :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe [FailedCustomVocabularyItem])
batchCreateCustomVocabularyItemResponse_errors = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {errors} -> errors) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {errors = a} :: BatchCreateCustomVocabularyItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
batchCreateCustomVocabularyItemResponse_localeId :: Lens.Lens' BatchCreateCustomVocabularyItemResponse (Prelude.Maybe Prelude.Text)
batchCreateCustomVocabularyItemResponse_localeId = Lens.lens (\BatchCreateCustomVocabularyItemResponse' {localeId} -> localeId) (\s@BatchCreateCustomVocabularyItemResponse' {} a -> s {localeId = a} :: BatchCreateCustomVocabularyItemResponse)

-- | A list of custom vocabulary items that were successfully created during
-- the operation.
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
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
