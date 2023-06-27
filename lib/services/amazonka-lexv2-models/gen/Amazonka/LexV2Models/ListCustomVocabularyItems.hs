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
-- Module      : Amazonka.LexV2Models.ListCustomVocabularyItems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Paginated list of custom vocabulary items for a given bot locale\'s
-- custom vocabulary.
module Amazonka.LexV2Models.ListCustomVocabularyItems
  ( -- * Creating a Request
    ListCustomVocabularyItems (..),
    newListCustomVocabularyItems,

    -- * Request Lenses
    listCustomVocabularyItems_maxResults,
    listCustomVocabularyItems_nextToken,
    listCustomVocabularyItems_botId,
    listCustomVocabularyItems_botVersion,
    listCustomVocabularyItems_localeId,

    -- * Destructuring the Response
    ListCustomVocabularyItemsResponse (..),
    newListCustomVocabularyItemsResponse,

    -- * Response Lenses
    listCustomVocabularyItemsResponse_botId,
    listCustomVocabularyItemsResponse_botVersion,
    listCustomVocabularyItemsResponse_customVocabularyItems,
    listCustomVocabularyItemsResponse_localeId,
    listCustomVocabularyItemsResponse_nextToken,
    listCustomVocabularyItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomVocabularyItems' smart constructor.
data ListCustomVocabularyItems = ListCustomVocabularyItems'
  { -- | The maximum number of items returned by the list operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken identifier to the list custom vocabulary request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the version of the bot associated with this custom
    -- vocabulary.
    botId :: Prelude.Text,
    -- | The bot version of the bot to the list custom vocabulary request.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale where this custom vocabulary
    -- is used. The string must match one of the supported locales. For more
    -- information, see Supported languages
    -- (https:\/\/docs.aws.amazon.com\/lexv2\/latest\/dg\/how-languages.html).
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomVocabularyItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCustomVocabularyItems_maxResults' - The maximum number of items returned by the list operation.
--
-- 'nextToken', 'listCustomVocabularyItems_nextToken' - The nextToken identifier to the list custom vocabulary request.
--
-- 'botId', 'listCustomVocabularyItems_botId' - The identifier of the version of the bot associated with this custom
-- vocabulary.
--
-- 'botVersion', 'listCustomVocabularyItems_botVersion' - The bot version of the bot to the list custom vocabulary request.
--
-- 'localeId', 'listCustomVocabularyItems_localeId' - The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see Supported languages
-- (https:\/\/docs.aws.amazon.com\/lexv2\/latest\/dg\/how-languages.html).
newListCustomVocabularyItems ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  ListCustomVocabularyItems
newListCustomVocabularyItems
  pBotId_
  pBotVersion_
  pLocaleId_ =
    ListCustomVocabularyItems'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The maximum number of items returned by the list operation.
listCustomVocabularyItems_maxResults :: Lens.Lens' ListCustomVocabularyItems (Prelude.Maybe Prelude.Natural)
listCustomVocabularyItems_maxResults = Lens.lens (\ListCustomVocabularyItems' {maxResults} -> maxResults) (\s@ListCustomVocabularyItems' {} a -> s {maxResults = a} :: ListCustomVocabularyItems)

-- | The nextToken identifier to the list custom vocabulary request.
listCustomVocabularyItems_nextToken :: Lens.Lens' ListCustomVocabularyItems (Prelude.Maybe Prelude.Text)
listCustomVocabularyItems_nextToken = Lens.lens (\ListCustomVocabularyItems' {nextToken} -> nextToken) (\s@ListCustomVocabularyItems' {} a -> s {nextToken = a} :: ListCustomVocabularyItems)

-- | The identifier of the version of the bot associated with this custom
-- vocabulary.
listCustomVocabularyItems_botId :: Lens.Lens' ListCustomVocabularyItems Prelude.Text
listCustomVocabularyItems_botId = Lens.lens (\ListCustomVocabularyItems' {botId} -> botId) (\s@ListCustomVocabularyItems' {} a -> s {botId = a} :: ListCustomVocabularyItems)

-- | The bot version of the bot to the list custom vocabulary request.
listCustomVocabularyItems_botVersion :: Lens.Lens' ListCustomVocabularyItems Prelude.Text
listCustomVocabularyItems_botVersion = Lens.lens (\ListCustomVocabularyItems' {botVersion} -> botVersion) (\s@ListCustomVocabularyItems' {} a -> s {botVersion = a} :: ListCustomVocabularyItems)

-- | The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see Supported languages
-- (https:\/\/docs.aws.amazon.com\/lexv2\/latest\/dg\/how-languages.html).
listCustomVocabularyItems_localeId :: Lens.Lens' ListCustomVocabularyItems Prelude.Text
listCustomVocabularyItems_localeId = Lens.lens (\ListCustomVocabularyItems' {localeId} -> localeId) (\s@ListCustomVocabularyItems' {} a -> s {localeId = a} :: ListCustomVocabularyItems)

instance Core.AWSRequest ListCustomVocabularyItems where
  type
    AWSResponse ListCustomVocabularyItems =
      ListCustomVocabularyItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomVocabularyItemsResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> ( x
                            Data..?> "customVocabularyItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomVocabularyItems where
  hashWithSalt _salt ListCustomVocabularyItems' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData ListCustomVocabularyItems where
  rnf ListCustomVocabularyItems' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToHeaders ListCustomVocabularyItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCustomVocabularyItems where
  toJSON ListCustomVocabularyItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCustomVocabularyItems where
  toPath ListCustomVocabularyItems' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/customvocabulary/DEFAULT/list"
      ]

instance Data.ToQuery ListCustomVocabularyItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomVocabularyItemsResponse' smart constructor.
data ListCustomVocabularyItemsResponse = ListCustomVocabularyItemsResponse'
  { -- | The identifier of the bot associated with this custom vocabulary.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the version of the bot associated with this custom
    -- vocabulary.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The custom vocabulary items from the list custom vocabulary response.
    customVocabularyItems :: Prelude.Maybe [CustomVocabularyItem],
    -- | The identifier of the language and locale where this custom vocabulary
    -- is used. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
    -- .
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The nextToken identifier to the list custom vocabulary response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomVocabularyItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'listCustomVocabularyItemsResponse_botId' - The identifier of the bot associated with this custom vocabulary.
--
-- 'botVersion', 'listCustomVocabularyItemsResponse_botVersion' - The identifier of the version of the bot associated with this custom
-- vocabulary.
--
-- 'customVocabularyItems', 'listCustomVocabularyItemsResponse_customVocabularyItems' - The custom vocabulary items from the list custom vocabulary response.
--
-- 'localeId', 'listCustomVocabularyItemsResponse_localeId' - The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
--
-- 'nextToken', 'listCustomVocabularyItemsResponse_nextToken' - The nextToken identifier to the list custom vocabulary response.
--
-- 'httpStatus', 'listCustomVocabularyItemsResponse_httpStatus' - The response's http status code.
newListCustomVocabularyItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomVocabularyItemsResponse
newListCustomVocabularyItemsResponse pHttpStatus_ =
  ListCustomVocabularyItemsResponse'
    { botId =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      customVocabularyItems = Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot associated with this custom vocabulary.
listCustomVocabularyItemsResponse_botId :: Lens.Lens' ListCustomVocabularyItemsResponse (Prelude.Maybe Prelude.Text)
listCustomVocabularyItemsResponse_botId = Lens.lens (\ListCustomVocabularyItemsResponse' {botId} -> botId) (\s@ListCustomVocabularyItemsResponse' {} a -> s {botId = a} :: ListCustomVocabularyItemsResponse)

-- | The identifier of the version of the bot associated with this custom
-- vocabulary.
listCustomVocabularyItemsResponse_botVersion :: Lens.Lens' ListCustomVocabularyItemsResponse (Prelude.Maybe Prelude.Text)
listCustomVocabularyItemsResponse_botVersion = Lens.lens (\ListCustomVocabularyItemsResponse' {botVersion} -> botVersion) (\s@ListCustomVocabularyItemsResponse' {} a -> s {botVersion = a} :: ListCustomVocabularyItemsResponse)

-- | The custom vocabulary items from the list custom vocabulary response.
listCustomVocabularyItemsResponse_customVocabularyItems :: Lens.Lens' ListCustomVocabularyItemsResponse (Prelude.Maybe [CustomVocabularyItem])
listCustomVocabularyItemsResponse_customVocabularyItems = Lens.lens (\ListCustomVocabularyItemsResponse' {customVocabularyItems} -> customVocabularyItems) (\s@ListCustomVocabularyItemsResponse' {} a -> s {customVocabularyItems = a} :: ListCustomVocabularyItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the language and locale where this custom vocabulary
-- is used. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported Languages>
-- .
listCustomVocabularyItemsResponse_localeId :: Lens.Lens' ListCustomVocabularyItemsResponse (Prelude.Maybe Prelude.Text)
listCustomVocabularyItemsResponse_localeId = Lens.lens (\ListCustomVocabularyItemsResponse' {localeId} -> localeId) (\s@ListCustomVocabularyItemsResponse' {} a -> s {localeId = a} :: ListCustomVocabularyItemsResponse)

-- | The nextToken identifier to the list custom vocabulary response.
listCustomVocabularyItemsResponse_nextToken :: Lens.Lens' ListCustomVocabularyItemsResponse (Prelude.Maybe Prelude.Text)
listCustomVocabularyItemsResponse_nextToken = Lens.lens (\ListCustomVocabularyItemsResponse' {nextToken} -> nextToken) (\s@ListCustomVocabularyItemsResponse' {} a -> s {nextToken = a} :: ListCustomVocabularyItemsResponse)

-- | The response's http status code.
listCustomVocabularyItemsResponse_httpStatus :: Lens.Lens' ListCustomVocabularyItemsResponse Prelude.Int
listCustomVocabularyItemsResponse_httpStatus = Lens.lens (\ListCustomVocabularyItemsResponse' {httpStatus} -> httpStatus) (\s@ListCustomVocabularyItemsResponse' {} a -> s {httpStatus = a} :: ListCustomVocabularyItemsResponse)

instance
  Prelude.NFData
    ListCustomVocabularyItemsResponse
  where
  rnf ListCustomVocabularyItemsResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf customVocabularyItems
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
