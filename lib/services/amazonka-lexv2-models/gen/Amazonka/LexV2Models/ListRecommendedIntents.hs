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
-- Module      : Amazonka.LexV2Models.ListRecommendedIntents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of recommended intents provided by the bot recommendation
-- that you can use in your bot. Intents in the response are ordered by
-- relevance.
module Amazonka.LexV2Models.ListRecommendedIntents
  ( -- * Creating a Request
    ListRecommendedIntents (..),
    newListRecommendedIntents,

    -- * Request Lenses
    listRecommendedIntents_maxResults,
    listRecommendedIntents_nextToken,
    listRecommendedIntents_botId,
    listRecommendedIntents_botVersion,
    listRecommendedIntents_localeId,
    listRecommendedIntents_botRecommendationId,

    -- * Destructuring the Response
    ListRecommendedIntentsResponse (..),
    newListRecommendedIntentsResponse,

    -- * Response Lenses
    listRecommendedIntentsResponse_botId,
    listRecommendedIntentsResponse_botRecommendationId,
    listRecommendedIntentsResponse_botVersion,
    listRecommendedIntentsResponse_localeId,
    listRecommendedIntentsResponse_nextToken,
    listRecommendedIntentsResponse_summaryList,
    listRecommendedIntentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecommendedIntents' smart constructor.
data ListRecommendedIntents = ListRecommendedIntents'
  { -- | The maximum number of bot recommendations to return in each page of
    -- results. If there are fewer results than the max page size, only the
    -- actual number of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the ListRecommendedIntents operation contains more
    -- results than specified in the maxResults parameter, a token is returned
    -- in the response. Use that token in the nextToken parameter to return the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot associated with the recommended
    -- intents.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the recommended intents.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the recommended intents.
    localeId :: Prelude.Text,
    -- | The identifier of the bot recommendation that contains the recommended
    -- intents.
    botRecommendationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendedIntents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecommendedIntents_maxResults' - The maximum number of bot recommendations to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
--
-- 'nextToken', 'listRecommendedIntents_nextToken' - If the response from the ListRecommendedIntents operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
--
-- 'botId', 'listRecommendedIntents_botId' - The unique identifier of the bot associated with the recommended
-- intents.
--
-- 'botVersion', 'listRecommendedIntents_botVersion' - The version of the bot that contains the recommended intents.
--
-- 'localeId', 'listRecommendedIntents_localeId' - The identifier of the language and locale of the recommended intents.
--
-- 'botRecommendationId', 'listRecommendedIntents_botRecommendationId' - The identifier of the bot recommendation that contains the recommended
-- intents.
newListRecommendedIntents ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'botRecommendationId'
  Prelude.Text ->
  ListRecommendedIntents
newListRecommendedIntents
  pBotId_
  pBotVersion_
  pLocaleId_
  pBotRecommendationId_ =
    ListRecommendedIntents'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        botRecommendationId = pBotRecommendationId_
      }

-- | The maximum number of bot recommendations to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
listRecommendedIntents_maxResults :: Lens.Lens' ListRecommendedIntents (Prelude.Maybe Prelude.Natural)
listRecommendedIntents_maxResults = Lens.lens (\ListRecommendedIntents' {maxResults} -> maxResults) (\s@ListRecommendedIntents' {} a -> s {maxResults = a} :: ListRecommendedIntents)

-- | If the response from the ListRecommendedIntents operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
listRecommendedIntents_nextToken :: Lens.Lens' ListRecommendedIntents (Prelude.Maybe Prelude.Text)
listRecommendedIntents_nextToken = Lens.lens (\ListRecommendedIntents' {nextToken} -> nextToken) (\s@ListRecommendedIntents' {} a -> s {nextToken = a} :: ListRecommendedIntents)

-- | The unique identifier of the bot associated with the recommended
-- intents.
listRecommendedIntents_botId :: Lens.Lens' ListRecommendedIntents Prelude.Text
listRecommendedIntents_botId = Lens.lens (\ListRecommendedIntents' {botId} -> botId) (\s@ListRecommendedIntents' {} a -> s {botId = a} :: ListRecommendedIntents)

-- | The version of the bot that contains the recommended intents.
listRecommendedIntents_botVersion :: Lens.Lens' ListRecommendedIntents Prelude.Text
listRecommendedIntents_botVersion = Lens.lens (\ListRecommendedIntents' {botVersion} -> botVersion) (\s@ListRecommendedIntents' {} a -> s {botVersion = a} :: ListRecommendedIntents)

-- | The identifier of the language and locale of the recommended intents.
listRecommendedIntents_localeId :: Lens.Lens' ListRecommendedIntents Prelude.Text
listRecommendedIntents_localeId = Lens.lens (\ListRecommendedIntents' {localeId} -> localeId) (\s@ListRecommendedIntents' {} a -> s {localeId = a} :: ListRecommendedIntents)

-- | The identifier of the bot recommendation that contains the recommended
-- intents.
listRecommendedIntents_botRecommendationId :: Lens.Lens' ListRecommendedIntents Prelude.Text
listRecommendedIntents_botRecommendationId = Lens.lens (\ListRecommendedIntents' {botRecommendationId} -> botRecommendationId) (\s@ListRecommendedIntents' {} a -> s {botRecommendationId = a} :: ListRecommendedIntents)

instance Core.AWSRequest ListRecommendedIntents where
  type
    AWSResponse ListRecommendedIntents =
      ListRecommendedIntentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecommendedIntentsResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botRecommendationId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "summaryList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecommendedIntents where
  hashWithSalt _salt ListRecommendedIntents' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` botRecommendationId

instance Prelude.NFData ListRecommendedIntents where
  rnf ListRecommendedIntents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botRecommendationId

instance Data.ToHeaders ListRecommendedIntents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRecommendedIntents where
  toJSON ListRecommendedIntents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListRecommendedIntents where
  toPath ListRecommendedIntents' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/botrecommendations/",
        Data.toBS botRecommendationId,
        "/intents"
      ]

instance Data.ToQuery ListRecommendedIntents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecommendedIntentsResponse' smart constructor.
data ListRecommendedIntentsResponse = ListRecommendedIntentsResponse'
  { -- | The unique identifier of the bot associated with the recommended intent.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot recommendation that contains the recommended
    -- intent.
    botRecommendationId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that contains the intent.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the language and locale of the intents to list. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the ListRecommendedIntents operation. If the nextToken field
    -- is present, you send the contents as the nextToken parameter of a
    -- ListRecommendedIntents operation request to get the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the intents that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- maxResults parameter of the request. If there are more intents
    -- available, the nextToken field contains a token to get the next page of
    -- results.
    summaryList :: Prelude.Maybe [RecommendedIntentSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendedIntentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'listRecommendedIntentsResponse_botId' - The unique identifier of the bot associated with the recommended intent.
--
-- 'botRecommendationId', 'listRecommendedIntentsResponse_botRecommendationId' - The identifier of the bot recommendation that contains the recommended
-- intent.
--
-- 'botVersion', 'listRecommendedIntentsResponse_botVersion' - The version of the bot that contains the intent.
--
-- 'localeId', 'listRecommendedIntentsResponse_localeId' - The identifier of the language and locale of the intents to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'nextToken', 'listRecommendedIntentsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the ListRecommendedIntents operation. If the nextToken field
-- is present, you send the contents as the nextToken parameter of a
-- ListRecommendedIntents operation request to get the next page of
-- results.
--
-- 'summaryList', 'listRecommendedIntentsResponse_summaryList' - Summary information for the intents that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- maxResults parameter of the request. If there are more intents
-- available, the nextToken field contains a token to get the next page of
-- results.
--
-- 'httpStatus', 'listRecommendedIntentsResponse_httpStatus' - The response's http status code.
newListRecommendedIntentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecommendedIntentsResponse
newListRecommendedIntentsResponse pHttpStatus_ =
  ListRecommendedIntentsResponse'
    { botId =
        Prelude.Nothing,
      botRecommendationId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      summaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot associated with the recommended intent.
listRecommendedIntentsResponse_botId :: Lens.Lens' ListRecommendedIntentsResponse (Prelude.Maybe Prelude.Text)
listRecommendedIntentsResponse_botId = Lens.lens (\ListRecommendedIntentsResponse' {botId} -> botId) (\s@ListRecommendedIntentsResponse' {} a -> s {botId = a} :: ListRecommendedIntentsResponse)

-- | The identifier of the bot recommendation that contains the recommended
-- intent.
listRecommendedIntentsResponse_botRecommendationId :: Lens.Lens' ListRecommendedIntentsResponse (Prelude.Maybe Prelude.Text)
listRecommendedIntentsResponse_botRecommendationId = Lens.lens (\ListRecommendedIntentsResponse' {botRecommendationId} -> botRecommendationId) (\s@ListRecommendedIntentsResponse' {} a -> s {botRecommendationId = a} :: ListRecommendedIntentsResponse)

-- | The version of the bot that contains the intent.
listRecommendedIntentsResponse_botVersion :: Lens.Lens' ListRecommendedIntentsResponse (Prelude.Maybe Prelude.Text)
listRecommendedIntentsResponse_botVersion = Lens.lens (\ListRecommendedIntentsResponse' {botVersion} -> botVersion) (\s@ListRecommendedIntentsResponse' {} a -> s {botVersion = a} :: ListRecommendedIntentsResponse)

-- | The identifier of the language and locale of the intents to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
listRecommendedIntentsResponse_localeId :: Lens.Lens' ListRecommendedIntentsResponse (Prelude.Maybe Prelude.Text)
listRecommendedIntentsResponse_localeId = Lens.lens (\ListRecommendedIntentsResponse' {localeId} -> localeId) (\s@ListRecommendedIntentsResponse' {} a -> s {localeId = a} :: ListRecommendedIntentsResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the ListRecommendedIntents operation. If the nextToken field
-- is present, you send the contents as the nextToken parameter of a
-- ListRecommendedIntents operation request to get the next page of
-- results.
listRecommendedIntentsResponse_nextToken :: Lens.Lens' ListRecommendedIntentsResponse (Prelude.Maybe Prelude.Text)
listRecommendedIntentsResponse_nextToken = Lens.lens (\ListRecommendedIntentsResponse' {nextToken} -> nextToken) (\s@ListRecommendedIntentsResponse' {} a -> s {nextToken = a} :: ListRecommendedIntentsResponse)

-- | Summary information for the intents that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- maxResults parameter of the request. If there are more intents
-- available, the nextToken field contains a token to get the next page of
-- results.
listRecommendedIntentsResponse_summaryList :: Lens.Lens' ListRecommendedIntentsResponse (Prelude.Maybe [RecommendedIntentSummary])
listRecommendedIntentsResponse_summaryList = Lens.lens (\ListRecommendedIntentsResponse' {summaryList} -> summaryList) (\s@ListRecommendedIntentsResponse' {} a -> s {summaryList = a} :: ListRecommendedIntentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecommendedIntentsResponse_httpStatus :: Lens.Lens' ListRecommendedIntentsResponse Prelude.Int
listRecommendedIntentsResponse_httpStatus = Lens.lens (\ListRecommendedIntentsResponse' {httpStatus} -> httpStatus) (\s@ListRecommendedIntentsResponse' {} a -> s {httpStatus = a} :: ListRecommendedIntentsResponse)

instance
  Prelude.NFData
    ListRecommendedIntentsResponse
  where
  rnf ListRecommendedIntentsResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botRecommendationId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaryList
      `Prelude.seq` Prelude.rnf httpStatus
