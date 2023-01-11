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
-- Module      : Amazonka.LexV2Models.ListBotRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of bot recommendations that meet the specified criteria.
module Amazonka.LexV2Models.ListBotRecommendations
  ( -- * Creating a Request
    ListBotRecommendations (..),
    newListBotRecommendations,

    -- * Request Lenses
    listBotRecommendations_maxResults,
    listBotRecommendations_nextToken,
    listBotRecommendations_botId,
    listBotRecommendations_botVersion,
    listBotRecommendations_localeId,

    -- * Destructuring the Response
    ListBotRecommendationsResponse (..),
    newListBotRecommendationsResponse,

    -- * Response Lenses
    listBotRecommendationsResponse_botId,
    listBotRecommendationsResponse_botRecommendationSummaries,
    listBotRecommendationsResponse_botVersion,
    listBotRecommendationsResponse_localeId,
    listBotRecommendationsResponse_nextToken,
    listBotRecommendationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBotRecommendations' smart constructor.
data ListBotRecommendations = ListBotRecommendations'
  { -- | The maximum number of bot recommendations to return in each page of
    -- results. If there are fewer results than the max page size, only the
    -- actual number of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the ListBotRecommendation operation contains more
    -- results than specified in the maxResults parameter, a token is returned
    -- in the response. Use that token in the nextToken parameter to return the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot that contains the bot recommendation
    -- list.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the bot recommendation list.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the bot recommendation
    -- list.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBotRecommendations_maxResults' - The maximum number of bot recommendations to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
--
-- 'nextToken', 'listBotRecommendations_nextToken' - If the response from the ListBotRecommendation operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
--
-- 'botId', 'listBotRecommendations_botId' - The unique identifier of the bot that contains the bot recommendation
-- list.
--
-- 'botVersion', 'listBotRecommendations_botVersion' - The version of the bot that contains the bot recommendation list.
--
-- 'localeId', 'listBotRecommendations_localeId' - The identifier of the language and locale of the bot recommendation
-- list.
newListBotRecommendations ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  ListBotRecommendations
newListBotRecommendations
  pBotId_
  pBotVersion_
  pLocaleId_ =
    ListBotRecommendations'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The maximum number of bot recommendations to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
listBotRecommendations_maxResults :: Lens.Lens' ListBotRecommendations (Prelude.Maybe Prelude.Natural)
listBotRecommendations_maxResults = Lens.lens (\ListBotRecommendations' {maxResults} -> maxResults) (\s@ListBotRecommendations' {} a -> s {maxResults = a} :: ListBotRecommendations)

-- | If the response from the ListBotRecommendation operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
listBotRecommendations_nextToken :: Lens.Lens' ListBotRecommendations (Prelude.Maybe Prelude.Text)
listBotRecommendations_nextToken = Lens.lens (\ListBotRecommendations' {nextToken} -> nextToken) (\s@ListBotRecommendations' {} a -> s {nextToken = a} :: ListBotRecommendations)

-- | The unique identifier of the bot that contains the bot recommendation
-- list.
listBotRecommendations_botId :: Lens.Lens' ListBotRecommendations Prelude.Text
listBotRecommendations_botId = Lens.lens (\ListBotRecommendations' {botId} -> botId) (\s@ListBotRecommendations' {} a -> s {botId = a} :: ListBotRecommendations)

-- | The version of the bot that contains the bot recommendation list.
listBotRecommendations_botVersion :: Lens.Lens' ListBotRecommendations Prelude.Text
listBotRecommendations_botVersion = Lens.lens (\ListBotRecommendations' {botVersion} -> botVersion) (\s@ListBotRecommendations' {} a -> s {botVersion = a} :: ListBotRecommendations)

-- | The identifier of the language and locale of the bot recommendation
-- list.
listBotRecommendations_localeId :: Lens.Lens' ListBotRecommendations Prelude.Text
listBotRecommendations_localeId = Lens.lens (\ListBotRecommendations' {localeId} -> localeId) (\s@ListBotRecommendations' {} a -> s {localeId = a} :: ListBotRecommendations)

instance Core.AWSRequest ListBotRecommendations where
  type
    AWSResponse ListBotRecommendations =
      ListBotRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotRecommendationsResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> ( x Data..?> "botRecommendationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBotRecommendations where
  hashWithSalt _salt ListBotRecommendations' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData ListBotRecommendations where
  rnf ListBotRecommendations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToHeaders ListBotRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBotRecommendations where
  toJSON ListBotRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListBotRecommendations where
  toPath ListBotRecommendations' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/botrecommendations/"
      ]

instance Data.ToQuery ListBotRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBotRecommendationsResponse' smart constructor.
data ListBotRecommendationsResponse = ListBotRecommendationsResponse'
  { -- | The unique identifier of the bot that contains the bot recommendation
    -- list.
    botId :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the bot recommendations that meet the filter
    -- specified in this request. The length of the list is specified in the
    -- maxResults parameter of the request. If there are more bot
    -- recommendations available, the nextToken field contains a token to get
    -- the next page of results.
    botRecommendationSummaries :: Prelude.Maybe [BotRecommendationSummary],
    -- | The version of the bot that contains the bot recommendation list.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the language and locale of the bot recommendation
    -- list.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the ListBotRecommendations operation. If the nextToken field
    -- is present, you send the contents as the nextToken parameter of a
    -- ListBotRecommendations operation request to get the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'listBotRecommendationsResponse_botId' - The unique identifier of the bot that contains the bot recommendation
-- list.
--
-- 'botRecommendationSummaries', 'listBotRecommendationsResponse_botRecommendationSummaries' - Summary information for the bot recommendations that meet the filter
-- specified in this request. The length of the list is specified in the
-- maxResults parameter of the request. If there are more bot
-- recommendations available, the nextToken field contains a token to get
-- the next page of results.
--
-- 'botVersion', 'listBotRecommendationsResponse_botVersion' - The version of the bot that contains the bot recommendation list.
--
-- 'localeId', 'listBotRecommendationsResponse_localeId' - The identifier of the language and locale of the bot recommendation
-- list.
--
-- 'nextToken', 'listBotRecommendationsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the ListBotRecommendations operation. If the nextToken field
-- is present, you send the contents as the nextToken parameter of a
-- ListBotRecommendations operation request to get the next page of
-- results.
--
-- 'httpStatus', 'listBotRecommendationsResponse_httpStatus' - The response's http status code.
newListBotRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotRecommendationsResponse
newListBotRecommendationsResponse pHttpStatus_ =
  ListBotRecommendationsResponse'
    { botId =
        Prelude.Nothing,
      botRecommendationSummaries =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot that contains the bot recommendation
-- list.
listBotRecommendationsResponse_botId :: Lens.Lens' ListBotRecommendationsResponse (Prelude.Maybe Prelude.Text)
listBotRecommendationsResponse_botId = Lens.lens (\ListBotRecommendationsResponse' {botId} -> botId) (\s@ListBotRecommendationsResponse' {} a -> s {botId = a} :: ListBotRecommendationsResponse)

-- | Summary information for the bot recommendations that meet the filter
-- specified in this request. The length of the list is specified in the
-- maxResults parameter of the request. If there are more bot
-- recommendations available, the nextToken field contains a token to get
-- the next page of results.
listBotRecommendationsResponse_botRecommendationSummaries :: Lens.Lens' ListBotRecommendationsResponse (Prelude.Maybe [BotRecommendationSummary])
listBotRecommendationsResponse_botRecommendationSummaries = Lens.lens (\ListBotRecommendationsResponse' {botRecommendationSummaries} -> botRecommendationSummaries) (\s@ListBotRecommendationsResponse' {} a -> s {botRecommendationSummaries = a} :: ListBotRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the bot that contains the bot recommendation list.
listBotRecommendationsResponse_botVersion :: Lens.Lens' ListBotRecommendationsResponse (Prelude.Maybe Prelude.Text)
listBotRecommendationsResponse_botVersion = Lens.lens (\ListBotRecommendationsResponse' {botVersion} -> botVersion) (\s@ListBotRecommendationsResponse' {} a -> s {botVersion = a} :: ListBotRecommendationsResponse)

-- | The identifier of the language and locale of the bot recommendation
-- list.
listBotRecommendationsResponse_localeId :: Lens.Lens' ListBotRecommendationsResponse (Prelude.Maybe Prelude.Text)
listBotRecommendationsResponse_localeId = Lens.lens (\ListBotRecommendationsResponse' {localeId} -> localeId) (\s@ListBotRecommendationsResponse' {} a -> s {localeId = a} :: ListBotRecommendationsResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the ListBotRecommendations operation. If the nextToken field
-- is present, you send the contents as the nextToken parameter of a
-- ListBotRecommendations operation request to get the next page of
-- results.
listBotRecommendationsResponse_nextToken :: Lens.Lens' ListBotRecommendationsResponse (Prelude.Maybe Prelude.Text)
listBotRecommendationsResponse_nextToken = Lens.lens (\ListBotRecommendationsResponse' {nextToken} -> nextToken) (\s@ListBotRecommendationsResponse' {} a -> s {nextToken = a} :: ListBotRecommendationsResponse)

-- | The response's http status code.
listBotRecommendationsResponse_httpStatus :: Lens.Lens' ListBotRecommendationsResponse Prelude.Int
listBotRecommendationsResponse_httpStatus = Lens.lens (\ListBotRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ListBotRecommendationsResponse' {} a -> s {httpStatus = a} :: ListBotRecommendationsResponse)

instance
  Prelude.NFData
    ListBotRecommendationsResponse
  where
  rnf ListBotRecommendationsResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botRecommendationSummaries
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
