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
-- Module      : Amazonka.LexV2Models.SearchAssociatedTranscripts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Search for associated transcripts that meet the specified criteria.
module Amazonka.LexV2Models.SearchAssociatedTranscripts
  ( -- * Creating a Request
    SearchAssociatedTranscripts (..),
    newSearchAssociatedTranscripts,

    -- * Request Lenses
    searchAssociatedTranscripts_maxResults,
    searchAssociatedTranscripts_nextIndex,
    searchAssociatedTranscripts_searchOrder,
    searchAssociatedTranscripts_botId,
    searchAssociatedTranscripts_botVersion,
    searchAssociatedTranscripts_localeId,
    searchAssociatedTranscripts_botRecommendationId,
    searchAssociatedTranscripts_filters,

    -- * Destructuring the Response
    SearchAssociatedTranscriptsResponse (..),
    newSearchAssociatedTranscriptsResponse,

    -- * Response Lenses
    searchAssociatedTranscriptsResponse_associatedTranscripts,
    searchAssociatedTranscriptsResponse_botId,
    searchAssociatedTranscriptsResponse_botRecommendationId,
    searchAssociatedTranscriptsResponse_botVersion,
    searchAssociatedTranscriptsResponse_localeId,
    searchAssociatedTranscriptsResponse_nextIndex,
    searchAssociatedTranscriptsResponse_totalResults,
    searchAssociatedTranscriptsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchAssociatedTranscripts' smart constructor.
data SearchAssociatedTranscripts = SearchAssociatedTranscripts'
  { -- | The maximum number of bot recommendations to return in each page of
    -- results. If there are fewer results than the max page size, only the
    -- actual number of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the SearchAssociatedTranscriptsRequest operation
    -- contains more results than specified in the maxResults parameter, an
    -- index is returned in the response. Use that index in the nextIndex
    -- parameter to return the next page of results.
    nextIndex :: Prelude.Maybe Prelude.Natural,
    -- | How SearchResults are ordered. Valid values are Ascending or Descending.
    -- The default is Descending.
    searchOrder :: Prelude.Maybe SearchOrder,
    -- | The unique identifier of the bot associated with the transcripts that
    -- you are searching.
    botId :: Prelude.Text,
    -- | The version of the bot containing the transcripts that you are
    -- searching.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the transcripts to search.
    -- The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
    localeId :: Prelude.Text,
    -- | The unique identifier of the bot recommendation associated with the
    -- transcripts to search.
    botRecommendationId :: Prelude.Text,
    -- | A list of filter objects.
    filters :: Prelude.NonEmpty AssociatedTranscriptFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAssociatedTranscripts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchAssociatedTranscripts_maxResults' - The maximum number of bot recommendations to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
--
-- 'nextIndex', 'searchAssociatedTranscripts_nextIndex' - If the response from the SearchAssociatedTranscriptsRequest operation
-- contains more results than specified in the maxResults parameter, an
-- index is returned in the response. Use that index in the nextIndex
-- parameter to return the next page of results.
--
-- 'searchOrder', 'searchAssociatedTranscripts_searchOrder' - How SearchResults are ordered. Valid values are Ascending or Descending.
-- The default is Descending.
--
-- 'botId', 'searchAssociatedTranscripts_botId' - The unique identifier of the bot associated with the transcripts that
-- you are searching.
--
-- 'botVersion', 'searchAssociatedTranscripts_botVersion' - The version of the bot containing the transcripts that you are
-- searching.
--
-- 'localeId', 'searchAssociatedTranscripts_localeId' - The identifier of the language and locale of the transcripts to search.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
--
-- 'botRecommendationId', 'searchAssociatedTranscripts_botRecommendationId' - The unique identifier of the bot recommendation associated with the
-- transcripts to search.
--
-- 'filters', 'searchAssociatedTranscripts_filters' - A list of filter objects.
newSearchAssociatedTranscripts ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'botRecommendationId'
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty AssociatedTranscriptFilter ->
  SearchAssociatedTranscripts
newSearchAssociatedTranscripts
  pBotId_
  pBotVersion_
  pLocaleId_
  pBotRecommendationId_
  pFilters_ =
    SearchAssociatedTranscripts'
      { maxResults =
          Prelude.Nothing,
        nextIndex = Prelude.Nothing,
        searchOrder = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        botRecommendationId = pBotRecommendationId_,
        filters = Lens.coerced Lens.# pFilters_
      }

-- | The maximum number of bot recommendations to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
searchAssociatedTranscripts_maxResults :: Lens.Lens' SearchAssociatedTranscripts (Prelude.Maybe Prelude.Natural)
searchAssociatedTranscripts_maxResults = Lens.lens (\SearchAssociatedTranscripts' {maxResults} -> maxResults) (\s@SearchAssociatedTranscripts' {} a -> s {maxResults = a} :: SearchAssociatedTranscripts)

-- | If the response from the SearchAssociatedTranscriptsRequest operation
-- contains more results than specified in the maxResults parameter, an
-- index is returned in the response. Use that index in the nextIndex
-- parameter to return the next page of results.
searchAssociatedTranscripts_nextIndex :: Lens.Lens' SearchAssociatedTranscripts (Prelude.Maybe Prelude.Natural)
searchAssociatedTranscripts_nextIndex = Lens.lens (\SearchAssociatedTranscripts' {nextIndex} -> nextIndex) (\s@SearchAssociatedTranscripts' {} a -> s {nextIndex = a} :: SearchAssociatedTranscripts)

-- | How SearchResults are ordered. Valid values are Ascending or Descending.
-- The default is Descending.
searchAssociatedTranscripts_searchOrder :: Lens.Lens' SearchAssociatedTranscripts (Prelude.Maybe SearchOrder)
searchAssociatedTranscripts_searchOrder = Lens.lens (\SearchAssociatedTranscripts' {searchOrder} -> searchOrder) (\s@SearchAssociatedTranscripts' {} a -> s {searchOrder = a} :: SearchAssociatedTranscripts)

-- | The unique identifier of the bot associated with the transcripts that
-- you are searching.
searchAssociatedTranscripts_botId :: Lens.Lens' SearchAssociatedTranscripts Prelude.Text
searchAssociatedTranscripts_botId = Lens.lens (\SearchAssociatedTranscripts' {botId} -> botId) (\s@SearchAssociatedTranscripts' {} a -> s {botId = a} :: SearchAssociatedTranscripts)

-- | The version of the bot containing the transcripts that you are
-- searching.
searchAssociatedTranscripts_botVersion :: Lens.Lens' SearchAssociatedTranscripts Prelude.Text
searchAssociatedTranscripts_botVersion = Lens.lens (\SearchAssociatedTranscripts' {botVersion} -> botVersion) (\s@SearchAssociatedTranscripts' {} a -> s {botVersion = a} :: SearchAssociatedTranscripts)

-- | The identifier of the language and locale of the transcripts to search.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
searchAssociatedTranscripts_localeId :: Lens.Lens' SearchAssociatedTranscripts Prelude.Text
searchAssociatedTranscripts_localeId = Lens.lens (\SearchAssociatedTranscripts' {localeId} -> localeId) (\s@SearchAssociatedTranscripts' {} a -> s {localeId = a} :: SearchAssociatedTranscripts)

-- | The unique identifier of the bot recommendation associated with the
-- transcripts to search.
searchAssociatedTranscripts_botRecommendationId :: Lens.Lens' SearchAssociatedTranscripts Prelude.Text
searchAssociatedTranscripts_botRecommendationId = Lens.lens (\SearchAssociatedTranscripts' {botRecommendationId} -> botRecommendationId) (\s@SearchAssociatedTranscripts' {} a -> s {botRecommendationId = a} :: SearchAssociatedTranscripts)

-- | A list of filter objects.
searchAssociatedTranscripts_filters :: Lens.Lens' SearchAssociatedTranscripts (Prelude.NonEmpty AssociatedTranscriptFilter)
searchAssociatedTranscripts_filters = Lens.lens (\SearchAssociatedTranscripts' {filters} -> filters) (\s@SearchAssociatedTranscripts' {} a -> s {filters = a} :: SearchAssociatedTranscripts) Prelude.. Lens.coerced

instance Core.AWSRequest SearchAssociatedTranscripts where
  type
    AWSResponse SearchAssociatedTranscripts =
      SearchAssociatedTranscriptsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchAssociatedTranscriptsResponse'
            Prelude.<$> ( x Data..?> "associatedTranscripts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botRecommendationId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextIndex")
            Prelude.<*> (x Data..?> "totalResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchAssociatedTranscripts where
  hashWithSalt _salt SearchAssociatedTranscripts' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextIndex
      `Prelude.hashWithSalt` searchOrder
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` botRecommendationId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchAssociatedTranscripts where
  rnf SearchAssociatedTranscripts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextIndex
      `Prelude.seq` Prelude.rnf searchOrder
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botRecommendationId
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchAssociatedTranscripts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchAssociatedTranscripts where
  toJSON SearchAssociatedTranscripts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextIndex" Data..=) Prelude.<$> nextIndex,
            ("searchOrder" Data..=) Prelude.<$> searchOrder,
            Prelude.Just ("filters" Data..= filters)
          ]
      )

instance Data.ToPath SearchAssociatedTranscripts where
  toPath SearchAssociatedTranscripts' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/botrecommendations/",
        Data.toBS botRecommendationId,
        "/associatedtranscripts"
      ]

instance Data.ToQuery SearchAssociatedTranscripts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchAssociatedTranscriptsResponse' smart constructor.
data SearchAssociatedTranscriptsResponse = SearchAssociatedTranscriptsResponse'
  { -- | The object that contains the associated transcript that meet the
    -- criteria you specified.
    associatedTranscripts :: Prelude.Maybe [AssociatedTranscript],
    -- | The unique identifier of the bot associated with the transcripts that
    -- you are searching.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot recommendation associated with the
    -- transcripts to search.
    botRecommendationId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot containing the transcripts that you are
    -- searching.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the language and locale of the transcripts to search.
    -- The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A index that indicates whether there are more results to return in a
    -- response to the SearchAssociatedTranscripts operation. If the nextIndex
    -- field is present, you send the contents as the nextIndex parameter of a
    -- SearchAssociatedTranscriptsRequest operation to get the next page of
    -- results.
    nextIndex :: Prelude.Maybe Prelude.Natural,
    -- | The total number of transcripts returned by the search.
    totalResults :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAssociatedTranscriptsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedTranscripts', 'searchAssociatedTranscriptsResponse_associatedTranscripts' - The object that contains the associated transcript that meet the
-- criteria you specified.
--
-- 'botId', 'searchAssociatedTranscriptsResponse_botId' - The unique identifier of the bot associated with the transcripts that
-- you are searching.
--
-- 'botRecommendationId', 'searchAssociatedTranscriptsResponse_botRecommendationId' - The unique identifier of the bot recommendation associated with the
-- transcripts to search.
--
-- 'botVersion', 'searchAssociatedTranscriptsResponse_botVersion' - The version of the bot containing the transcripts that you are
-- searching.
--
-- 'localeId', 'searchAssociatedTranscriptsResponse_localeId' - The identifier of the language and locale of the transcripts to search.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
--
-- 'nextIndex', 'searchAssociatedTranscriptsResponse_nextIndex' - A index that indicates whether there are more results to return in a
-- response to the SearchAssociatedTranscripts operation. If the nextIndex
-- field is present, you send the contents as the nextIndex parameter of a
-- SearchAssociatedTranscriptsRequest operation to get the next page of
-- results.
--
-- 'totalResults', 'searchAssociatedTranscriptsResponse_totalResults' - The total number of transcripts returned by the search.
--
-- 'httpStatus', 'searchAssociatedTranscriptsResponse_httpStatus' - The response's http status code.
newSearchAssociatedTranscriptsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchAssociatedTranscriptsResponse
newSearchAssociatedTranscriptsResponse pHttpStatus_ =
  SearchAssociatedTranscriptsResponse'
    { associatedTranscripts =
        Prelude.Nothing,
      botId = Prelude.Nothing,
      botRecommendationId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextIndex = Prelude.Nothing,
      totalResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The object that contains the associated transcript that meet the
-- criteria you specified.
searchAssociatedTranscriptsResponse_associatedTranscripts :: Lens.Lens' SearchAssociatedTranscriptsResponse (Prelude.Maybe [AssociatedTranscript])
searchAssociatedTranscriptsResponse_associatedTranscripts = Lens.lens (\SearchAssociatedTranscriptsResponse' {associatedTranscripts} -> associatedTranscripts) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {associatedTranscripts = a} :: SearchAssociatedTranscriptsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the bot associated with the transcripts that
-- you are searching.
searchAssociatedTranscriptsResponse_botId :: Lens.Lens' SearchAssociatedTranscriptsResponse (Prelude.Maybe Prelude.Text)
searchAssociatedTranscriptsResponse_botId = Lens.lens (\SearchAssociatedTranscriptsResponse' {botId} -> botId) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {botId = a} :: SearchAssociatedTranscriptsResponse)

-- | The unique identifier of the bot recommendation associated with the
-- transcripts to search.
searchAssociatedTranscriptsResponse_botRecommendationId :: Lens.Lens' SearchAssociatedTranscriptsResponse (Prelude.Maybe Prelude.Text)
searchAssociatedTranscriptsResponse_botRecommendationId = Lens.lens (\SearchAssociatedTranscriptsResponse' {botRecommendationId} -> botRecommendationId) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {botRecommendationId = a} :: SearchAssociatedTranscriptsResponse)

-- | The version of the bot containing the transcripts that you are
-- searching.
searchAssociatedTranscriptsResponse_botVersion :: Lens.Lens' SearchAssociatedTranscriptsResponse (Prelude.Maybe Prelude.Text)
searchAssociatedTranscriptsResponse_botVersion = Lens.lens (\SearchAssociatedTranscriptsResponse' {botVersion} -> botVersion) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {botVersion = a} :: SearchAssociatedTranscriptsResponse)

-- | The identifier of the language and locale of the transcripts to search.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
searchAssociatedTranscriptsResponse_localeId :: Lens.Lens' SearchAssociatedTranscriptsResponse (Prelude.Maybe Prelude.Text)
searchAssociatedTranscriptsResponse_localeId = Lens.lens (\SearchAssociatedTranscriptsResponse' {localeId} -> localeId) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {localeId = a} :: SearchAssociatedTranscriptsResponse)

-- | A index that indicates whether there are more results to return in a
-- response to the SearchAssociatedTranscripts operation. If the nextIndex
-- field is present, you send the contents as the nextIndex parameter of a
-- SearchAssociatedTranscriptsRequest operation to get the next page of
-- results.
searchAssociatedTranscriptsResponse_nextIndex :: Lens.Lens' SearchAssociatedTranscriptsResponse (Prelude.Maybe Prelude.Natural)
searchAssociatedTranscriptsResponse_nextIndex = Lens.lens (\SearchAssociatedTranscriptsResponse' {nextIndex} -> nextIndex) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {nextIndex = a} :: SearchAssociatedTranscriptsResponse)

-- | The total number of transcripts returned by the search.
searchAssociatedTranscriptsResponse_totalResults :: Lens.Lens' SearchAssociatedTranscriptsResponse (Prelude.Maybe Prelude.Natural)
searchAssociatedTranscriptsResponse_totalResults = Lens.lens (\SearchAssociatedTranscriptsResponse' {totalResults} -> totalResults) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {totalResults = a} :: SearchAssociatedTranscriptsResponse)

-- | The response's http status code.
searchAssociatedTranscriptsResponse_httpStatus :: Lens.Lens' SearchAssociatedTranscriptsResponse Prelude.Int
searchAssociatedTranscriptsResponse_httpStatus = Lens.lens (\SearchAssociatedTranscriptsResponse' {httpStatus} -> httpStatus) (\s@SearchAssociatedTranscriptsResponse' {} a -> s {httpStatus = a} :: SearchAssociatedTranscriptsResponse)

instance
  Prelude.NFData
    SearchAssociatedTranscriptsResponse
  where
  rnf SearchAssociatedTranscriptsResponse' {..} =
    Prelude.rnf associatedTranscripts
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botRecommendationId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextIndex
      `Prelude.seq` Prelude.rnf totalResults
      `Prelude.seq` Prelude.rnf httpStatus
