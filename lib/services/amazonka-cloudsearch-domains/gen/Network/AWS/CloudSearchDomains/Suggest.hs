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
-- Module      : Network.AWS.CloudSearchDomains.Suggest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves autocomplete suggestions for a partial query string. You can
-- use suggestions enable you to display likely matches before users finish
-- typing. In Amazon CloudSearch, suggestions are based on the contents of
-- a particular text field. When you request suggestions, Amazon
-- CloudSearch finds all of the documents whose values in the suggester
-- field start with the specified query string. The beginning of the field
-- must match the query string to be considered a match.
--
-- For more information about configuring suggesters and retrieving
-- suggestions, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- The endpoint for submitting @Suggest@ requests is domain-specific. You
-- submit suggest requests to a domain\'s search endpoint. To get the
-- search endpoint for your domain, use the Amazon CloudSearch
-- configuration service @DescribeDomains@ action. A domain\'s endpoints
-- are also displayed on the domain dashboard in the Amazon CloudSearch
-- console.
module Network.AWS.CloudSearchDomains.Suggest
  ( -- * Creating a Request
    Suggest (..),
    newSuggest,

    -- * Request Lenses
    suggest_size,
    suggest_query,
    suggest_suggester,

    -- * Destructuring the Response
    SuggestResponse (..),
    newSuggestResponse,

    -- * Response Lenses
    suggestResponse_status,
    suggestResponse_suggest,
    suggestResponse_httpStatus,
  )
where

import Network.AWS.CloudSearchDomains.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @Suggest@ request.
--
-- /See:/ 'newSuggest' smart constructor.
data Suggest = Suggest'
  { -- | Specifies the maximum number of suggestions to return.
    size :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the string for which you want to get suggestions.
    query :: Prelude.Text,
    -- | Specifies the name of the suggester to use to find suggested matches.
    suggester :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Suggest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'suggest_size' - Specifies the maximum number of suggestions to return.
--
-- 'query', 'suggest_query' - Specifies the string for which you want to get suggestions.
--
-- 'suggester', 'suggest_suggester' - Specifies the name of the suggester to use to find suggested matches.
newSuggest ::
  -- | 'query'
  Prelude.Text ->
  -- | 'suggester'
  Prelude.Text ->
  Suggest
newSuggest pQuery_ pSuggester_ =
  Suggest'
    { size = Prelude.Nothing,
      query = pQuery_,
      suggester = pSuggester_
    }

-- | Specifies the maximum number of suggestions to return.
suggest_size :: Lens.Lens' Suggest (Prelude.Maybe Prelude.Integer)
suggest_size = Lens.lens (\Suggest' {size} -> size) (\s@Suggest' {} a -> s {size = a} :: Suggest)

-- | Specifies the string for which you want to get suggestions.
suggest_query :: Lens.Lens' Suggest Prelude.Text
suggest_query = Lens.lens (\Suggest' {query} -> query) (\s@Suggest' {} a -> s {query = a} :: Suggest)

-- | Specifies the name of the suggester to use to find suggested matches.
suggest_suggester :: Lens.Lens' Suggest Prelude.Text
suggest_suggester = Lens.lens (\Suggest' {suggester} -> suggester) (\s@Suggest' {} a -> s {suggester = a} :: Suggest)

instance Core.AWSRequest Suggest where
  type AWSResponse Suggest = SuggestResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SuggestResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "suggest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Suggest

instance Prelude.NFData Suggest

instance Core.ToHeaders Suggest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath Suggest where
  toPath = Prelude.const "/2013-01-01/suggest"

instance Core.ToQuery Suggest where
  toQuery Suggest' {..} =
    Prelude.mconcat
      [ "size" Core.=: size,
        "q" Core.=: query,
        "suggester" Core.=: suggester,
        "format=sdk&pretty=true"
      ]

-- | Contains the response to a @Suggest@ request.
--
-- /See:/ 'newSuggestResponse' smart constructor.
data SuggestResponse = SuggestResponse'
  { -- | The status of a @SuggestRequest@. Contains the resource ID (@rid@) and
    -- how long it took to process the request (@timems@).
    status :: Prelude.Maybe SuggestStatus,
    -- | Container for the matching search suggestion information.
    suggest :: Prelude.Maybe SuggestModel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'suggestResponse_status' - The status of a @SuggestRequest@. Contains the resource ID (@rid@) and
-- how long it took to process the request (@timems@).
--
-- 'suggest', 'suggestResponse_suggest' - Container for the matching search suggestion information.
--
-- 'httpStatus', 'suggestResponse_httpStatus' - The response's http status code.
newSuggestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SuggestResponse
newSuggestResponse pHttpStatus_ =
  SuggestResponse'
    { status = Prelude.Nothing,
      suggest = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of a @SuggestRequest@. Contains the resource ID (@rid@) and
-- how long it took to process the request (@timems@).
suggestResponse_status :: Lens.Lens' SuggestResponse (Prelude.Maybe SuggestStatus)
suggestResponse_status = Lens.lens (\SuggestResponse' {status} -> status) (\s@SuggestResponse' {} a -> s {status = a} :: SuggestResponse)

-- | Container for the matching search suggestion information.
suggestResponse_suggest :: Lens.Lens' SuggestResponse (Prelude.Maybe SuggestModel)
suggestResponse_suggest = Lens.lens (\SuggestResponse' {suggest} -> suggest) (\s@SuggestResponse' {} a -> s {suggest = a} :: SuggestResponse)

-- | The response's http status code.
suggestResponse_httpStatus :: Lens.Lens' SuggestResponse Prelude.Int
suggestResponse_httpStatus = Lens.lens (\SuggestResponse' {httpStatus} -> httpStatus) (\s@SuggestResponse' {} a -> s {httpStatus = a} :: SuggestResponse)

instance Prelude.NFData SuggestResponse
