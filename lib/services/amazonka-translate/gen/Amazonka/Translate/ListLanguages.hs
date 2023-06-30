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
-- Module      : Amazonka.Translate.ListLanguages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of languages (RFC-5646 codes and names) that Amazon
-- Translate supports.
module Amazonka.Translate.ListLanguages
  ( -- * Creating a Request
    ListLanguages (..),
    newListLanguages,

    -- * Request Lenses
    listLanguages_displayLanguageCode,
    listLanguages_maxResults,
    listLanguages_nextToken,

    -- * Destructuring the Response
    ListLanguagesResponse (..),
    newListLanguagesResponse,

    -- * Response Lenses
    listLanguagesResponse_displayLanguageCode,
    listLanguagesResponse_languages,
    listLanguagesResponse_nextToken,
    listLanguagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newListLanguages' smart constructor.
data ListLanguages = ListLanguages'
  { -- | The language code for the language to use to display the language names
    -- in the response. The language code is @en@ by default.
    displayLanguageCode :: Prelude.Maybe DisplayLanguageCode,
    -- | The maximum number of results to return in each response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Include the NextToken value to fetch the next group of supported
    -- languages.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLanguages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayLanguageCode', 'listLanguages_displayLanguageCode' - The language code for the language to use to display the language names
-- in the response. The language code is @en@ by default.
--
-- 'maxResults', 'listLanguages_maxResults' - The maximum number of results to return in each response.
--
-- 'nextToken', 'listLanguages_nextToken' - Include the NextToken value to fetch the next group of supported
-- languages.
newListLanguages ::
  ListLanguages
newListLanguages =
  ListLanguages'
    { displayLanguageCode =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The language code for the language to use to display the language names
-- in the response. The language code is @en@ by default.
listLanguages_displayLanguageCode :: Lens.Lens' ListLanguages (Prelude.Maybe DisplayLanguageCode)
listLanguages_displayLanguageCode = Lens.lens (\ListLanguages' {displayLanguageCode} -> displayLanguageCode) (\s@ListLanguages' {} a -> s {displayLanguageCode = a} :: ListLanguages)

-- | The maximum number of results to return in each response.
listLanguages_maxResults :: Lens.Lens' ListLanguages (Prelude.Maybe Prelude.Natural)
listLanguages_maxResults = Lens.lens (\ListLanguages' {maxResults} -> maxResults) (\s@ListLanguages' {} a -> s {maxResults = a} :: ListLanguages)

-- | Include the NextToken value to fetch the next group of supported
-- languages.
listLanguages_nextToken :: Lens.Lens' ListLanguages (Prelude.Maybe Prelude.Text)
listLanguages_nextToken = Lens.lens (\ListLanguages' {nextToken} -> nextToken) (\s@ListLanguages' {} a -> s {nextToken = a} :: ListLanguages)

instance Core.AWSRequest ListLanguages where
  type
    AWSResponse ListLanguages =
      ListLanguagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLanguagesResponse'
            Prelude.<$> (x Data..?> "DisplayLanguageCode")
            Prelude.<*> (x Data..?> "Languages" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLanguages where
  hashWithSalt _salt ListLanguages' {..} =
    _salt
      `Prelude.hashWithSalt` displayLanguageCode
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLanguages where
  rnf ListLanguages' {..} =
    Prelude.rnf displayLanguageCode
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLanguages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.ListLanguages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLanguages where
  toJSON ListLanguages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayLanguageCode" Data..=)
              Prelude.<$> displayLanguageCode,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLanguages where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLanguages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLanguagesResponse' smart constructor.
data ListLanguagesResponse = ListLanguagesResponse'
  { -- | The language code passed in with the request.
    displayLanguageCode :: Prelude.Maybe DisplayLanguageCode,
    -- | The list of supported languages.
    languages :: Prelude.Maybe [Language],
    -- | If the response does not include all remaining results, use the
    -- NextToken in the next request to fetch the next group of supported
    -- languages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLanguagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayLanguageCode', 'listLanguagesResponse_displayLanguageCode' - The language code passed in with the request.
--
-- 'languages', 'listLanguagesResponse_languages' - The list of supported languages.
--
-- 'nextToken', 'listLanguagesResponse_nextToken' - If the response does not include all remaining results, use the
-- NextToken in the next request to fetch the next group of supported
-- languages.
--
-- 'httpStatus', 'listLanguagesResponse_httpStatus' - The response's http status code.
newListLanguagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLanguagesResponse
newListLanguagesResponse pHttpStatus_ =
  ListLanguagesResponse'
    { displayLanguageCode =
        Prelude.Nothing,
      languages = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code passed in with the request.
listLanguagesResponse_displayLanguageCode :: Lens.Lens' ListLanguagesResponse (Prelude.Maybe DisplayLanguageCode)
listLanguagesResponse_displayLanguageCode = Lens.lens (\ListLanguagesResponse' {displayLanguageCode} -> displayLanguageCode) (\s@ListLanguagesResponse' {} a -> s {displayLanguageCode = a} :: ListLanguagesResponse)

-- | The list of supported languages.
listLanguagesResponse_languages :: Lens.Lens' ListLanguagesResponse (Prelude.Maybe [Language])
listLanguagesResponse_languages = Lens.lens (\ListLanguagesResponse' {languages} -> languages) (\s@ListLanguagesResponse' {} a -> s {languages = a} :: ListLanguagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response does not include all remaining results, use the
-- NextToken in the next request to fetch the next group of supported
-- languages.
listLanguagesResponse_nextToken :: Lens.Lens' ListLanguagesResponse (Prelude.Maybe Prelude.Text)
listLanguagesResponse_nextToken = Lens.lens (\ListLanguagesResponse' {nextToken} -> nextToken) (\s@ListLanguagesResponse' {} a -> s {nextToken = a} :: ListLanguagesResponse)

-- | The response's http status code.
listLanguagesResponse_httpStatus :: Lens.Lens' ListLanguagesResponse Prelude.Int
listLanguagesResponse_httpStatus = Lens.lens (\ListLanguagesResponse' {httpStatus} -> httpStatus) (\s@ListLanguagesResponse' {} a -> s {httpStatus = a} :: ListLanguagesResponse)

instance Prelude.NFData ListLanguagesResponse where
  rnf ListLanguagesResponse' {..} =
    Prelude.rnf displayLanguageCode
      `Prelude.seq` Prelude.rnf languages
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
