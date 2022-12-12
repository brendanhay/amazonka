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
-- Module      : Amazonka.Proton.ListEnvironmentTemplateVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List major or minor versions of an environment template with detail
-- data.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListEnvironmentTemplateVersions
  ( -- * Creating a Request
    ListEnvironmentTemplateVersions (..),
    newListEnvironmentTemplateVersions,

    -- * Request Lenses
    listEnvironmentTemplateVersions_majorVersion,
    listEnvironmentTemplateVersions_maxResults,
    listEnvironmentTemplateVersions_nextToken,
    listEnvironmentTemplateVersions_templateName,

    -- * Destructuring the Response
    ListEnvironmentTemplateVersionsResponse (..),
    newListEnvironmentTemplateVersionsResponse,

    -- * Response Lenses
    listEnvironmentTemplateVersionsResponse_nextToken,
    listEnvironmentTemplateVersionsResponse_httpStatus,
    listEnvironmentTemplateVersionsResponse_templateVersions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironmentTemplateVersions' smart constructor.
data ListEnvironmentTemplateVersions = ListEnvironmentTemplateVersions'
  { -- | To view a list of minor of versions under a major version of an
    -- environment template, include @major Version@.
    --
    -- To view a list of major versions of an environment template, /exclude/
    -- @major Version@.
    majorVersion :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of major or minor versions of an environment template
    -- to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the location of the next major or minor version
    -- in the array of major or minor versions of an environment template,
    -- after the list of major or minor versions that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentTemplateVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorVersion', 'listEnvironmentTemplateVersions_majorVersion' - To view a list of minor of versions under a major version of an
-- environment template, include @major Version@.
--
-- To view a list of major versions of an environment template, /exclude/
-- @major Version@.
--
-- 'maxResults', 'listEnvironmentTemplateVersions_maxResults' - The maximum number of major or minor versions of an environment template
-- to list.
--
-- 'nextToken', 'listEnvironmentTemplateVersions_nextToken' - A token that indicates the location of the next major or minor version
-- in the array of major or minor versions of an environment template,
-- after the list of major or minor versions that was previously requested.
--
-- 'templateName', 'listEnvironmentTemplateVersions_templateName' - The name of the environment template.
newListEnvironmentTemplateVersions ::
  -- | 'templateName'
  Prelude.Text ->
  ListEnvironmentTemplateVersions
newListEnvironmentTemplateVersions pTemplateName_ =
  ListEnvironmentTemplateVersions'
    { majorVersion =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | To view a list of minor of versions under a major version of an
-- environment template, include @major Version@.
--
-- To view a list of major versions of an environment template, /exclude/
-- @major Version@.
listEnvironmentTemplateVersions_majorVersion :: Lens.Lens' ListEnvironmentTemplateVersions (Prelude.Maybe Prelude.Text)
listEnvironmentTemplateVersions_majorVersion = Lens.lens (\ListEnvironmentTemplateVersions' {majorVersion} -> majorVersion) (\s@ListEnvironmentTemplateVersions' {} a -> s {majorVersion = a} :: ListEnvironmentTemplateVersions)

-- | The maximum number of major or minor versions of an environment template
-- to list.
listEnvironmentTemplateVersions_maxResults :: Lens.Lens' ListEnvironmentTemplateVersions (Prelude.Maybe Prelude.Natural)
listEnvironmentTemplateVersions_maxResults = Lens.lens (\ListEnvironmentTemplateVersions' {maxResults} -> maxResults) (\s@ListEnvironmentTemplateVersions' {} a -> s {maxResults = a} :: ListEnvironmentTemplateVersions)

-- | A token that indicates the location of the next major or minor version
-- in the array of major or minor versions of an environment template,
-- after the list of major or minor versions that was previously requested.
listEnvironmentTemplateVersions_nextToken :: Lens.Lens' ListEnvironmentTemplateVersions (Prelude.Maybe Prelude.Text)
listEnvironmentTemplateVersions_nextToken = Lens.lens (\ListEnvironmentTemplateVersions' {nextToken} -> nextToken) (\s@ListEnvironmentTemplateVersions' {} a -> s {nextToken = a} :: ListEnvironmentTemplateVersions)

-- | The name of the environment template.
listEnvironmentTemplateVersions_templateName :: Lens.Lens' ListEnvironmentTemplateVersions Prelude.Text
listEnvironmentTemplateVersions_templateName = Lens.lens (\ListEnvironmentTemplateVersions' {templateName} -> templateName) (\s@ListEnvironmentTemplateVersions' {} a -> s {templateName = a} :: ListEnvironmentTemplateVersions)

instance
  Core.AWSPager
    ListEnvironmentTemplateVersions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentTemplateVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEnvironmentTemplateVersionsResponse_templateVersions
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEnvironmentTemplateVersions_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentTemplateVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListEnvironmentTemplateVersions
  where
  type
    AWSResponse ListEnvironmentTemplateVersions =
      ListEnvironmentTemplateVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentTemplateVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "templateVersions"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListEnvironmentTemplateVersions
  where
  hashWithSalt
    _salt
    ListEnvironmentTemplateVersions' {..} =
      _salt `Prelude.hashWithSalt` majorVersion
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    ListEnvironmentTemplateVersions
  where
  rnf ListEnvironmentTemplateVersions' {..} =
    Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templateName

instance
  Data.ToHeaders
    ListEnvironmentTemplateVersions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListEnvironmentTemplateVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEnvironmentTemplateVersions where
  toJSON ListEnvironmentTemplateVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("majorVersion" Data..=) Prelude.<$> majorVersion,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("templateName" Data..= templateName)
          ]
      )

instance Data.ToPath ListEnvironmentTemplateVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEnvironmentTemplateVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnvironmentTemplateVersionsResponse' smart constructor.
data ListEnvironmentTemplateVersionsResponse = ListEnvironmentTemplateVersionsResponse'
  { -- | A token that indicates the location of the next major or minor version
    -- in the array of major or minor versions of an environment template,
    -- after the list of major or minor versions that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of major or minor versions of an environment template detail
    -- data.
    templateVersions :: [EnvironmentTemplateVersionSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentTemplateVersionsResponse_nextToken' - A token that indicates the location of the next major or minor version
-- in the array of major or minor versions of an environment template,
-- after the list of major or minor versions that was previously requested.
--
-- 'httpStatus', 'listEnvironmentTemplateVersionsResponse_httpStatus' - The response's http status code.
--
-- 'templateVersions', 'listEnvironmentTemplateVersionsResponse_templateVersions' - An array of major or minor versions of an environment template detail
-- data.
newListEnvironmentTemplateVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentTemplateVersionsResponse
newListEnvironmentTemplateVersionsResponse
  pHttpStatus_ =
    ListEnvironmentTemplateVersionsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        templateVersions = Prelude.mempty
      }

-- | A token that indicates the location of the next major or minor version
-- in the array of major or minor versions of an environment template,
-- after the list of major or minor versions that was previously requested.
listEnvironmentTemplateVersionsResponse_nextToken :: Lens.Lens' ListEnvironmentTemplateVersionsResponse (Prelude.Maybe Prelude.Text)
listEnvironmentTemplateVersionsResponse_nextToken = Lens.lens (\ListEnvironmentTemplateVersionsResponse' {nextToken} -> nextToken) (\s@ListEnvironmentTemplateVersionsResponse' {} a -> s {nextToken = a} :: ListEnvironmentTemplateVersionsResponse)

-- | The response's http status code.
listEnvironmentTemplateVersionsResponse_httpStatus :: Lens.Lens' ListEnvironmentTemplateVersionsResponse Prelude.Int
listEnvironmentTemplateVersionsResponse_httpStatus = Lens.lens (\ListEnvironmentTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentTemplateVersionsResponse' {} a -> s {httpStatus = a} :: ListEnvironmentTemplateVersionsResponse)

-- | An array of major or minor versions of an environment template detail
-- data.
listEnvironmentTemplateVersionsResponse_templateVersions :: Lens.Lens' ListEnvironmentTemplateVersionsResponse [EnvironmentTemplateVersionSummary]
listEnvironmentTemplateVersionsResponse_templateVersions = Lens.lens (\ListEnvironmentTemplateVersionsResponse' {templateVersions} -> templateVersions) (\s@ListEnvironmentTemplateVersionsResponse' {} a -> s {templateVersions = a} :: ListEnvironmentTemplateVersionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEnvironmentTemplateVersionsResponse
  where
  rnf ListEnvironmentTemplateVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templateVersions
