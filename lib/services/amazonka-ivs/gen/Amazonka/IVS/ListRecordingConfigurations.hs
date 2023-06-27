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
-- Module      : Amazonka.IVS.ListRecordingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about all recording configurations in your
-- account, in the Amazon Web Services region where the API request is
-- processed.
--
-- This operation returns paginated results.
module Amazonka.IVS.ListRecordingConfigurations
  ( -- * Creating a Request
    ListRecordingConfigurations (..),
    newListRecordingConfigurations,

    -- * Request Lenses
    listRecordingConfigurations_maxResults,
    listRecordingConfigurations_nextToken,

    -- * Destructuring the Response
    ListRecordingConfigurationsResponse (..),
    newListRecordingConfigurationsResponse,

    -- * Response Lenses
    listRecordingConfigurationsResponse_nextToken,
    listRecordingConfigurationsResponse_httpStatus,
    listRecordingConfigurationsResponse_recordingConfigurations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecordingConfigurations' smart constructor.
data ListRecordingConfigurations = ListRecordingConfigurations'
  { -- | Maximum number of recording configurations to return. Default: your
    -- service quota or 100, whichever is smaller.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first recording configuration to retrieve. This is used for
    -- pagination; see the @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecordingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecordingConfigurations_maxResults' - Maximum number of recording configurations to return. Default: your
-- service quota or 100, whichever is smaller.
--
-- 'nextToken', 'listRecordingConfigurations_nextToken' - The first recording configuration to retrieve. This is used for
-- pagination; see the @nextToken@ response field.
newListRecordingConfigurations ::
  ListRecordingConfigurations
newListRecordingConfigurations =
  ListRecordingConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of recording configurations to return. Default: your
-- service quota or 100, whichever is smaller.
listRecordingConfigurations_maxResults :: Lens.Lens' ListRecordingConfigurations (Prelude.Maybe Prelude.Natural)
listRecordingConfigurations_maxResults = Lens.lens (\ListRecordingConfigurations' {maxResults} -> maxResults) (\s@ListRecordingConfigurations' {} a -> s {maxResults = a} :: ListRecordingConfigurations)

-- | The first recording configuration to retrieve. This is used for
-- pagination; see the @nextToken@ response field.
listRecordingConfigurations_nextToken :: Lens.Lens' ListRecordingConfigurations (Prelude.Maybe Prelude.Text)
listRecordingConfigurations_nextToken = Lens.lens (\ListRecordingConfigurations' {nextToken} -> nextToken) (\s@ListRecordingConfigurations' {} a -> s {nextToken = a} :: ListRecordingConfigurations)

instance Core.AWSPager ListRecordingConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecordingConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listRecordingConfigurationsResponse_recordingConfigurations
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRecordingConfigurations_nextToken
          Lens..~ rs
          Lens.^? listRecordingConfigurationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRecordingConfigurations where
  type
    AWSResponse ListRecordingConfigurations =
      ListRecordingConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecordingConfigurationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "recordingConfigurations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListRecordingConfigurations where
  hashWithSalt _salt ListRecordingConfigurations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRecordingConfigurations where
  rnf ListRecordingConfigurations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListRecordingConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRecordingConfigurations where
  toJSON ListRecordingConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListRecordingConfigurations where
  toPath = Prelude.const "/ListRecordingConfigurations"

instance Data.ToQuery ListRecordingConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecordingConfigurationsResponse' smart constructor.
data ListRecordingConfigurationsResponse = ListRecordingConfigurationsResponse'
  { -- | If there are more recording configurations than @maxResults@, use
    -- @nextToken@ in the request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of the matching recording configurations.
    recordingConfigurations :: [RecordingConfigurationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecordingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecordingConfigurationsResponse_nextToken' - If there are more recording configurations than @maxResults@, use
-- @nextToken@ in the request to get the next set.
--
-- 'httpStatus', 'listRecordingConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'recordingConfigurations', 'listRecordingConfigurationsResponse_recordingConfigurations' - List of the matching recording configurations.
newListRecordingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecordingConfigurationsResponse
newListRecordingConfigurationsResponse pHttpStatus_ =
  ListRecordingConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      recordingConfigurations =
        Prelude.mempty
    }

-- | If there are more recording configurations than @maxResults@, use
-- @nextToken@ in the request to get the next set.
listRecordingConfigurationsResponse_nextToken :: Lens.Lens' ListRecordingConfigurationsResponse (Prelude.Maybe Prelude.Text)
listRecordingConfigurationsResponse_nextToken = Lens.lens (\ListRecordingConfigurationsResponse' {nextToken} -> nextToken) (\s@ListRecordingConfigurationsResponse' {} a -> s {nextToken = a} :: ListRecordingConfigurationsResponse)

-- | The response's http status code.
listRecordingConfigurationsResponse_httpStatus :: Lens.Lens' ListRecordingConfigurationsResponse Prelude.Int
listRecordingConfigurationsResponse_httpStatus = Lens.lens (\ListRecordingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListRecordingConfigurationsResponse' {} a -> s {httpStatus = a} :: ListRecordingConfigurationsResponse)

-- | List of the matching recording configurations.
listRecordingConfigurationsResponse_recordingConfigurations :: Lens.Lens' ListRecordingConfigurationsResponse [RecordingConfigurationSummary]
listRecordingConfigurationsResponse_recordingConfigurations = Lens.lens (\ListRecordingConfigurationsResponse' {recordingConfigurations} -> recordingConfigurations) (\s@ListRecordingConfigurationsResponse' {} a -> s {recordingConfigurations = a} :: ListRecordingConfigurationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListRecordingConfigurationsResponse
  where
  rnf ListRecordingConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf recordingConfigurations
