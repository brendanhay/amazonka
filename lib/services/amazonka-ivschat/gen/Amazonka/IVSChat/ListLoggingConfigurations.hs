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
-- Module      : Amazonka.IVSChat.ListLoggingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about all your logging configurations in the
-- AWS region where the API request is processed.
module Amazonka.IVSChat.ListLoggingConfigurations
  ( -- * Creating a Request
    ListLoggingConfigurations (..),
    newListLoggingConfigurations,

    -- * Request Lenses
    listLoggingConfigurations_maxResults,
    listLoggingConfigurations_nextToken,

    -- * Destructuring the Response
    ListLoggingConfigurationsResponse (..),
    newListLoggingConfigurationsResponse,

    -- * Response Lenses
    listLoggingConfigurationsResponse_nextToken,
    listLoggingConfigurationsResponse_httpStatus,
    listLoggingConfigurationsResponse_loggingConfigurations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLoggingConfigurations' smart constructor.
data ListLoggingConfigurations = ListLoggingConfigurations'
  { -- | Maximum number of logging configurations to return. Default: 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first logging configurations to retrieve. This is used for
    -- pagination; see the @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLoggingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLoggingConfigurations_maxResults' - Maximum number of logging configurations to return. Default: 50.
--
-- 'nextToken', 'listLoggingConfigurations_nextToken' - The first logging configurations to retrieve. This is used for
-- pagination; see the @nextToken@ response field.
newListLoggingConfigurations ::
  ListLoggingConfigurations
newListLoggingConfigurations =
  ListLoggingConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of logging configurations to return. Default: 50.
listLoggingConfigurations_maxResults :: Lens.Lens' ListLoggingConfigurations (Prelude.Maybe Prelude.Natural)
listLoggingConfigurations_maxResults = Lens.lens (\ListLoggingConfigurations' {maxResults} -> maxResults) (\s@ListLoggingConfigurations' {} a -> s {maxResults = a} :: ListLoggingConfigurations)

-- | The first logging configurations to retrieve. This is used for
-- pagination; see the @nextToken@ response field.
listLoggingConfigurations_nextToken :: Lens.Lens' ListLoggingConfigurations (Prelude.Maybe Prelude.Text)
listLoggingConfigurations_nextToken = Lens.lens (\ListLoggingConfigurations' {nextToken} -> nextToken) (\s@ListLoggingConfigurations' {} a -> s {nextToken = a} :: ListLoggingConfigurations)

instance Core.AWSRequest ListLoggingConfigurations where
  type
    AWSResponse ListLoggingConfigurations =
      ListLoggingConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggingConfigurationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "loggingConfigurations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListLoggingConfigurations where
  hashWithSalt _salt ListLoggingConfigurations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLoggingConfigurations where
  rnf ListLoggingConfigurations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLoggingConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLoggingConfigurations where
  toJSON ListLoggingConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLoggingConfigurations where
  toPath = Prelude.const "/ListLoggingConfigurations"

instance Data.ToQuery ListLoggingConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLoggingConfigurationsResponse' smart constructor.
data ListLoggingConfigurationsResponse = ListLoggingConfigurationsResponse'
  { -- | If there are more logging configurations than @maxResults@, use
    -- @nextToken@ in the request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of the matching logging configurations (summary information only).
    -- There is only one type of destination (@cloudWatchLogs@, @firehose@, or
    -- @s3@) in a @destinationConfiguration@.
    loggingConfigurations :: [LoggingConfigurationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLoggingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLoggingConfigurationsResponse_nextToken' - If there are more logging configurations than @maxResults@, use
-- @nextToken@ in the request to get the next set.
--
-- 'httpStatus', 'listLoggingConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'loggingConfigurations', 'listLoggingConfigurationsResponse_loggingConfigurations' - List of the matching logging configurations (summary information only).
-- There is only one type of destination (@cloudWatchLogs@, @firehose@, or
-- @s3@) in a @destinationConfiguration@.
newListLoggingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLoggingConfigurationsResponse
newListLoggingConfigurationsResponse pHttpStatus_ =
  ListLoggingConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      loggingConfigurations = Prelude.mempty
    }

-- | If there are more logging configurations than @maxResults@, use
-- @nextToken@ in the request to get the next set.
listLoggingConfigurationsResponse_nextToken :: Lens.Lens' ListLoggingConfigurationsResponse (Prelude.Maybe Prelude.Text)
listLoggingConfigurationsResponse_nextToken = Lens.lens (\ListLoggingConfigurationsResponse' {nextToken} -> nextToken) (\s@ListLoggingConfigurationsResponse' {} a -> s {nextToken = a} :: ListLoggingConfigurationsResponse)

-- | The response's http status code.
listLoggingConfigurationsResponse_httpStatus :: Lens.Lens' ListLoggingConfigurationsResponse Prelude.Int
listLoggingConfigurationsResponse_httpStatus = Lens.lens (\ListLoggingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListLoggingConfigurationsResponse' {} a -> s {httpStatus = a} :: ListLoggingConfigurationsResponse)

-- | List of the matching logging configurations (summary information only).
-- There is only one type of destination (@cloudWatchLogs@, @firehose@, or
-- @s3@) in a @destinationConfiguration@.
listLoggingConfigurationsResponse_loggingConfigurations :: Lens.Lens' ListLoggingConfigurationsResponse [LoggingConfigurationSummary]
listLoggingConfigurationsResponse_loggingConfigurations = Lens.lens (\ListLoggingConfigurationsResponse' {loggingConfigurations} -> loggingConfigurations) (\s@ListLoggingConfigurationsResponse' {} a -> s {loggingConfigurations = a} :: ListLoggingConfigurationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListLoggingConfigurationsResponse
  where
  rnf ListLoggingConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf loggingConfigurations
