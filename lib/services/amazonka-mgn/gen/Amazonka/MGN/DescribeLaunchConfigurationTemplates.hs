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
-- Module      : Amazonka.MGN.DescribeLaunchConfigurationTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Launch Configuration Templates, filtered by Launch
-- Configuration Template IDs
--
-- This operation returns paginated results.
module Amazonka.MGN.DescribeLaunchConfigurationTemplates
  ( -- * Creating a Request
    DescribeLaunchConfigurationTemplates (..),
    newDescribeLaunchConfigurationTemplates,

    -- * Request Lenses
    describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs,
    describeLaunchConfigurationTemplates_maxResults,
    describeLaunchConfigurationTemplates_nextToken,

    -- * Destructuring the Response
    DescribeLaunchConfigurationTemplatesResponse (..),
    newDescribeLaunchConfigurationTemplatesResponse,

    -- * Response Lenses
    describeLaunchConfigurationTemplatesResponse_items,
    describeLaunchConfigurationTemplatesResponse_nextToken,
    describeLaunchConfigurationTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLaunchConfigurationTemplates' smart constructor.
data DescribeLaunchConfigurationTemplates = DescribeLaunchConfigurationTemplates'
  { -- | Request to filter Launch Configuration Templates list by Launch
    -- Configuration Template ID.
    launchConfigurationTemplateIDs :: Prelude.Maybe [Prelude.Text],
    -- | Maximum results to be returned in DescribeLaunchConfigurationTemplates.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next pagination token returned from
    -- DescribeLaunchConfigurationTemplates.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLaunchConfigurationTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchConfigurationTemplateIDs', 'describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs' - Request to filter Launch Configuration Templates list by Launch
-- Configuration Template ID.
--
-- 'maxResults', 'describeLaunchConfigurationTemplates_maxResults' - Maximum results to be returned in DescribeLaunchConfigurationTemplates.
--
-- 'nextToken', 'describeLaunchConfigurationTemplates_nextToken' - Next pagination token returned from
-- DescribeLaunchConfigurationTemplates.
newDescribeLaunchConfigurationTemplates ::
  DescribeLaunchConfigurationTemplates
newDescribeLaunchConfigurationTemplates =
  DescribeLaunchConfigurationTemplates'
    { launchConfigurationTemplateIDs =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Request to filter Launch Configuration Templates list by Launch
-- Configuration Template ID.
describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs :: Lens.Lens' DescribeLaunchConfigurationTemplates (Prelude.Maybe [Prelude.Text])
describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs = Lens.lens (\DescribeLaunchConfigurationTemplates' {launchConfigurationTemplateIDs} -> launchConfigurationTemplateIDs) (\s@DescribeLaunchConfigurationTemplates' {} a -> s {launchConfigurationTemplateIDs = a} :: DescribeLaunchConfigurationTemplates) Prelude.. Lens.mapping Lens.coerced

-- | Maximum results to be returned in DescribeLaunchConfigurationTemplates.
describeLaunchConfigurationTemplates_maxResults :: Lens.Lens' DescribeLaunchConfigurationTemplates (Prelude.Maybe Prelude.Natural)
describeLaunchConfigurationTemplates_maxResults = Lens.lens (\DescribeLaunchConfigurationTemplates' {maxResults} -> maxResults) (\s@DescribeLaunchConfigurationTemplates' {} a -> s {maxResults = a} :: DescribeLaunchConfigurationTemplates)

-- | Next pagination token returned from
-- DescribeLaunchConfigurationTemplates.
describeLaunchConfigurationTemplates_nextToken :: Lens.Lens' DescribeLaunchConfigurationTemplates (Prelude.Maybe Prelude.Text)
describeLaunchConfigurationTemplates_nextToken = Lens.lens (\DescribeLaunchConfigurationTemplates' {nextToken} -> nextToken) (\s@DescribeLaunchConfigurationTemplates' {} a -> s {nextToken = a} :: DescribeLaunchConfigurationTemplates)

instance
  Core.AWSPager
    DescribeLaunchConfigurationTemplates
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLaunchConfigurationTemplatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLaunchConfigurationTemplatesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeLaunchConfigurationTemplates_nextToken
          Lens..~ rs
          Lens.^? describeLaunchConfigurationTemplatesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeLaunchConfigurationTemplates
  where
  type
    AWSResponse DescribeLaunchConfigurationTemplates =
      DescribeLaunchConfigurationTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLaunchConfigurationTemplatesResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLaunchConfigurationTemplates
  where
  hashWithSalt
    _salt
    DescribeLaunchConfigurationTemplates' {..} =
      _salt
        `Prelude.hashWithSalt` launchConfigurationTemplateIDs
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeLaunchConfigurationTemplates
  where
  rnf DescribeLaunchConfigurationTemplates' {..} =
    Prelude.rnf launchConfigurationTemplateIDs
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeLaunchConfigurationTemplates
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

instance
  Data.ToJSON
    DescribeLaunchConfigurationTemplates
  where
  toJSON DescribeLaunchConfigurationTemplates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("launchConfigurationTemplateIDs" Data..=)
              Prelude.<$> launchConfigurationTemplateIDs,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    DescribeLaunchConfigurationTemplates
  where
  toPath =
    Prelude.const
      "/DescribeLaunchConfigurationTemplates"

instance
  Data.ToQuery
    DescribeLaunchConfigurationTemplates
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLaunchConfigurationTemplatesResponse' smart constructor.
data DescribeLaunchConfigurationTemplatesResponse = DescribeLaunchConfigurationTemplatesResponse'
  { -- | List of items returned by DescribeLaunchConfigurationTemplates.
    items :: Prelude.Maybe [LaunchConfigurationTemplate],
    -- | Next pagination token returned from
    -- DescribeLaunchConfigurationTemplates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLaunchConfigurationTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'describeLaunchConfigurationTemplatesResponse_items' - List of items returned by DescribeLaunchConfigurationTemplates.
--
-- 'nextToken', 'describeLaunchConfigurationTemplatesResponse_nextToken' - Next pagination token returned from
-- DescribeLaunchConfigurationTemplates.
--
-- 'httpStatus', 'describeLaunchConfigurationTemplatesResponse_httpStatus' - The response's http status code.
newDescribeLaunchConfigurationTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLaunchConfigurationTemplatesResponse
newDescribeLaunchConfigurationTemplatesResponse
  pHttpStatus_ =
    DescribeLaunchConfigurationTemplatesResponse'
      { items =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | List of items returned by DescribeLaunchConfigurationTemplates.
describeLaunchConfigurationTemplatesResponse_items :: Lens.Lens' DescribeLaunchConfigurationTemplatesResponse (Prelude.Maybe [LaunchConfigurationTemplate])
describeLaunchConfigurationTemplatesResponse_items = Lens.lens (\DescribeLaunchConfigurationTemplatesResponse' {items} -> items) (\s@DescribeLaunchConfigurationTemplatesResponse' {} a -> s {items = a} :: DescribeLaunchConfigurationTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next pagination token returned from
-- DescribeLaunchConfigurationTemplates.
describeLaunchConfigurationTemplatesResponse_nextToken :: Lens.Lens' DescribeLaunchConfigurationTemplatesResponse (Prelude.Maybe Prelude.Text)
describeLaunchConfigurationTemplatesResponse_nextToken = Lens.lens (\DescribeLaunchConfigurationTemplatesResponse' {nextToken} -> nextToken) (\s@DescribeLaunchConfigurationTemplatesResponse' {} a -> s {nextToken = a} :: DescribeLaunchConfigurationTemplatesResponse)

-- | The response's http status code.
describeLaunchConfigurationTemplatesResponse_httpStatus :: Lens.Lens' DescribeLaunchConfigurationTemplatesResponse Prelude.Int
describeLaunchConfigurationTemplatesResponse_httpStatus = Lens.lens (\DescribeLaunchConfigurationTemplatesResponse' {httpStatus} -> httpStatus) (\s@DescribeLaunchConfigurationTemplatesResponse' {} a -> s {httpStatus = a} :: DescribeLaunchConfigurationTemplatesResponse)

instance
  Prelude.NFData
    DescribeLaunchConfigurationTemplatesResponse
  where
  rnf DescribeLaunchConfigurationTemplatesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
