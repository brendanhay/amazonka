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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new ReplicationConfigurationTemplate.
--
-- This operation returns paginated results.
module Amazonka.MGN.DescribeLaunchConfigurationTemplates
  ( -- * Creating a Request
    DescribeLaunchConfigurationTemplates (..),
    newDescribeLaunchConfigurationTemplates,

    -- * Request Lenses
    describeLaunchConfigurationTemplates_nextToken,
    describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs,
    describeLaunchConfigurationTemplates_maxResults,

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
  { -- | Request to disconnect Source Server from service by Server ID.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Request to disconnect Source Server from service by Server ID.
    launchConfigurationTemplateIDs :: Prelude.Maybe [Prelude.Text],
    -- | Request to disconnect Source Server from service by Server ID.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeLaunchConfigurationTemplates_nextToken' - Request to disconnect Source Server from service by Server ID.
--
-- 'launchConfigurationTemplateIDs', 'describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs' - Request to disconnect Source Server from service by Server ID.
--
-- 'maxResults', 'describeLaunchConfigurationTemplates_maxResults' - Request to disconnect Source Server from service by Server ID.
newDescribeLaunchConfigurationTemplates ::
  DescribeLaunchConfigurationTemplates
newDescribeLaunchConfigurationTemplates =
  DescribeLaunchConfigurationTemplates'
    { nextToken =
        Prelude.Nothing,
      launchConfigurationTemplateIDs =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Request to disconnect Source Server from service by Server ID.
describeLaunchConfigurationTemplates_nextToken :: Lens.Lens' DescribeLaunchConfigurationTemplates (Prelude.Maybe Prelude.Text)
describeLaunchConfigurationTemplates_nextToken = Lens.lens (\DescribeLaunchConfigurationTemplates' {nextToken} -> nextToken) (\s@DescribeLaunchConfigurationTemplates' {} a -> s {nextToken = a} :: DescribeLaunchConfigurationTemplates)

-- | Request to disconnect Source Server from service by Server ID.
describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs :: Lens.Lens' DescribeLaunchConfigurationTemplates (Prelude.Maybe [Prelude.Text])
describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs = Lens.lens (\DescribeLaunchConfigurationTemplates' {launchConfigurationTemplateIDs} -> launchConfigurationTemplateIDs) (\s@DescribeLaunchConfigurationTemplates' {} a -> s {launchConfigurationTemplateIDs = a} :: DescribeLaunchConfigurationTemplates) Prelude.. Lens.mapping Lens.coerced

-- | Request to disconnect Source Server from service by Server ID.
describeLaunchConfigurationTemplates_maxResults :: Lens.Lens' DescribeLaunchConfigurationTemplates (Prelude.Maybe Prelude.Natural)
describeLaunchConfigurationTemplates_maxResults = Lens.lens (\DescribeLaunchConfigurationTemplates' {maxResults} -> maxResults) (\s@DescribeLaunchConfigurationTemplates' {} a -> s {maxResults = a} :: DescribeLaunchConfigurationTemplates)

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
      Prelude.Just Prelude.$
        rq
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
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` launchConfigurationTemplateIDs
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    DescribeLaunchConfigurationTemplates
  where
  rnf DescribeLaunchConfigurationTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateIDs
      `Prelude.seq` Prelude.rnf maxResults

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
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("launchConfigurationTemplateIDs" Data..=)
              Prelude.<$> launchConfigurationTemplateIDs,
            ("maxResults" Data..=) Prelude.<$> maxResults
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
  { -- | Request to disconnect Source Server from service by Server ID.
    items :: Prelude.Maybe [LaunchConfigurationTemplate],
    -- | Request to disconnect Source Server from service by Server ID.
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
-- 'items', 'describeLaunchConfigurationTemplatesResponse_items' - Request to disconnect Source Server from service by Server ID.
--
-- 'nextToken', 'describeLaunchConfigurationTemplatesResponse_nextToken' - Request to disconnect Source Server from service by Server ID.
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

-- | Request to disconnect Source Server from service by Server ID.
describeLaunchConfigurationTemplatesResponse_items :: Lens.Lens' DescribeLaunchConfigurationTemplatesResponse (Prelude.Maybe [LaunchConfigurationTemplate])
describeLaunchConfigurationTemplatesResponse_items = Lens.lens (\DescribeLaunchConfigurationTemplatesResponse' {items} -> items) (\s@DescribeLaunchConfigurationTemplatesResponse' {} a -> s {items = a} :: DescribeLaunchConfigurationTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Request to disconnect Source Server from service by Server ID.
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
