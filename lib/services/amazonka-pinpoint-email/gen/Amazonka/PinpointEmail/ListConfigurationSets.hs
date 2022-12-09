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
-- Module      : Amazonka.PinpointEmail.ListConfigurationSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all of the configuration sets associated with your Amazon Pinpoint
-- account in the current region.
--
-- In Amazon Pinpoint, /configuration sets/ are groups of rules that you
-- can apply to the emails you send. You apply a configuration set to an
-- email by including a reference to the configuration set in the headers
-- of the email. When you apply a configuration set to an email, all of the
-- rules in that configuration set are applied to the email.
--
-- This operation returns paginated results.
module Amazonka.PinpointEmail.ListConfigurationSets
  ( -- * Creating a Request
    ListConfigurationSets (..),
    newListConfigurationSets,

    -- * Request Lenses
    listConfigurationSets_nextToken,
    listConfigurationSets_pageSize,

    -- * Destructuring the Response
    ListConfigurationSetsResponse (..),
    newListConfigurationSetsResponse,

    -- * Response Lenses
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to obtain a list of configuration sets for your Amazon
-- Pinpoint account in the current AWS Region.
--
-- /See:/ 'newListConfigurationSets' smart constructor.
data ListConfigurationSets = ListConfigurationSets'
  { -- | A token returned from a previous call to @ListConfigurationSets@ to
    -- indicate the position in the list of configuration sets.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to
    -- @ListConfigurationSets@. If the number of results is larger than the
    -- number you specified in this parameter, then the response includes a
    -- @NextToken@ element, which you can use to obtain additional results.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationSets_nextToken' - A token returned from a previous call to @ListConfigurationSets@ to
-- indicate the position in the list of configuration sets.
--
-- 'pageSize', 'listConfigurationSets_pageSize' - The number of results to show in a single call to
-- @ListConfigurationSets@. If the number of results is larger than the
-- number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
newListConfigurationSets ::
  ListConfigurationSets
newListConfigurationSets =
  ListConfigurationSets'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token returned from a previous call to @ListConfigurationSets@ to
-- indicate the position in the list of configuration sets.
listConfigurationSets_nextToken :: Lens.Lens' ListConfigurationSets (Prelude.Maybe Prelude.Text)
listConfigurationSets_nextToken = Lens.lens (\ListConfigurationSets' {nextToken} -> nextToken) (\s@ListConfigurationSets' {} a -> s {nextToken = a} :: ListConfigurationSets)

-- | The number of results to show in a single call to
-- @ListConfigurationSets@. If the number of results is larger than the
-- number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
listConfigurationSets_pageSize :: Lens.Lens' ListConfigurationSets (Prelude.Maybe Prelude.Int)
listConfigurationSets_pageSize = Lens.lens (\ListConfigurationSets' {pageSize} -> pageSize) (\s@ListConfigurationSets' {} a -> s {pageSize = a} :: ListConfigurationSets)

instance Core.AWSPager ListConfigurationSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfigurationSetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConfigurationSetsResponse_configurationSets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConfigurationSets_nextToken
          Lens..~ rs
          Lens.^? listConfigurationSetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListConfigurationSets where
  type
    AWSResponse ListConfigurationSets =
      ListConfigurationSetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationSetsResponse'
            Prelude.<$> ( x Data..?> "ConfigurationSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurationSets where
  hashWithSalt _salt ListConfigurationSets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListConfigurationSets where
  rnf ListConfigurationSets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders ListConfigurationSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConfigurationSets where
  toPath = Prelude.const "/v1/email/configuration-sets"

instance Data.ToQuery ListConfigurationSets where
  toQuery ListConfigurationSets' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "PageSize" Data.=: pageSize
      ]

-- | A list of configuration sets in your Amazon Pinpoint account in the
-- current AWS Region.
--
-- /See:/ 'newListConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { -- | An array that contains all of the configuration sets in your Amazon
    -- Pinpoint account in the current AWS Region.
    configurationSets :: Prelude.Maybe [Prelude.Text],
    -- | A token that indicates that there are additional configuration sets to
    -- list. To view additional configuration sets, issue another request to
    -- @ListConfigurationSets@, and pass this token in the @NextToken@
    -- parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSets', 'listConfigurationSetsResponse_configurationSets' - An array that contains all of the configuration sets in your Amazon
-- Pinpoint account in the current AWS Region.
--
-- 'nextToken', 'listConfigurationSetsResponse_nextToken' - A token that indicates that there are additional configuration sets to
-- list. To view additional configuration sets, issue another request to
-- @ListConfigurationSets@, and pass this token in the @NextToken@
-- parameter.
--
-- 'httpStatus', 'listConfigurationSetsResponse_httpStatus' - The response's http status code.
newListConfigurationSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigurationSetsResponse
newListConfigurationSetsResponse pHttpStatus_ =
  ListConfigurationSetsResponse'
    { configurationSets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that contains all of the configuration sets in your Amazon
-- Pinpoint account in the current AWS Region.
listConfigurationSetsResponse_configurationSets :: Lens.Lens' ListConfigurationSetsResponse (Prelude.Maybe [Prelude.Text])
listConfigurationSetsResponse_configurationSets = Lens.lens (\ListConfigurationSetsResponse' {configurationSets} -> configurationSets) (\s@ListConfigurationSetsResponse' {} a -> s {configurationSets = a} :: ListConfigurationSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there are additional configuration sets to
-- list. To view additional configuration sets, issue another request to
-- @ListConfigurationSets@, and pass this token in the @NextToken@
-- parameter.
listConfigurationSetsResponse_nextToken :: Lens.Lens' ListConfigurationSetsResponse (Prelude.Maybe Prelude.Text)
listConfigurationSetsResponse_nextToken = Lens.lens (\ListConfigurationSetsResponse' {nextToken} -> nextToken) (\s@ListConfigurationSetsResponse' {} a -> s {nextToken = a} :: ListConfigurationSetsResponse)

-- | The response's http status code.
listConfigurationSetsResponse_httpStatus :: Lens.Lens' ListConfigurationSetsResponse Prelude.Int
listConfigurationSetsResponse_httpStatus = Lens.lens (\ListConfigurationSetsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationSetsResponse' {} a -> s {httpStatus = a} :: ListConfigurationSetsResponse)

instance Prelude.NFData ListConfigurationSetsResponse where
  rnf ListConfigurationSetsResponse' {..} =
    Prelude.rnf configurationSets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
