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
-- Module      : Amazonka.Lambda.ListProvisionedConcurrencyConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of provisioned concurrency configurations for a
-- function.
--
-- This operation returns paginated results.
module Amazonka.Lambda.ListProvisionedConcurrencyConfigs
  ( -- * Creating a Request
    ListProvisionedConcurrencyConfigs (..),
    newListProvisionedConcurrencyConfigs,

    -- * Request Lenses
    listProvisionedConcurrencyConfigs_marker,
    listProvisionedConcurrencyConfigs_maxItems,
    listProvisionedConcurrencyConfigs_functionName,

    -- * Destructuring the Response
    ListProvisionedConcurrencyConfigsResponse (..),
    newListProvisionedConcurrencyConfigsResponse,

    -- * Response Lenses
    listProvisionedConcurrencyConfigsResponse_nextMarker,
    listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs,
    listProvisionedConcurrencyConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProvisionedConcurrencyConfigs' smart constructor.
data ListProvisionedConcurrencyConfigs = ListProvisionedConcurrencyConfigs'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Specify a number to limit the number of configurations returned.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisionedConcurrencyConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listProvisionedConcurrencyConfigs_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'maxItems', 'listProvisionedConcurrencyConfigs_maxItems' - Specify a number to limit the number of configurations returned.
--
-- 'functionName', 'listProvisionedConcurrencyConfigs_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newListProvisionedConcurrencyConfigs ::
  -- | 'functionName'
  Prelude.Text ->
  ListProvisionedConcurrencyConfigs
newListProvisionedConcurrencyConfigs pFunctionName_ =
  ListProvisionedConcurrencyConfigs'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listProvisionedConcurrencyConfigs_marker :: Lens.Lens' ListProvisionedConcurrencyConfigs (Prelude.Maybe Prelude.Text)
listProvisionedConcurrencyConfigs_marker = Lens.lens (\ListProvisionedConcurrencyConfigs' {marker} -> marker) (\s@ListProvisionedConcurrencyConfigs' {} a -> s {marker = a} :: ListProvisionedConcurrencyConfigs)

-- | Specify a number to limit the number of configurations returned.
listProvisionedConcurrencyConfigs_maxItems :: Lens.Lens' ListProvisionedConcurrencyConfigs (Prelude.Maybe Prelude.Natural)
listProvisionedConcurrencyConfigs_maxItems = Lens.lens (\ListProvisionedConcurrencyConfigs' {maxItems} -> maxItems) (\s@ListProvisionedConcurrencyConfigs' {} a -> s {maxItems = a} :: ListProvisionedConcurrencyConfigs)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
listProvisionedConcurrencyConfigs_functionName :: Lens.Lens' ListProvisionedConcurrencyConfigs Prelude.Text
listProvisionedConcurrencyConfigs_functionName = Lens.lens (\ListProvisionedConcurrencyConfigs' {functionName} -> functionName) (\s@ListProvisionedConcurrencyConfigs' {} a -> s {functionName = a} :: ListProvisionedConcurrencyConfigs)

instance
  Core.AWSPager
    ListProvisionedConcurrencyConfigs
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisionedConcurrencyConfigsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listProvisionedConcurrencyConfigs_marker
          Lens..~ rs
          Lens.^? listProvisionedConcurrencyConfigsResponse_nextMarker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListProvisionedConcurrencyConfigs
  where
  type
    AWSResponse ListProvisionedConcurrencyConfigs =
      ListProvisionedConcurrencyConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisionedConcurrencyConfigsResponse'
            Prelude.<$> (x Data..?> "NextMarker")
            Prelude.<*> ( x
                            Data..?> "ProvisionedConcurrencyConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListProvisionedConcurrencyConfigs
  where
  hashWithSalt
    _salt
    ListProvisionedConcurrencyConfigs' {..} =
      _salt
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` functionName

instance
  Prelude.NFData
    ListProvisionedConcurrencyConfigs
  where
  rnf ListProvisionedConcurrencyConfigs' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf functionName

instance
  Data.ToHeaders
    ListProvisionedConcurrencyConfigs
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListProvisionedConcurrencyConfigs
  where
  toPath ListProvisionedConcurrencyConfigs' {..} =
    Prelude.mconcat
      [ "/2019-09-30/functions/",
        Data.toBS functionName,
        "/provisioned-concurrency"
      ]

instance
  Data.ToQuery
    ListProvisionedConcurrencyConfigs
  where
  toQuery ListProvisionedConcurrencyConfigs' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "List=ALL"
      ]

-- | /See:/ 'newListProvisionedConcurrencyConfigsResponse' smart constructor.
data ListProvisionedConcurrencyConfigsResponse = ListProvisionedConcurrencyConfigsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A list of provisioned concurrency configurations.
    provisionedConcurrencyConfigs :: Prelude.Maybe [ProvisionedConcurrencyConfigListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisionedConcurrencyConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listProvisionedConcurrencyConfigsResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'provisionedConcurrencyConfigs', 'listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs' - A list of provisioned concurrency configurations.
--
-- 'httpStatus', 'listProvisionedConcurrencyConfigsResponse_httpStatus' - The response's http status code.
newListProvisionedConcurrencyConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProvisionedConcurrencyConfigsResponse
newListProvisionedConcurrencyConfigsResponse
  pHttpStatus_ =
    ListProvisionedConcurrencyConfigsResponse'
      { nextMarker =
          Prelude.Nothing,
        provisionedConcurrencyConfigs =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token that\'s included if more results are available.
listProvisionedConcurrencyConfigsResponse_nextMarker :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Prelude.Maybe Prelude.Text)
listProvisionedConcurrencyConfigsResponse_nextMarker = Lens.lens (\ListProvisionedConcurrencyConfigsResponse' {nextMarker} -> nextMarker) (\s@ListProvisionedConcurrencyConfigsResponse' {} a -> s {nextMarker = a} :: ListProvisionedConcurrencyConfigsResponse)

-- | A list of provisioned concurrency configurations.
listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Prelude.Maybe [ProvisionedConcurrencyConfigListItem])
listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs = Lens.lens (\ListProvisionedConcurrencyConfigsResponse' {provisionedConcurrencyConfigs} -> provisionedConcurrencyConfigs) (\s@ListProvisionedConcurrencyConfigsResponse' {} a -> s {provisionedConcurrencyConfigs = a} :: ListProvisionedConcurrencyConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProvisionedConcurrencyConfigsResponse_httpStatus :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse Prelude.Int
listProvisionedConcurrencyConfigsResponse_httpStatus = Lens.lens (\ListProvisionedConcurrencyConfigsResponse' {httpStatus} -> httpStatus) (\s@ListProvisionedConcurrencyConfigsResponse' {} a -> s {httpStatus = a} :: ListProvisionedConcurrencyConfigsResponse)

instance
  Prelude.NFData
    ListProvisionedConcurrencyConfigsResponse
  where
  rnf ListProvisionedConcurrencyConfigsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf provisionedConcurrencyConfigs
      `Prelude.seq` Prelude.rnf httpStatus
