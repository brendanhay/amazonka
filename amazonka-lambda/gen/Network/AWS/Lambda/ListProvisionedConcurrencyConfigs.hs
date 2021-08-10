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
-- Module      : Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of provisioned concurrency configurations for a
-- function.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
  ( -- * Creating a Request
    ListProvisionedConcurrencyConfigs (..),
    newListProvisionedConcurrencyConfigs,

    -- * Request Lenses
    listProvisionedConcurrencyConfigs_maxItems,
    listProvisionedConcurrencyConfigs_marker,
    listProvisionedConcurrencyConfigs_functionName,

    -- * Destructuring the Response
    ListProvisionedConcurrencyConfigsResponse (..),
    newListProvisionedConcurrencyConfigsResponse,

    -- * Response Lenses
    listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs,
    listProvisionedConcurrencyConfigsResponse_nextMarker,
    listProvisionedConcurrencyConfigsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProvisionedConcurrencyConfigs' smart constructor.
data ListProvisionedConcurrencyConfigs = ListProvisionedConcurrencyConfigs'
  { -- | Specify a number to limit the number of configurations returned.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
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
-- 'maxItems', 'listProvisionedConcurrencyConfigs_maxItems' - Specify a number to limit the number of configurations returned.
--
-- 'marker', 'listProvisionedConcurrencyConfigs_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'functionName', 'listProvisionedConcurrencyConfigs_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newListProvisionedConcurrencyConfigs ::
  -- | 'functionName'
  Prelude.Text ->
  ListProvisionedConcurrencyConfigs
newListProvisionedConcurrencyConfigs pFunctionName_ =
  ListProvisionedConcurrencyConfigs'
    { maxItems =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a number to limit the number of configurations returned.
listProvisionedConcurrencyConfigs_maxItems :: Lens.Lens' ListProvisionedConcurrencyConfigs (Prelude.Maybe Prelude.Natural)
listProvisionedConcurrencyConfigs_maxItems = Lens.lens (\ListProvisionedConcurrencyConfigs' {maxItems} -> maxItems) (\s@ListProvisionedConcurrencyConfigs' {} a -> s {maxItems = a} :: ListProvisionedConcurrencyConfigs)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listProvisionedConcurrencyConfigs_marker :: Lens.Lens' ListProvisionedConcurrencyConfigs (Prelude.Maybe Prelude.Text)
listProvisionedConcurrencyConfigs_marker = Lens.lens (\ListProvisionedConcurrencyConfigs' {marker} -> marker) (\s@ListProvisionedConcurrencyConfigs' {} a -> s {marker = a} :: ListProvisionedConcurrencyConfigs)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
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
      Prelude.Just Prelude.$
        rq
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisionedConcurrencyConfigsResponse'
            Prelude.<$> ( x Core..?> "ProvisionedConcurrencyConfigs"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextMarker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListProvisionedConcurrencyConfigs

instance
  Prelude.NFData
    ListProvisionedConcurrencyConfigs

instance
  Core.ToHeaders
    ListProvisionedConcurrencyConfigs
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ListProvisionedConcurrencyConfigs
  where
  toPath ListProvisionedConcurrencyConfigs' {..} =
    Prelude.mconcat
      [ "/2019-09-30/functions/",
        Core.toBS functionName,
        "/provisioned-concurrency"
      ]

instance
  Core.ToQuery
    ListProvisionedConcurrencyConfigs
  where
  toQuery ListProvisionedConcurrencyConfigs' {..} =
    Prelude.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "List=ALL"
      ]

-- | /See:/ 'newListProvisionedConcurrencyConfigsResponse' smart constructor.
data ListProvisionedConcurrencyConfigsResponse = ListProvisionedConcurrencyConfigsResponse'
  { -- | A list of provisioned concurrency configurations.
    provisionedConcurrencyConfigs :: Prelude.Maybe [ProvisionedConcurrencyConfigListItem],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
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
-- 'provisionedConcurrencyConfigs', 'listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs' - A list of provisioned concurrency configurations.
--
-- 'nextMarker', 'listProvisionedConcurrencyConfigsResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listProvisionedConcurrencyConfigsResponse_httpStatus' - The response's http status code.
newListProvisionedConcurrencyConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProvisionedConcurrencyConfigsResponse
newListProvisionedConcurrencyConfigsResponse
  pHttpStatus_ =
    ListProvisionedConcurrencyConfigsResponse'
      { provisionedConcurrencyConfigs =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of provisioned concurrency configurations.
listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Prelude.Maybe [ProvisionedConcurrencyConfigListItem])
listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs = Lens.lens (\ListProvisionedConcurrencyConfigsResponse' {provisionedConcurrencyConfigs} -> provisionedConcurrencyConfigs) (\s@ListProvisionedConcurrencyConfigsResponse' {} a -> s {provisionedConcurrencyConfigs = a} :: ListProvisionedConcurrencyConfigsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token that\'s included if more results are available.
listProvisionedConcurrencyConfigsResponse_nextMarker :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Prelude.Maybe Prelude.Text)
listProvisionedConcurrencyConfigsResponse_nextMarker = Lens.lens (\ListProvisionedConcurrencyConfigsResponse' {nextMarker} -> nextMarker) (\s@ListProvisionedConcurrencyConfigsResponse' {} a -> s {nextMarker = a} :: ListProvisionedConcurrencyConfigsResponse)

-- | The response's http status code.
listProvisionedConcurrencyConfigsResponse_httpStatus :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse Prelude.Int
listProvisionedConcurrencyConfigsResponse_httpStatus = Lens.lens (\ListProvisionedConcurrencyConfigsResponse' {httpStatus} -> httpStatus) (\s@ListProvisionedConcurrencyConfigsResponse' {} a -> s {httpStatus = a} :: ListProvisionedConcurrencyConfigsResponse)

instance
  Prelude.NFData
    ListProvisionedConcurrencyConfigsResponse
