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
-- Module      : Network.AWS.Lambda.ListAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html aliases>
-- for a Lambda function.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_functionVersion,
    listAliases_maxItems,
    listAliases_marker,
    listAliases_functionName,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_nextMarker,
    listAliasesResponse_aliases,
    listAliasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | Specify a function version to only list aliases that invoke that
    -- version.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | Limit the number of aliases returned.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionVersion', 'listAliases_functionVersion' - Specify a function version to only list aliases that invoke that
-- version.
--
-- 'maxItems', 'listAliases_maxItems' - Limit the number of aliases returned.
--
-- 'marker', 'listAliases_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'functionName', 'listAliases_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newListAliases ::
  -- | 'functionName'
  Prelude.Text ->
  ListAliases
newListAliases pFunctionName_ =
  ListAliases'
    { functionVersion = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a function version to only list aliases that invoke that
-- version.
listAliases_functionVersion :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_functionVersion = Lens.lens (\ListAliases' {functionVersion} -> functionVersion) (\s@ListAliases' {} a -> s {functionVersion = a} :: ListAliases)

-- | Limit the number of aliases returned.
listAliases_maxItems :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_maxItems = Lens.lens (\ListAliases' {maxItems} -> maxItems) (\s@ListAliases' {} a -> s {maxItems = a} :: ListAliases)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listAliases_marker :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_marker = Lens.lens (\ListAliases' {marker} -> marker) (\s@ListAliases' {} a -> s {marker = a} :: ListAliases)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
listAliases_functionName :: Lens.Lens' ListAliases Prelude.Text
listAliases_functionName = Lens.lens (\ListAliases' {functionName} -> functionName) (\s@ListAliases' {} a -> s {functionName = a} :: ListAliases)

instance Core.AWSPager ListAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_aliases Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAliases_marker
          Lens..~ rs
          Lens.^? listAliasesResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest ListAliases where
  type AWSResponse ListAliases = ListAliasesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> (x Core..?> "Aliases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAliases

instance Prelude.NFData ListAliases

instance Core.ToHeaders ListAliases where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAliases where
  toPath ListAliases' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/aliases"
      ]

instance Core.ToQuery ListAliases where
  toQuery ListAliases' {..} =
    Prelude.mconcat
      [ "FunctionVersion" Core.=: functionVersion,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A list of aliases.
    aliases :: Prelude.Maybe [AliasConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listAliasesResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'aliases', 'listAliasesResponse_aliases' - A list of aliases.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { nextMarker = Prelude.Nothing,
      aliases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s included if more results are available.
listAliasesResponse_nextMarker :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Text)
listAliasesResponse_nextMarker = Lens.lens (\ListAliasesResponse' {nextMarker} -> nextMarker) (\s@ListAliasesResponse' {} a -> s {nextMarker = a} :: ListAliasesResponse)

-- | A list of aliases.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Prelude.Maybe [AliasConfiguration])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Prelude.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Prelude.NFData ListAliasesResponse
