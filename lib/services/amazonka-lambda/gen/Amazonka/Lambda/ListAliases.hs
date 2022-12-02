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
-- Module      : Amazonka.Lambda.ListAliases
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Lambda.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_marker,
    listAliases_functionVersion,
    listAliases_maxItems,
    listAliases_functionName,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_aliases,
    listAliasesResponse_nextMarker,
    listAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Specify a function version to only list aliases that invoke that
    -- version.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | Limit the number of aliases returned.
    maxItems :: Prelude.Maybe Prelude.Natural,
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
-- 'marker', 'listAliases_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'functionVersion', 'listAliases_functionVersion' - Specify a function version to only list aliases that invoke that
-- version.
--
-- 'maxItems', 'listAliases_maxItems' - Limit the number of aliases returned.
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
    { marker = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listAliases_marker :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_marker = Lens.lens (\ListAliases' {marker} -> marker) (\s@ListAliases' {} a -> s {marker = a} :: ListAliases)

-- | Specify a function version to only list aliases that invoke that
-- version.
listAliases_functionVersion :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_functionVersion = Lens.lens (\ListAliases' {functionVersion} -> functionVersion) (\s@ListAliases' {} a -> s {functionVersion = a} :: ListAliases)

-- | Limit the number of aliases returned.
listAliases_maxItems :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_maxItems = Lens.lens (\ListAliases' {maxItems} -> maxItems) (\s@ListAliases' {} a -> s {maxItems = a} :: ListAliases)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Prelude.<$> (x Data..?> "Aliases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAliases where
  hashWithSalt _salt ListAliases' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData ListAliases where
  rnf ListAliases' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders ListAliases where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAliases where
  toPath ListAliases' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/aliases"
      ]

instance Data.ToQuery ListAliases where
  toQuery ListAliases' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "FunctionVersion" Data.=: functionVersion,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | A list of aliases.
    aliases :: Prelude.Maybe [AliasConfiguration],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
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
-- 'aliases', 'listAliasesResponse_aliases' - A list of aliases.
--
-- 'nextMarker', 'listAliasesResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { aliases = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of aliases.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Prelude.Maybe [AliasConfiguration])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
listAliasesResponse_nextMarker :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Text)
listAliasesResponse_nextMarker = Lens.lens (\ListAliasesResponse' {nextMarker} -> nextMarker) (\s@ListAliasesResponse' {} a -> s {nextMarker = a} :: ListAliasesResponse)

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Prelude.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Prelude.NFData ListAliasesResponse where
  rnf ListAliasesResponse' {..} =
    Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
