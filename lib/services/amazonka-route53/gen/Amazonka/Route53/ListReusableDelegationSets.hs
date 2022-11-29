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
-- Module      : Amazonka.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the reusable delegation sets that are associated
-- with the current Amazon Web Services account.
module Amazonka.Route53.ListReusableDelegationSets
  ( -- * Creating a Request
    ListReusableDelegationSets (..),
    newListReusableDelegationSets,

    -- * Request Lenses
    listReusableDelegationSets_marker,
    listReusableDelegationSets_maxItems,

    -- * Destructuring the Response
    ListReusableDelegationSetsResponse (..),
    newListReusableDelegationSetsResponse,

    -- * Response Lenses
    listReusableDelegationSetsResponse_nextMarker,
    listReusableDelegationSetsResponse_httpStatus,
    listReusableDelegationSetsResponse_delegationSets,
    listReusableDelegationSetsResponse_marker,
    listReusableDelegationSetsResponse_isTruncated,
    listReusableDelegationSetsResponse_maxItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to get a list of the reusable delegation sets that are
-- associated with the current Amazon Web Services account.
--
-- /See:/ 'newListReusableDelegationSets' smart constructor.
data ListReusableDelegationSets = ListReusableDelegationSets'
  { -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more reusable delegation sets. To get another group, submit another
    -- @ListReusableDelegationSets@ request.
    --
    -- For the value of @marker@, specify the value of @NextMarker@ from the
    -- previous response, which is the ID of the first reusable delegation set
    -- that Amazon Route 53 will return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more reusable delegation sets to get.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The number of reusable delegation sets that you want Amazon Route 53 to
    -- return in the response to this request. If you specify a value greater
    -- than 100, Route 53 returns only the first 100 reusable delegation sets.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReusableDelegationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listReusableDelegationSets_marker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more reusable delegation sets. To get another group, submit another
-- @ListReusableDelegationSets@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first reusable delegation set
-- that Amazon Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more reusable delegation sets to get.
--
-- 'maxItems', 'listReusableDelegationSets_maxItems' - The number of reusable delegation sets that you want Amazon Route 53 to
-- return in the response to this request. If you specify a value greater
-- than 100, Route 53 returns only the first 100 reusable delegation sets.
newListReusableDelegationSets ::
  ListReusableDelegationSets
newListReusableDelegationSets =
  ListReusableDelegationSets'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more reusable delegation sets. To get another group, submit another
-- @ListReusableDelegationSets@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first reusable delegation set
-- that Amazon Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more reusable delegation sets to get.
listReusableDelegationSets_marker :: Lens.Lens' ListReusableDelegationSets (Prelude.Maybe Prelude.Text)
listReusableDelegationSets_marker = Lens.lens (\ListReusableDelegationSets' {marker} -> marker) (\s@ListReusableDelegationSets' {} a -> s {marker = a} :: ListReusableDelegationSets)

-- | The number of reusable delegation sets that you want Amazon Route 53 to
-- return in the response to this request. If you specify a value greater
-- than 100, Route 53 returns only the first 100 reusable delegation sets.
listReusableDelegationSets_maxItems :: Lens.Lens' ListReusableDelegationSets (Prelude.Maybe Prelude.Text)
listReusableDelegationSets_maxItems = Lens.lens (\ListReusableDelegationSets' {maxItems} -> maxItems) (\s@ListReusableDelegationSets' {} a -> s {maxItems = a} :: ListReusableDelegationSets)

instance Core.AWSRequest ListReusableDelegationSets where
  type
    AWSResponse ListReusableDelegationSets =
      ListReusableDelegationSetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListReusableDelegationSetsResponse'
            Prelude.<$> (x Core..@? "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "DelegationSets" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "DelegationSet"
                        )
            Prelude.<*> (x Core..@ "Marker")
            Prelude.<*> (x Core..@ "IsTruncated")
            Prelude.<*> (x Core..@ "MaxItems")
      )

instance Prelude.Hashable ListReusableDelegationSets where
  hashWithSalt _salt ListReusableDelegationSets' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListReusableDelegationSets where
  rnf ListReusableDelegationSets' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Core.ToHeaders ListReusableDelegationSets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListReusableDelegationSets where
  toPath = Prelude.const "/2013-04-01/delegationset"

instance Core.ToQuery ListReusableDelegationSets where
  toQuery ListReusableDelegationSets' {..} =
    Prelude.mconcat
      [ "marker" Core.=: marker,
        "maxitems" Core.=: maxItems
      ]

-- | A complex type that contains information about the reusable delegation
-- sets that are associated with the current Amazon Web Services account.
--
-- /See:/ 'newListReusableDelegationSetsResponse' smart constructor.
data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse'
  { -- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
    -- next reusable delegation set that Amazon Route 53 will return if you
    -- submit another @ListReusableDelegationSets@ request and specify the
    -- value of @NextMarker@ in the @marker@ parameter.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains one @DelegationSet@ element for each
    -- reusable delegation set that was created by the current Amazon Web
    -- Services account.
    delegationSets :: [DelegationSet],
    -- | For the second and subsequent calls to @ListReusableDelegationSets@,
    -- @Marker@ is the value that you specified for the @marker@ parameter in
    -- the request that produced the current response.
    marker :: Prelude.Text,
    -- | A flag that indicates whether there are more reusable delegation sets to
    -- be listed.
    isTruncated :: Prelude.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to
    -- @ListReusableDelegationSets@ that produced the current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReusableDelegationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listReusableDelegationSetsResponse_nextMarker' - If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- next reusable delegation set that Amazon Route 53 will return if you
-- submit another @ListReusableDelegationSets@ request and specify the
-- value of @NextMarker@ in the @marker@ parameter.
--
-- 'httpStatus', 'listReusableDelegationSetsResponse_httpStatus' - The response's http status code.
--
-- 'delegationSets', 'listReusableDelegationSetsResponse_delegationSets' - A complex type that contains one @DelegationSet@ element for each
-- reusable delegation set that was created by the current Amazon Web
-- Services account.
--
-- 'marker', 'listReusableDelegationSetsResponse_marker' - For the second and subsequent calls to @ListReusableDelegationSets@,
-- @Marker@ is the value that you specified for the @marker@ parameter in
-- the request that produced the current response.
--
-- 'isTruncated', 'listReusableDelegationSetsResponse_isTruncated' - A flag that indicates whether there are more reusable delegation sets to
-- be listed.
--
-- 'maxItems', 'listReusableDelegationSetsResponse_maxItems' - The value that you specified for the @maxitems@ parameter in the call to
-- @ListReusableDelegationSets@ that produced the current response.
newListReusableDelegationSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'marker'
  Prelude.Text ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListReusableDelegationSetsResponse
newListReusableDelegationSetsResponse
  pHttpStatus_
  pMarker_
  pIsTruncated_
  pMaxItems_ =
    ListReusableDelegationSetsResponse'
      { nextMarker =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        delegationSets = Prelude.mempty,
        marker = pMarker_,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- next reusable delegation set that Amazon Route 53 will return if you
-- submit another @ListReusableDelegationSets@ request and specify the
-- value of @NextMarker@ in the @marker@ parameter.
listReusableDelegationSetsResponse_nextMarker :: Lens.Lens' ListReusableDelegationSetsResponse (Prelude.Maybe Prelude.Text)
listReusableDelegationSetsResponse_nextMarker = Lens.lens (\ListReusableDelegationSetsResponse' {nextMarker} -> nextMarker) (\s@ListReusableDelegationSetsResponse' {} a -> s {nextMarker = a} :: ListReusableDelegationSetsResponse)

-- | The response's http status code.
listReusableDelegationSetsResponse_httpStatus :: Lens.Lens' ListReusableDelegationSetsResponse Prelude.Int
listReusableDelegationSetsResponse_httpStatus = Lens.lens (\ListReusableDelegationSetsResponse' {httpStatus} -> httpStatus) (\s@ListReusableDelegationSetsResponse' {} a -> s {httpStatus = a} :: ListReusableDelegationSetsResponse)

-- | A complex type that contains one @DelegationSet@ element for each
-- reusable delegation set that was created by the current Amazon Web
-- Services account.
listReusableDelegationSetsResponse_delegationSets :: Lens.Lens' ListReusableDelegationSetsResponse [DelegationSet]
listReusableDelegationSetsResponse_delegationSets = Lens.lens (\ListReusableDelegationSetsResponse' {delegationSets} -> delegationSets) (\s@ListReusableDelegationSetsResponse' {} a -> s {delegationSets = a} :: ListReusableDelegationSetsResponse) Prelude.. Lens.coerced

-- | For the second and subsequent calls to @ListReusableDelegationSets@,
-- @Marker@ is the value that you specified for the @marker@ parameter in
-- the request that produced the current response.
listReusableDelegationSetsResponse_marker :: Lens.Lens' ListReusableDelegationSetsResponse Prelude.Text
listReusableDelegationSetsResponse_marker = Lens.lens (\ListReusableDelegationSetsResponse' {marker} -> marker) (\s@ListReusableDelegationSetsResponse' {} a -> s {marker = a} :: ListReusableDelegationSetsResponse)

-- | A flag that indicates whether there are more reusable delegation sets to
-- be listed.
listReusableDelegationSetsResponse_isTruncated :: Lens.Lens' ListReusableDelegationSetsResponse Prelude.Bool
listReusableDelegationSetsResponse_isTruncated = Lens.lens (\ListReusableDelegationSetsResponse' {isTruncated} -> isTruncated) (\s@ListReusableDelegationSetsResponse' {} a -> s {isTruncated = a} :: ListReusableDelegationSetsResponse)

-- | The value that you specified for the @maxitems@ parameter in the call to
-- @ListReusableDelegationSets@ that produced the current response.
listReusableDelegationSetsResponse_maxItems :: Lens.Lens' ListReusableDelegationSetsResponse Prelude.Text
listReusableDelegationSetsResponse_maxItems = Lens.lens (\ListReusableDelegationSetsResponse' {maxItems} -> maxItems) (\s@ListReusableDelegationSetsResponse' {} a -> s {maxItems = a} :: ListReusableDelegationSetsResponse)

instance
  Prelude.NFData
    ListReusableDelegationSetsResponse
  where
  rnf ListReusableDelegationSetsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf delegationSets
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf maxItems
