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
-- Module      : Amazonka.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List streaming distributions.
--
-- This operation returns paginated results.
module Amazonka.CloudFront.ListStreamingDistributions
  ( -- * Creating a Request
    ListStreamingDistributions (..),
    newListStreamingDistributions,

    -- * Request Lenses
    listStreamingDistributions_marker,
    listStreamingDistributions_maxItems,

    -- * Destructuring the Response
    ListStreamingDistributionsResponse (..),
    newListStreamingDistributionsResponse,

    -- * Response Lenses
    listStreamingDistributionsResponse_httpStatus,
    listStreamingDistributionsResponse_streamingDistributionList,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to list your streaming distributions.
--
-- /See:/ 'newListStreamingDistributions' smart constructor.
data ListStreamingDistributions = ListStreamingDistributions'
  { -- | The value that you provided for the @Marker@ request parameter.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The value that you provided for the @MaxItems@ request parameter.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingDistributions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listStreamingDistributions_marker' - The value that you provided for the @Marker@ request parameter.
--
-- 'maxItems', 'listStreamingDistributions_maxItems' - The value that you provided for the @MaxItems@ request parameter.
newListStreamingDistributions ::
  ListStreamingDistributions
newListStreamingDistributions =
  ListStreamingDistributions'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | The value that you provided for the @Marker@ request parameter.
listStreamingDistributions_marker :: Lens.Lens' ListStreamingDistributions (Prelude.Maybe Prelude.Text)
listStreamingDistributions_marker = Lens.lens (\ListStreamingDistributions' {marker} -> marker) (\s@ListStreamingDistributions' {} a -> s {marker = a} :: ListStreamingDistributions)

-- | The value that you provided for the @MaxItems@ request parameter.
listStreamingDistributions_maxItems :: Lens.Lens' ListStreamingDistributions (Prelude.Maybe Prelude.Text)
listStreamingDistributions_maxItems = Lens.lens (\ListStreamingDistributions' {maxItems} -> maxItems) (\s@ListStreamingDistributions' {} a -> s {maxItems = a} :: ListStreamingDistributions)

instance Core.AWSPager ListStreamingDistributions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^. listStreamingDistributionsResponse_streamingDistributionList
              Prelude.. streamingDistributionList_isTruncated
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listStreamingDistributionsResponse_streamingDistributionList
              Prelude.. streamingDistributionList_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStreamingDistributions_marker
          Lens..~ rs
          Lens.^? listStreamingDistributionsResponse_streamingDistributionList
            Prelude.. streamingDistributionList_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListStreamingDistributions where
  type
    AWSResponse ListStreamingDistributions =
      ListStreamingDistributionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListStreamingDistributionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.parseXML x)
      )

instance Prelude.Hashable ListStreamingDistributions where
  hashWithSalt _salt ListStreamingDistributions' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListStreamingDistributions where
  rnf ListStreamingDistributions' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Core.ToHeaders ListStreamingDistributions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListStreamingDistributions where
  toPath =
    Prelude.const "/2020-05-31/streaming-distribution"

instance Core.ToQuery ListStreamingDistributions where
  toQuery ListStreamingDistributions' {..} =
    Prelude.mconcat
      [ "Marker" Core.=: marker,
        "MaxItems" Core.=: maxItems
      ]

-- | The returned result of the corresponding request.
--
-- /See:/ 'newListStreamingDistributionsResponse' smart constructor.
data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @StreamingDistributionList@ type.
    streamingDistributionList :: StreamingDistributionList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingDistributionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listStreamingDistributionsResponse_httpStatus' - The response's http status code.
--
-- 'streamingDistributionList', 'listStreamingDistributionsResponse_streamingDistributionList' - The @StreamingDistributionList@ type.
newListStreamingDistributionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'streamingDistributionList'
  StreamingDistributionList ->
  ListStreamingDistributionsResponse
newListStreamingDistributionsResponse
  pHttpStatus_
  pStreamingDistributionList_ =
    ListStreamingDistributionsResponse'
      { httpStatus =
          pHttpStatus_,
        streamingDistributionList =
          pStreamingDistributionList_
      }

-- | The response's http status code.
listStreamingDistributionsResponse_httpStatus :: Lens.Lens' ListStreamingDistributionsResponse Prelude.Int
listStreamingDistributionsResponse_httpStatus = Lens.lens (\ListStreamingDistributionsResponse' {httpStatus} -> httpStatus) (\s@ListStreamingDistributionsResponse' {} a -> s {httpStatus = a} :: ListStreamingDistributionsResponse)

-- | The @StreamingDistributionList@ type.
listStreamingDistributionsResponse_streamingDistributionList :: Lens.Lens' ListStreamingDistributionsResponse StreamingDistributionList
listStreamingDistributionsResponse_streamingDistributionList = Lens.lens (\ListStreamingDistributionsResponse' {streamingDistributionList} -> streamingDistributionList) (\s@ListStreamingDistributionsResponse' {} a -> s {streamingDistributionList = a} :: ListStreamingDistributionsResponse)

instance
  Prelude.NFData
    ListStreamingDistributionsResponse
  where
  rnf ListStreamingDistributionsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf streamingDistributionList
