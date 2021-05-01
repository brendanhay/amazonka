{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List streaming distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListStreamingDistributions
  ( -- * Creating a Request
    ListStreamingDistributions (..),
    newListStreamingDistributions,

    -- * Request Lenses
    listStreamingDistributions_maxItems,
    listStreamingDistributions_marker,

    -- * Destructuring the Response
    ListStreamingDistributionsResponse (..),
    newListStreamingDistributionsResponse,

    -- * Response Lenses
    listStreamingDistributionsResponse_httpStatus,
    listStreamingDistributionsResponse_streamingDistributionList,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list your streaming distributions.
--
-- /See:/ 'newListStreamingDistributions' smart constructor.
data ListStreamingDistributions = ListStreamingDistributions'
  { -- | The value that you provided for the @MaxItems@ request parameter.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The value that you provided for the @Marker@ request parameter.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingDistributions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listStreamingDistributions_maxItems' - The value that you provided for the @MaxItems@ request parameter.
--
-- 'marker', 'listStreamingDistributions_marker' - The value that you provided for the @Marker@ request parameter.
newListStreamingDistributions ::
  ListStreamingDistributions
newListStreamingDistributions =
  ListStreamingDistributions'
    { maxItems =
        Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The value that you provided for the @MaxItems@ request parameter.
listStreamingDistributions_maxItems :: Lens.Lens' ListStreamingDistributions (Prelude.Maybe Prelude.Text)
listStreamingDistributions_maxItems = Lens.lens (\ListStreamingDistributions' {maxItems} -> maxItems) (\s@ListStreamingDistributions' {} a -> s {maxItems = a} :: ListStreamingDistributions)

-- | The value that you provided for the @Marker@ request parameter.
listStreamingDistributions_marker :: Lens.Lens' ListStreamingDistributions (Prelude.Maybe Prelude.Text)
listStreamingDistributions_marker = Lens.lens (\ListStreamingDistributions' {marker} -> marker) (\s@ListStreamingDistributions' {} a -> s {marker = a} :: ListStreamingDistributions)

instance Pager.AWSPager ListStreamingDistributions where
  page rq rs
    | Pager.stop
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
          Lens.& listStreamingDistributions_marker
          Lens..~ rs
          Lens.^? listStreamingDistributionsResponse_streamingDistributionList
            Prelude.. streamingDistributionList_nextMarker
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListStreamingDistributions
  where
  type
    Rs ListStreamingDistributions =
      ListStreamingDistributionsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListStreamingDistributionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.parseXML x)
      )

instance Prelude.Hashable ListStreamingDistributions

instance Prelude.NFData ListStreamingDistributions

instance Prelude.ToHeaders ListStreamingDistributions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListStreamingDistributions where
  toPath =
    Prelude.const "/2020-05-31/streaming-distribution"

instance Prelude.ToQuery ListStreamingDistributions where
  toQuery ListStreamingDistributions' {..} =
    Prelude.mconcat
      [ "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
