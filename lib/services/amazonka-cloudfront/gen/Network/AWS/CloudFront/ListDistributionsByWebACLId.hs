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
-- Module      : Network.AWS.CloudFront.ListDistributionsByWebACLId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the distributions that are associated with a specified WAF web ACL.
module Network.AWS.CloudFront.ListDistributionsByWebACLId
  ( -- * Creating a Request
    ListDistributionsByWebACLId (..),
    newListDistributionsByWebACLId,

    -- * Request Lenses
    listDistributionsByWebACLId_marker,
    listDistributionsByWebACLId_maxItems,
    listDistributionsByWebACLId_webACLId,

    -- * Destructuring the Response
    ListDistributionsByWebACLIdResponse (..),
    newListDistributionsByWebACLIdResponse,

    -- * Response Lenses
    listDistributionsByWebACLIdResponse_distributionList,
    listDistributionsByWebACLIdResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list distributions that are associated with a specified
-- WAF web ACL.
--
-- /See:/ 'newListDistributionsByWebACLId' smart constructor.
data ListDistributionsByWebACLId = ListDistributionsByWebACLId'
  { -- | Use @Marker@ and @MaxItems@ to control pagination of results. If you
    -- have more than @MaxItems@ distributions that satisfy the request, the
    -- response includes a @NextMarker@ element. To get the next page of
    -- results, submit another request. For the value of @Marker@, specify the
    -- value of @NextMarker@ from the last response. (For the first request,
    -- omit @Marker@.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of distributions that you want CloudFront to return
    -- in the response body. The maximum and default values are both 100.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The ID of the WAF web ACL that you want to list the associated
    -- distributions. If you specify \"null\" for the ID, the request returns a
    -- list of the distributions that aren\'t associated with a web ACL.
    webACLId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByWebACLId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listDistributionsByWebACLId_marker' - Use @Marker@ and @MaxItems@ to control pagination of results. If you
-- have more than @MaxItems@ distributions that satisfy the request, the
-- response includes a @NextMarker@ element. To get the next page of
-- results, submit another request. For the value of @Marker@, specify the
-- value of @NextMarker@ from the last response. (For the first request,
-- omit @Marker@.)
--
-- 'maxItems', 'listDistributionsByWebACLId_maxItems' - The maximum number of distributions that you want CloudFront to return
-- in the response body. The maximum and default values are both 100.
--
-- 'webACLId', 'listDistributionsByWebACLId_webACLId' - The ID of the WAF web ACL that you want to list the associated
-- distributions. If you specify \"null\" for the ID, the request returns a
-- list of the distributions that aren\'t associated with a web ACL.
newListDistributionsByWebACLId ::
  -- | 'webACLId'
  Prelude.Text ->
  ListDistributionsByWebACLId
newListDistributionsByWebACLId pWebACLId_ =
  ListDistributionsByWebACLId'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      webACLId = pWebACLId_
    }

-- | Use @Marker@ and @MaxItems@ to control pagination of results. If you
-- have more than @MaxItems@ distributions that satisfy the request, the
-- response includes a @NextMarker@ element. To get the next page of
-- results, submit another request. For the value of @Marker@, specify the
-- value of @NextMarker@ from the last response. (For the first request,
-- omit @Marker@.)
listDistributionsByWebACLId_marker :: Lens.Lens' ListDistributionsByWebACLId (Prelude.Maybe Prelude.Text)
listDistributionsByWebACLId_marker = Lens.lens (\ListDistributionsByWebACLId' {marker} -> marker) (\s@ListDistributionsByWebACLId' {} a -> s {marker = a} :: ListDistributionsByWebACLId)

-- | The maximum number of distributions that you want CloudFront to return
-- in the response body. The maximum and default values are both 100.
listDistributionsByWebACLId_maxItems :: Lens.Lens' ListDistributionsByWebACLId (Prelude.Maybe Prelude.Text)
listDistributionsByWebACLId_maxItems = Lens.lens (\ListDistributionsByWebACLId' {maxItems} -> maxItems) (\s@ListDistributionsByWebACLId' {} a -> s {maxItems = a} :: ListDistributionsByWebACLId)

-- | The ID of the WAF web ACL that you want to list the associated
-- distributions. If you specify \"null\" for the ID, the request returns a
-- list of the distributions that aren\'t associated with a web ACL.
listDistributionsByWebACLId_webACLId :: Lens.Lens' ListDistributionsByWebACLId Prelude.Text
listDistributionsByWebACLId_webACLId = Lens.lens (\ListDistributionsByWebACLId' {webACLId} -> webACLId) (\s@ListDistributionsByWebACLId' {} a -> s {webACLId = a} :: ListDistributionsByWebACLId)

instance Core.AWSRequest ListDistributionsByWebACLId where
  type
    AWSResponse ListDistributionsByWebACLId =
      ListDistributionsByWebACLIdResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByWebACLIdResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDistributionsByWebACLId

instance Prelude.NFData ListDistributionsByWebACLId

instance Core.ToHeaders ListDistributionsByWebACLId where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDistributionsByWebACLId where
  toPath ListDistributionsByWebACLId' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributionsByWebACLId/",
        Core.toBS webACLId
      ]

instance Core.ToQuery ListDistributionsByWebACLId where
  toQuery ListDistributionsByWebACLId' {..} =
    Prelude.mconcat
      [ "Marker" Core.=: marker,
        "MaxItems" Core.=: maxItems
      ]

-- | The response to a request to list the distributions that are associated
-- with a specified WAF web ACL.
--
-- /See:/ 'newListDistributionsByWebACLIdResponse' smart constructor.
data ListDistributionsByWebACLIdResponse = ListDistributionsByWebACLIdResponse'
  { -- | The @DistributionList@ type.
    distributionList :: Prelude.Maybe DistributionList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByWebACLIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionList', 'listDistributionsByWebACLIdResponse_distributionList' - The @DistributionList@ type.
--
-- 'httpStatus', 'listDistributionsByWebACLIdResponse_httpStatus' - The response's http status code.
newListDistributionsByWebACLIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionsByWebACLIdResponse
newListDistributionsByWebACLIdResponse pHttpStatus_ =
  ListDistributionsByWebACLIdResponse'
    { distributionList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DistributionList@ type.
listDistributionsByWebACLIdResponse_distributionList :: Lens.Lens' ListDistributionsByWebACLIdResponse (Prelude.Maybe DistributionList)
listDistributionsByWebACLIdResponse_distributionList = Lens.lens (\ListDistributionsByWebACLIdResponse' {distributionList} -> distributionList) (\s@ListDistributionsByWebACLIdResponse' {} a -> s {distributionList = a} :: ListDistributionsByWebACLIdResponse)

-- | The response's http status code.
listDistributionsByWebACLIdResponse_httpStatus :: Lens.Lens' ListDistributionsByWebACLIdResponse Prelude.Int
listDistributionsByWebACLIdResponse_httpStatus = Lens.lens (\ListDistributionsByWebACLIdResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByWebACLIdResponse' {} a -> s {httpStatus = a} :: ListDistributionsByWebACLIdResponse)

instance
  Prelude.NFData
    ListDistributionsByWebACLIdResponse
