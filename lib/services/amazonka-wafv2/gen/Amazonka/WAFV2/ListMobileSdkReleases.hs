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
-- Module      : Amazonka.WAFV2.ListMobileSdkReleases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the available releases for the mobile SDK and the
-- specified device platform.
--
-- The mobile SDK is not generally available. Customers who have access to
-- the mobile SDK can use it to establish and manage WAF tokens for use in
-- HTTP(S) requests from a mobile device to WAF. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
module Amazonka.WAFV2.ListMobileSdkReleases
  ( -- * Creating a Request
    ListMobileSdkReleases (..),
    newListMobileSdkReleases,

    -- * Request Lenses
    listMobileSdkReleases_limit,
    listMobileSdkReleases_nextMarker,
    listMobileSdkReleases_platform,

    -- * Destructuring the Response
    ListMobileSdkReleasesResponse (..),
    newListMobileSdkReleasesResponse,

    -- * Response Lenses
    listMobileSdkReleasesResponse_releaseSummaries,
    listMobileSdkReleasesResponse_nextMarker,
    listMobileSdkReleasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListMobileSdkReleases' smart constructor.
data ListMobileSdkReleases = ListMobileSdkReleases'
  { -- | The maximum number of objects that you want WAF to return for this
    -- request. If more objects are available, in the response, WAF provides a
    -- @NextMarker@ value that you can use in a subsequent call to get the next
    -- batch of objects.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The device platform to retrieve the list for.
    platform :: Platform
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMobileSdkReleases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listMobileSdkReleases_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listMobileSdkReleases_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'platform', 'listMobileSdkReleases_platform' - The device platform to retrieve the list for.
newListMobileSdkReleases ::
  -- | 'platform'
  Platform ->
  ListMobileSdkReleases
newListMobileSdkReleases pPlatform_ =
  ListMobileSdkReleases'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listMobileSdkReleases_limit :: Lens.Lens' ListMobileSdkReleases (Prelude.Maybe Prelude.Natural)
listMobileSdkReleases_limit = Lens.lens (\ListMobileSdkReleases' {limit} -> limit) (\s@ListMobileSdkReleases' {} a -> s {limit = a} :: ListMobileSdkReleases)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listMobileSdkReleases_nextMarker :: Lens.Lens' ListMobileSdkReleases (Prelude.Maybe Prelude.Text)
listMobileSdkReleases_nextMarker = Lens.lens (\ListMobileSdkReleases' {nextMarker} -> nextMarker) (\s@ListMobileSdkReleases' {} a -> s {nextMarker = a} :: ListMobileSdkReleases)

-- | The device platform to retrieve the list for.
listMobileSdkReleases_platform :: Lens.Lens' ListMobileSdkReleases Platform
listMobileSdkReleases_platform = Lens.lens (\ListMobileSdkReleases' {platform} -> platform) (\s@ListMobileSdkReleases' {} a -> s {platform = a} :: ListMobileSdkReleases)

instance Core.AWSRequest ListMobileSdkReleases where
  type
    AWSResponse ListMobileSdkReleases =
      ListMobileSdkReleasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMobileSdkReleasesResponse'
            Prelude.<$> ( x Data..?> "ReleaseSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMobileSdkReleases where
  hashWithSalt _salt ListMobileSdkReleases' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` platform

instance Prelude.NFData ListMobileSdkReleases where
  rnf ListMobileSdkReleases' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf platform

instance Data.ToHeaders ListMobileSdkReleases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.ListMobileSdkReleases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMobileSdkReleases where
  toJSON ListMobileSdkReleases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker,
            Prelude.Just ("Platform" Data..= platform)
          ]
      )

instance Data.ToPath ListMobileSdkReleases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMobileSdkReleases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMobileSdkReleasesResponse' smart constructor.
data ListMobileSdkReleasesResponse = ListMobileSdkReleasesResponse'
  { -- | High level information for the available SDK releases.
    releaseSummaries :: Prelude.Maybe [ReleaseSummary],
    -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMobileSdkReleasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseSummaries', 'listMobileSdkReleasesResponse_releaseSummaries' - High level information for the available SDK releases.
--
-- 'nextMarker', 'listMobileSdkReleasesResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'httpStatus', 'listMobileSdkReleasesResponse_httpStatus' - The response's http status code.
newListMobileSdkReleasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMobileSdkReleasesResponse
newListMobileSdkReleasesResponse pHttpStatus_ =
  ListMobileSdkReleasesResponse'
    { releaseSummaries =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | High level information for the available SDK releases.
listMobileSdkReleasesResponse_releaseSummaries :: Lens.Lens' ListMobileSdkReleasesResponse (Prelude.Maybe [ReleaseSummary])
listMobileSdkReleasesResponse_releaseSummaries = Lens.lens (\ListMobileSdkReleasesResponse' {releaseSummaries} -> releaseSummaries) (\s@ListMobileSdkReleasesResponse' {} a -> s {releaseSummaries = a} :: ListMobileSdkReleasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listMobileSdkReleasesResponse_nextMarker :: Lens.Lens' ListMobileSdkReleasesResponse (Prelude.Maybe Prelude.Text)
listMobileSdkReleasesResponse_nextMarker = Lens.lens (\ListMobileSdkReleasesResponse' {nextMarker} -> nextMarker) (\s@ListMobileSdkReleasesResponse' {} a -> s {nextMarker = a} :: ListMobileSdkReleasesResponse)

-- | The response's http status code.
listMobileSdkReleasesResponse_httpStatus :: Lens.Lens' ListMobileSdkReleasesResponse Prelude.Int
listMobileSdkReleasesResponse_httpStatus = Lens.lens (\ListMobileSdkReleasesResponse' {httpStatus} -> httpStatus) (\s@ListMobileSdkReleasesResponse' {} a -> s {httpStatus = a} :: ListMobileSdkReleasesResponse)

instance Prelude.NFData ListMobileSdkReleasesResponse where
  rnf ListMobileSdkReleasesResponse' {..} =
    Prelude.rnf releaseSummaries
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
