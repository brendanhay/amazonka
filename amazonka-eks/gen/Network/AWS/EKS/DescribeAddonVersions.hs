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
-- Module      : Network.AWS.EKS.DescribeAddonVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Kubernetes versions that the add-on can be used with.
--
-- This operation returns paginated results.
module Network.AWS.EKS.DescribeAddonVersions
  ( -- * Creating a Request
    DescribeAddonVersions (..),
    newDescribeAddonVersions,

    -- * Request Lenses
    describeAddonVersions_nextToken,
    describeAddonVersions_maxResults,
    describeAddonVersions_kubernetesVersion,
    describeAddonVersions_addonName,

    -- * Destructuring the Response
    DescribeAddonVersionsResponse (..),
    newDescribeAddonVersionsResponse,

    -- * Response Lenses
    describeAddonVersionsResponse_nextToken,
    describeAddonVersionsResponse_addons,
    describeAddonVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAddonVersions' smart constructor.
data DescribeAddonVersions = DescribeAddonVersions'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeAddonVersionsRequest@ where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Kubernetes versions that the add-on can be used with.
    kubernetesVersion :: Core.Maybe Core.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddonVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAddonVersions_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeAddonVersionsRequest@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'describeAddonVersions_maxResults' - The maximum number of results to return.
--
-- 'kubernetesVersion', 'describeAddonVersions_kubernetesVersion' - The Kubernetes versions that the add-on can be used with.
--
-- 'addonName', 'describeAddonVersions_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
newDescribeAddonVersions ::
  DescribeAddonVersions
newDescribeAddonVersions =
  DescribeAddonVersions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      kubernetesVersion = Core.Nothing,
      addonName = Core.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeAddonVersionsRequest@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeAddonVersions_nextToken :: Lens.Lens' DescribeAddonVersions (Core.Maybe Core.Text)
describeAddonVersions_nextToken = Lens.lens (\DescribeAddonVersions' {nextToken} -> nextToken) (\s@DescribeAddonVersions' {} a -> s {nextToken = a} :: DescribeAddonVersions)

-- | The maximum number of results to return.
describeAddonVersions_maxResults :: Lens.Lens' DescribeAddonVersions (Core.Maybe Core.Natural)
describeAddonVersions_maxResults = Lens.lens (\DescribeAddonVersions' {maxResults} -> maxResults) (\s@DescribeAddonVersions' {} a -> s {maxResults = a} :: DescribeAddonVersions)

-- | The Kubernetes versions that the add-on can be used with.
describeAddonVersions_kubernetesVersion :: Lens.Lens' DescribeAddonVersions (Core.Maybe Core.Text)
describeAddonVersions_kubernetesVersion = Lens.lens (\DescribeAddonVersions' {kubernetesVersion} -> kubernetesVersion) (\s@DescribeAddonVersions' {} a -> s {kubernetesVersion = a} :: DescribeAddonVersions)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
describeAddonVersions_addonName :: Lens.Lens' DescribeAddonVersions (Core.Maybe Core.Text)
describeAddonVersions_addonName = Lens.lens (\DescribeAddonVersions' {addonName} -> addonName) (\s@DescribeAddonVersions' {} a -> s {addonName = a} :: DescribeAddonVersions)

instance Core.AWSPager DescribeAddonVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAddonVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAddonVersionsResponse_addons
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAddonVersions_nextToken
          Lens..~ rs
          Lens.^? describeAddonVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeAddonVersions where
  type
    AWSResponse DescribeAddonVersions =
      DescribeAddonVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddonVersionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "addons" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAddonVersions

instance Core.NFData DescribeAddonVersions

instance Core.ToHeaders DescribeAddonVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeAddonVersions where
  toPath = Core.const "/addons/supported-versions"

instance Core.ToQuery DescribeAddonVersions where
  toQuery DescribeAddonVersions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "kubernetesVersion" Core.=: kubernetesVersion,
        "addonName" Core.=: addonName
      ]

-- | /See:/ 'newDescribeAddonVersionsResponse' smart constructor.
data DescribeAddonVersionsResponse = DescribeAddonVersionsResponse'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeAddonVersionsResponse@ where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of available versions with Kubernetes version compatibility.
    addons :: Core.Maybe [AddonInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddonVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAddonVersionsResponse_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeAddonVersionsResponse@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'addons', 'describeAddonVersionsResponse_addons' - The list of available versions with Kubernetes version compatibility.
--
-- 'httpStatus', 'describeAddonVersionsResponse_httpStatus' - The response's http status code.
newDescribeAddonVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAddonVersionsResponse
newDescribeAddonVersionsResponse pHttpStatus_ =
  DescribeAddonVersionsResponse'
    { nextToken =
        Core.Nothing,
      addons = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeAddonVersionsResponse@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeAddonVersionsResponse_nextToken :: Lens.Lens' DescribeAddonVersionsResponse (Core.Maybe Core.Text)
describeAddonVersionsResponse_nextToken = Lens.lens (\DescribeAddonVersionsResponse' {nextToken} -> nextToken) (\s@DescribeAddonVersionsResponse' {} a -> s {nextToken = a} :: DescribeAddonVersionsResponse)

-- | The list of available versions with Kubernetes version compatibility.
describeAddonVersionsResponse_addons :: Lens.Lens' DescribeAddonVersionsResponse (Core.Maybe [AddonInfo])
describeAddonVersionsResponse_addons = Lens.lens (\DescribeAddonVersionsResponse' {addons} -> addons) (\s@DescribeAddonVersionsResponse' {} a -> s {addons = a} :: DescribeAddonVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAddonVersionsResponse_httpStatus :: Lens.Lens' DescribeAddonVersionsResponse Core.Int
describeAddonVersionsResponse_httpStatus = Lens.lens (\DescribeAddonVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAddonVersionsResponse' {} a -> s {httpStatus = a} :: DescribeAddonVersionsResponse)

instance Core.NFData DescribeAddonVersionsResponse
