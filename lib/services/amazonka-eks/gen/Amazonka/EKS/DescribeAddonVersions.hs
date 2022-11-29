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
-- Module      : Amazonka.EKS.DescribeAddonVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Kubernetes versions that the add-on can be used with.
--
-- This operation returns paginated results.
module Amazonka.EKS.DescribeAddonVersions
  ( -- * Creating a Request
    DescribeAddonVersions (..),
    newDescribeAddonVersions,

    -- * Request Lenses
    describeAddonVersions_nextToken,
    describeAddonVersions_addonName,
    describeAddonVersions_maxResults,
    describeAddonVersions_kubernetesVersion,

    -- * Destructuring the Response
    DescribeAddonVersionsResponse (..),
    newDescribeAddonVersionsResponse,

    -- * Response Lenses
    describeAddonVersionsResponse_nextToken,
    describeAddonVersionsResponse_addons,
    describeAddonVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Kubernetes versions that the add-on can be used with.
    kubernetesVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'addonName', 'describeAddonVersions_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
--
-- 'maxResults', 'describeAddonVersions_maxResults' - The maximum number of results to return.
--
-- 'kubernetesVersion', 'describeAddonVersions_kubernetesVersion' - The Kubernetes versions that the add-on can be used with.
newDescribeAddonVersions ::
  DescribeAddonVersions
newDescribeAddonVersions =
  DescribeAddonVersions'
    { nextToken = Prelude.Nothing,
      addonName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      kubernetesVersion = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeAddonVersionsRequest@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeAddonVersions_nextToken :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe Prelude.Text)
describeAddonVersions_nextToken = Lens.lens (\DescribeAddonVersions' {nextToken} -> nextToken) (\s@DescribeAddonVersions' {} a -> s {nextToken = a} :: DescribeAddonVersions)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
describeAddonVersions_addonName :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe Prelude.Text)
describeAddonVersions_addonName = Lens.lens (\DescribeAddonVersions' {addonName} -> addonName) (\s@DescribeAddonVersions' {} a -> s {addonName = a} :: DescribeAddonVersions)

-- | The maximum number of results to return.
describeAddonVersions_maxResults :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe Prelude.Natural)
describeAddonVersions_maxResults = Lens.lens (\DescribeAddonVersions' {maxResults} -> maxResults) (\s@DescribeAddonVersions' {} a -> s {maxResults = a} :: DescribeAddonVersions)

-- | The Kubernetes versions that the add-on can be used with.
describeAddonVersions_kubernetesVersion :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe Prelude.Text)
describeAddonVersions_kubernetesVersion = Lens.lens (\DescribeAddonVersions' {kubernetesVersion} -> kubernetesVersion) (\s@DescribeAddonVersions' {} a -> s {kubernetesVersion = a} :: DescribeAddonVersions)

instance Core.AWSPager DescribeAddonVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAddonVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAddonVersionsResponse_addons
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAddonVersions_nextToken
          Lens..~ rs
          Lens.^? describeAddonVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAddonVersions where
  type
    AWSResponse DescribeAddonVersions =
      DescribeAddonVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddonVersionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "addons" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddonVersions where
  hashWithSalt _salt DescribeAddonVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` addonName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` kubernetesVersion

instance Prelude.NFData DescribeAddonVersions where
  rnf DescribeAddonVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf addonName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf kubernetesVersion

instance Core.ToHeaders DescribeAddonVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeAddonVersions where
  toPath = Prelude.const "/addons/supported-versions"

instance Core.ToQuery DescribeAddonVersions where
  toQuery DescribeAddonVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "addonName" Core.=: addonName,
        "maxResults" Core.=: maxResults,
        "kubernetesVersion" Core.=: kubernetesVersion
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of available versions with Kubernetes version compatibility.
    addons :: Prelude.Maybe [AddonInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAddonVersionsResponse
newDescribeAddonVersionsResponse pHttpStatus_ =
  DescribeAddonVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      addons = Prelude.Nothing,
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
describeAddonVersionsResponse_nextToken :: Lens.Lens' DescribeAddonVersionsResponse (Prelude.Maybe Prelude.Text)
describeAddonVersionsResponse_nextToken = Lens.lens (\DescribeAddonVersionsResponse' {nextToken} -> nextToken) (\s@DescribeAddonVersionsResponse' {} a -> s {nextToken = a} :: DescribeAddonVersionsResponse)

-- | The list of available versions with Kubernetes version compatibility.
describeAddonVersionsResponse_addons :: Lens.Lens' DescribeAddonVersionsResponse (Prelude.Maybe [AddonInfo])
describeAddonVersionsResponse_addons = Lens.lens (\DescribeAddonVersionsResponse' {addons} -> addons) (\s@DescribeAddonVersionsResponse' {} a -> s {addons = a} :: DescribeAddonVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAddonVersionsResponse_httpStatus :: Lens.Lens' DescribeAddonVersionsResponse Prelude.Int
describeAddonVersionsResponse_httpStatus = Lens.lens (\DescribeAddonVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAddonVersionsResponse' {} a -> s {httpStatus = a} :: DescribeAddonVersionsResponse)

instance Prelude.NFData DescribeAddonVersionsResponse where
  rnf DescribeAddonVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf addons
      `Prelude.seq` Prelude.rnf httpStatus
