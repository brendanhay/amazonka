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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the versions for an add-on. Information such as the Kubernetes
-- versions that you can use the add-on with, the @owner@, @publisher@, and
-- the @type@ of the add-on are returned.
--
-- This operation returns paginated results.
module Amazonka.EKS.DescribeAddonVersions
  ( -- * Creating a Request
    DescribeAddonVersions (..),
    newDescribeAddonVersions,

    -- * Request Lenses
    describeAddonVersions_addonName,
    describeAddonVersions_kubernetesVersion,
    describeAddonVersions_maxResults,
    describeAddonVersions_nextToken,
    describeAddonVersions_owners,
    describeAddonVersions_publishers,
    describeAddonVersions_types,

    -- * Destructuring the Response
    DescribeAddonVersionsResponse (..),
    newDescribeAddonVersionsResponse,

    -- * Response Lenses
    describeAddonVersionsResponse_addons,
    describeAddonVersionsResponse_nextToken,
    describeAddonVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAddonVersions' smart constructor.
data DescribeAddonVersions = DescribeAddonVersions'
  { -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Prelude.Maybe Prelude.Text,
    -- | The Kubernetes versions that you can use the add-on with.
    kubernetesVersion :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeAddonVersionsRequest@ where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The owner of the add-on. For valid @owners@, don\'t specify a value for
    -- this property.
    owners :: Prelude.Maybe [Prelude.Text],
    -- | The publisher of the add-on. For valid @publishers@, don\'t specify a
    -- value for this property.
    publishers :: Prelude.Maybe [Prelude.Text],
    -- | The type of the add-on. For valid @types@, don\'t specify a value for
    -- this property.
    types :: Prelude.Maybe [Prelude.Text]
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
-- 'addonName', 'describeAddonVersions_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
--
-- 'kubernetesVersion', 'describeAddonVersions_kubernetesVersion' - The Kubernetes versions that you can use the add-on with.
--
-- 'maxResults', 'describeAddonVersions_maxResults' - The maximum number of results to return.
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
-- 'owners', 'describeAddonVersions_owners' - The owner of the add-on. For valid @owners@, don\'t specify a value for
-- this property.
--
-- 'publishers', 'describeAddonVersions_publishers' - The publisher of the add-on. For valid @publishers@, don\'t specify a
-- value for this property.
--
-- 'types', 'describeAddonVersions_types' - The type of the add-on. For valid @types@, don\'t specify a value for
-- this property.
newDescribeAddonVersions ::
  DescribeAddonVersions
newDescribeAddonVersions =
  DescribeAddonVersions'
    { addonName = Prelude.Nothing,
      kubernetesVersion = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      owners = Prelude.Nothing,
      publishers = Prelude.Nothing,
      types = Prelude.Nothing
    }

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
describeAddonVersions_addonName :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe Prelude.Text)
describeAddonVersions_addonName = Lens.lens (\DescribeAddonVersions' {addonName} -> addonName) (\s@DescribeAddonVersions' {} a -> s {addonName = a} :: DescribeAddonVersions)

-- | The Kubernetes versions that you can use the add-on with.
describeAddonVersions_kubernetesVersion :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe Prelude.Text)
describeAddonVersions_kubernetesVersion = Lens.lens (\DescribeAddonVersions' {kubernetesVersion} -> kubernetesVersion) (\s@DescribeAddonVersions' {} a -> s {kubernetesVersion = a} :: DescribeAddonVersions)

-- | The maximum number of results to return.
describeAddonVersions_maxResults :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe Prelude.Natural)
describeAddonVersions_maxResults = Lens.lens (\DescribeAddonVersions' {maxResults} -> maxResults) (\s@DescribeAddonVersions' {} a -> s {maxResults = a} :: DescribeAddonVersions)

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

-- | The owner of the add-on. For valid @owners@, don\'t specify a value for
-- this property.
describeAddonVersions_owners :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe [Prelude.Text])
describeAddonVersions_owners = Lens.lens (\DescribeAddonVersions' {owners} -> owners) (\s@DescribeAddonVersions' {} a -> s {owners = a} :: DescribeAddonVersions) Prelude.. Lens.mapping Lens.coerced

-- | The publisher of the add-on. For valid @publishers@, don\'t specify a
-- value for this property.
describeAddonVersions_publishers :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe [Prelude.Text])
describeAddonVersions_publishers = Lens.lens (\DescribeAddonVersions' {publishers} -> publishers) (\s@DescribeAddonVersions' {} a -> s {publishers = a} :: DescribeAddonVersions) Prelude.. Lens.mapping Lens.coerced

-- | The type of the add-on. For valid @types@, don\'t specify a value for
-- this property.
describeAddonVersions_types :: Lens.Lens' DescribeAddonVersions (Prelude.Maybe [Prelude.Text])
describeAddonVersions_types = Lens.lens (\DescribeAddonVersions' {types} -> types) (\s@DescribeAddonVersions' {} a -> s {types = a} :: DescribeAddonVersions) Prelude.. Lens.mapping Lens.coerced

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> (x Data..?> "addons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddonVersions where
  hashWithSalt _salt DescribeAddonVersions' {..} =
    _salt
      `Prelude.hashWithSalt` addonName
      `Prelude.hashWithSalt` kubernetesVersion
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` owners
      `Prelude.hashWithSalt` publishers
      `Prelude.hashWithSalt` types

instance Prelude.NFData DescribeAddonVersions where
  rnf DescribeAddonVersions' {..} =
    Prelude.rnf addonName
      `Prelude.seq` Prelude.rnf kubernetesVersion
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf owners
      `Prelude.seq` Prelude.rnf publishers
      `Prelude.seq` Prelude.rnf types

instance Data.ToHeaders DescribeAddonVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAddonVersions where
  toPath = Prelude.const "/addons/supported-versions"

instance Data.ToQuery DescribeAddonVersions where
  toQuery DescribeAddonVersions' {..} =
    Prelude.mconcat
      [ "addonName" Data.=: addonName,
        "kubernetesVersion" Data.=: kubernetesVersion,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "owners"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> owners),
        "publishers"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> publishers),
        "types"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> types)
      ]

-- | /See:/ 'newDescribeAddonVersionsResponse' smart constructor.
data DescribeAddonVersionsResponse = DescribeAddonVersionsResponse'
  { -- | The list of available versions with Kubernetes version compatibility and
    -- other properties.
    addons :: Prelude.Maybe [AddonInfo],
    -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeAddonVersionsResponse@ where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'addons', 'describeAddonVersionsResponse_addons' - The list of available versions with Kubernetes version compatibility and
-- other properties.
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
-- 'httpStatus', 'describeAddonVersionsResponse_httpStatus' - The response's http status code.
newDescribeAddonVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAddonVersionsResponse
newDescribeAddonVersionsResponse pHttpStatus_ =
  DescribeAddonVersionsResponse'
    { addons =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of available versions with Kubernetes version compatibility and
-- other properties.
describeAddonVersionsResponse_addons :: Lens.Lens' DescribeAddonVersionsResponse (Prelude.Maybe [AddonInfo])
describeAddonVersionsResponse_addons = Lens.lens (\DescribeAddonVersionsResponse' {addons} -> addons) (\s@DescribeAddonVersionsResponse' {} a -> s {addons = a} :: DescribeAddonVersionsResponse) Prelude.. Lens.mapping Lens.coerced

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

-- | The response's http status code.
describeAddonVersionsResponse_httpStatus :: Lens.Lens' DescribeAddonVersionsResponse Prelude.Int
describeAddonVersionsResponse_httpStatus = Lens.lens (\DescribeAddonVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAddonVersionsResponse' {} a -> s {httpStatus = a} :: DescribeAddonVersionsResponse)

instance Prelude.NFData DescribeAddonVersionsResponse where
  rnf DescribeAddonVersionsResponse' {..} =
    Prelude.rnf addons
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
