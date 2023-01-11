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
-- Module      : Amazonka.SSM.DescribeInstancePatches
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the patches on the specified managed node
-- and their state relative to the patch baseline being used for the node.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeInstancePatches
  ( -- * Creating a Request
    DescribeInstancePatches (..),
    newDescribeInstancePatches,

    -- * Request Lenses
    describeInstancePatches_filters,
    describeInstancePatches_maxResults,
    describeInstancePatches_nextToken,
    describeInstancePatches_instanceId,

    -- * Destructuring the Response
    DescribeInstancePatchesResponse (..),
    newDescribeInstancePatchesResponse,

    -- * Response Lenses
    describeInstancePatchesResponse_nextToken,
    describeInstancePatchesResponse_patches,
    describeInstancePatchesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeInstancePatches' smart constructor.
data DescribeInstancePatches = DescribeInstancePatches'
  { -- | Each element in the array is a structure containing a key-value pair.
    --
    -- Supported keys for @DescribeInstancePatches@include the following:
    --
    -- -   __@Classification@__
    --
    --     Sample values: @Security@ | @SecurityUpdates@
    --
    -- -   __@KBId@__
    --
    --     Sample values: @KB4480056@ | @java-1.7.0-openjdk.x86_64@
    --
    -- -   __@Severity@__
    --
    --     Sample values: @Important@ | @Medium@ | @Low@
    --
    -- -   __@State@__
    --
    --     Sample values: @Installed@ | @InstalledOther@ |
    --     @InstalledPendingReboot@
    filters :: Prelude.Maybe [PatchOrchestratorFilter],
    -- | The maximum number of patches to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the managed node whose patch state information should be
    -- retrieved.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancePatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeInstancePatches_filters' - Each element in the array is a structure containing a key-value pair.
--
-- Supported keys for @DescribeInstancePatches@include the following:
--
-- -   __@Classification@__
--
--     Sample values: @Security@ | @SecurityUpdates@
--
-- -   __@KBId@__
--
--     Sample values: @KB4480056@ | @java-1.7.0-openjdk.x86_64@
--
-- -   __@Severity@__
--
--     Sample values: @Important@ | @Medium@ | @Low@
--
-- -   __@State@__
--
--     Sample values: @Installed@ | @InstalledOther@ |
--     @InstalledPendingReboot@
--
-- 'maxResults', 'describeInstancePatches_maxResults' - The maximum number of patches to return (per page).
--
-- 'nextToken', 'describeInstancePatches_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'instanceId', 'describeInstancePatches_instanceId' - The ID of the managed node whose patch state information should be
-- retrieved.
newDescribeInstancePatches ::
  -- | 'instanceId'
  Prelude.Text ->
  DescribeInstancePatches
newDescribeInstancePatches pInstanceId_ =
  DescribeInstancePatches'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Each element in the array is a structure containing a key-value pair.
--
-- Supported keys for @DescribeInstancePatches@include the following:
--
-- -   __@Classification@__
--
--     Sample values: @Security@ | @SecurityUpdates@
--
-- -   __@KBId@__
--
--     Sample values: @KB4480056@ | @java-1.7.0-openjdk.x86_64@
--
-- -   __@Severity@__
--
--     Sample values: @Important@ | @Medium@ | @Low@
--
-- -   __@State@__
--
--     Sample values: @Installed@ | @InstalledOther@ |
--     @InstalledPendingReboot@
describeInstancePatches_filters :: Lens.Lens' DescribeInstancePatches (Prelude.Maybe [PatchOrchestratorFilter])
describeInstancePatches_filters = Lens.lens (\DescribeInstancePatches' {filters} -> filters) (\s@DescribeInstancePatches' {} a -> s {filters = a} :: DescribeInstancePatches) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of patches to return (per page).
describeInstancePatches_maxResults :: Lens.Lens' DescribeInstancePatches (Prelude.Maybe Prelude.Natural)
describeInstancePatches_maxResults = Lens.lens (\DescribeInstancePatches' {maxResults} -> maxResults) (\s@DescribeInstancePatches' {} a -> s {maxResults = a} :: DescribeInstancePatches)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatches_nextToken :: Lens.Lens' DescribeInstancePatches (Prelude.Maybe Prelude.Text)
describeInstancePatches_nextToken = Lens.lens (\DescribeInstancePatches' {nextToken} -> nextToken) (\s@DescribeInstancePatches' {} a -> s {nextToken = a} :: DescribeInstancePatches)

-- | The ID of the managed node whose patch state information should be
-- retrieved.
describeInstancePatches_instanceId :: Lens.Lens' DescribeInstancePatches Prelude.Text
describeInstancePatches_instanceId = Lens.lens (\DescribeInstancePatches' {instanceId} -> instanceId) (\s@DescribeInstancePatches' {} a -> s {instanceId = a} :: DescribeInstancePatches)

instance Core.AWSPager DescribeInstancePatches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchesResponse_patches
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstancePatches_nextToken
          Lens..~ rs
          Lens.^? describeInstancePatchesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstancePatches where
  type
    AWSResponse DescribeInstancePatches =
      DescribeInstancePatchesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Patches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstancePatches where
  hashWithSalt _salt DescribeInstancePatches' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DescribeInstancePatches where
  rnf DescribeInstancePatches' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders DescribeInstancePatches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeInstancePatches" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeInstancePatches where
  toJSON DescribeInstancePatches' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath DescribeInstancePatches where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstancePatches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstancePatchesResponse' smart constructor.
data DescribeInstancePatchesResponse = DescribeInstancePatchesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each entry in the array is a structure containing:
    --
    -- -   Title (string)
    --
    -- -   KBId (string)
    --
    -- -   Classification (string)
    --
    -- -   Severity (string)
    --
    -- -   State (string, such as \"INSTALLED\" or \"FAILED\")
    --
    -- -   InstalledTime (DateTime)
    --
    -- -   InstalledBy (string)
    patches :: Prelude.Maybe [PatchComplianceData],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancePatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'patches', 'describeInstancePatchesResponse_patches' - Each entry in the array is a structure containing:
--
-- -   Title (string)
--
-- -   KBId (string)
--
-- -   Classification (string)
--
-- -   Severity (string)
--
-- -   State (string, such as \"INSTALLED\" or \"FAILED\")
--
-- -   InstalledTime (DateTime)
--
-- -   InstalledBy (string)
--
-- 'httpStatus', 'describeInstancePatchesResponse_httpStatus' - The response's http status code.
newDescribeInstancePatchesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstancePatchesResponse
newDescribeInstancePatchesResponse pHttpStatus_ =
  DescribeInstancePatchesResponse'
    { nextToken =
        Prelude.Nothing,
      patches = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstancePatchesResponse_nextToken :: Lens.Lens' DescribeInstancePatchesResponse (Prelude.Maybe Prelude.Text)
describeInstancePatchesResponse_nextToken = Lens.lens (\DescribeInstancePatchesResponse' {nextToken} -> nextToken) (\s@DescribeInstancePatchesResponse' {} a -> s {nextToken = a} :: DescribeInstancePatchesResponse)

-- | Each entry in the array is a structure containing:
--
-- -   Title (string)
--
-- -   KBId (string)
--
-- -   Classification (string)
--
-- -   Severity (string)
--
-- -   State (string, such as \"INSTALLED\" or \"FAILED\")
--
-- -   InstalledTime (DateTime)
--
-- -   InstalledBy (string)
describeInstancePatchesResponse_patches :: Lens.Lens' DescribeInstancePatchesResponse (Prelude.Maybe [PatchComplianceData])
describeInstancePatchesResponse_patches = Lens.lens (\DescribeInstancePatchesResponse' {patches} -> patches) (\s@DescribeInstancePatchesResponse' {} a -> s {patches = a} :: DescribeInstancePatchesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstancePatchesResponse_httpStatus :: Lens.Lens' DescribeInstancePatchesResponse Prelude.Int
describeInstancePatchesResponse_httpStatus = Lens.lens (\DescribeInstancePatchesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchesResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchesResponse)

instance
  Prelude.NFData
    DescribeInstancePatchesResponse
  where
  rnf DescribeInstancePatchesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf patches
      `Prelude.seq` Prelude.rnf httpStatus
