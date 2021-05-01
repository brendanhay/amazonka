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
-- Module      : Network.AWS.SSM.DescribeInstancePatches
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the patches on the specified instance and
-- their state relative to the patch baseline being used for the instance.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatches
  ( -- * Creating a Request
    DescribeInstancePatches (..),
    newDescribeInstancePatches,

    -- * Request Lenses
    describeInstancePatches_nextToken,
    describeInstancePatches_maxResults,
    describeInstancePatches_filters,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInstancePatches' smart constructor.
data DescribeInstancePatches = DescribeInstancePatches'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An array of structures. Each entry in the array is a structure
    -- containing a Key, Value combination. Valid values for Key are
    -- @Classification@ | @KBId@ | @Severity@ | @State@.
    filters :: Prelude.Maybe [PatchOrchestratorFilter],
    -- | The ID of the instance whose patch state information should be
    -- retrieved.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancePatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatches_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeInstancePatches_maxResults' - The maximum number of patches to return (per page).
--
-- 'filters', 'describeInstancePatches_filters' - An array of structures. Each entry in the array is a structure
-- containing a Key, Value combination. Valid values for Key are
-- @Classification@ | @KBId@ | @Severity@ | @State@.
--
-- 'instanceId', 'describeInstancePatches_instanceId' - The ID of the instance whose patch state information should be
-- retrieved.
newDescribeInstancePatches ::
  -- | 'instanceId'
  Prelude.Text ->
  DescribeInstancePatches
newDescribeInstancePatches pInstanceId_ =
  DescribeInstancePatches'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatches_nextToken :: Lens.Lens' DescribeInstancePatches (Prelude.Maybe Prelude.Text)
describeInstancePatches_nextToken = Lens.lens (\DescribeInstancePatches' {nextToken} -> nextToken) (\s@DescribeInstancePatches' {} a -> s {nextToken = a} :: DescribeInstancePatches)

-- | The maximum number of patches to return (per page).
describeInstancePatches_maxResults :: Lens.Lens' DescribeInstancePatches (Prelude.Maybe Prelude.Natural)
describeInstancePatches_maxResults = Lens.lens (\DescribeInstancePatches' {maxResults} -> maxResults) (\s@DescribeInstancePatches' {} a -> s {maxResults = a} :: DescribeInstancePatches)

-- | An array of structures. Each entry in the array is a structure
-- containing a Key, Value combination. Valid values for Key are
-- @Classification@ | @KBId@ | @Severity@ | @State@.
describeInstancePatches_filters :: Lens.Lens' DescribeInstancePatches (Prelude.Maybe [PatchOrchestratorFilter])
describeInstancePatches_filters = Lens.lens (\DescribeInstancePatches' {filters} -> filters) (\s@DescribeInstancePatches' {} a -> s {filters = a} :: DescribeInstancePatches) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the instance whose patch state information should be
-- retrieved.
describeInstancePatches_instanceId :: Lens.Lens' DescribeInstancePatches Prelude.Text
describeInstancePatches_instanceId = Lens.lens (\DescribeInstancePatches' {instanceId} -> instanceId) (\s@DescribeInstancePatches' {} a -> s {instanceId = a} :: DescribeInstancePatches)

instance Pager.AWSPager DescribeInstancePatches where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeInstancePatchesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeInstancePatchesResponse_patches
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeInstancePatches_nextToken
          Lens..~ rs
          Lens.^? describeInstancePatchesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeInstancePatches where
  type
    Rs DescribeInstancePatches =
      DescribeInstancePatchesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Patches" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstancePatches

instance Prelude.NFData DescribeInstancePatches

instance Prelude.ToHeaders DescribeInstancePatches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DescribeInstancePatches" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeInstancePatches where
  toJSON DescribeInstancePatches' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filters" Prelude..=) Prelude.<$> filters,
            Prelude.Just ("InstanceId" Prelude..= instanceId)
          ]
      )

instance Prelude.ToPath DescribeInstancePatches where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeInstancePatches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstancePatchesResponse' smart constructor.
data DescribeInstancePatchesResponse = DescribeInstancePatchesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each entry in the array is a structure containing:
    --
    -- Title (string)
    --
    -- KBId (string)
    --
    -- Classification (string)
    --
    -- Severity (string)
    --
    -- State (string, such as \"INSTALLED\" or \"FAILED\")
    --
    -- InstalledTime (DateTime)
    --
    -- InstalledBy (string)
    patches :: Prelude.Maybe [PatchComplianceData],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- Title (string)
--
-- KBId (string)
--
-- Classification (string)
--
-- Severity (string)
--
-- State (string, such as \"INSTALLED\" or \"FAILED\")
--
-- InstalledTime (DateTime)
--
-- InstalledBy (string)
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
-- Title (string)
--
-- KBId (string)
--
-- Classification (string)
--
-- Severity (string)
--
-- State (string, such as \"INSTALLED\" or \"FAILED\")
--
-- InstalledTime (DateTime)
--
-- InstalledBy (string)
describeInstancePatchesResponse_patches :: Lens.Lens' DescribeInstancePatchesResponse (Prelude.Maybe [PatchComplianceData])
describeInstancePatchesResponse_patches = Lens.lens (\DescribeInstancePatchesResponse' {patches} -> patches) (\s@DescribeInstancePatchesResponse' {} a -> s {patches = a} :: DescribeInstancePatchesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeInstancePatchesResponse_httpStatus :: Lens.Lens' DescribeInstancePatchesResponse Prelude.Int
describeInstancePatchesResponse_httpStatus = Lens.lens (\DescribeInstancePatchesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchesResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchesResponse)

instance
  Prelude.NFData
    DescribeInstancePatchesResponse
