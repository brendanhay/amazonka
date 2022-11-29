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
-- Module      : Amazonka.SSM.DescribeAvailablePatches
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patches eligible to be included in a patch baseline.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeAvailablePatches
  ( -- * Creating a Request
    DescribeAvailablePatches (..),
    newDescribeAvailablePatches,

    -- * Request Lenses
    describeAvailablePatches_nextToken,
    describeAvailablePatches_filters,
    describeAvailablePatches_maxResults,

    -- * Destructuring the Response
    DescribeAvailablePatchesResponse (..),
    newDescribeAvailablePatchesResponse,

    -- * Response Lenses
    describeAvailablePatchesResponse_nextToken,
    describeAvailablePatchesResponse_patches,
    describeAvailablePatchesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeAvailablePatches' smart constructor.
data DescribeAvailablePatches = DescribeAvailablePatches'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each element in the array is a structure containing a key-value pair.
    --
    -- __Windows Server__
    --
    -- Supported keys for Windows Server managed node patches include the
    -- following:
    --
    -- -   __@PATCH_SET@__
    --
    --     Sample values: @OS@ | @APPLICATION@
    --
    -- -   __@PRODUCT@__
    --
    --     Sample values: @WindowsServer2012@ | @Office 2010@ |
    --     @MicrosoftDefenderAntivirus@
    --
    -- -   __@PRODUCT_FAMILY@__
    --
    --     Sample values: @Windows@ | @Office@
    --
    -- -   __@MSRC_SEVERITY@__
    --
    --     Sample values: @ServicePacks@ | @Important@ | @Moderate@
    --
    -- -   __@CLASSIFICATION@__
    --
    --     Sample values: @ServicePacks@ | @SecurityUpdates@ |
    --     @DefinitionUpdates@
    --
    -- -   __@PATCH_ID@__
    --
    --     Sample values: @KB123456@ | @KB4516046@
    --
    -- __Linux__
    --
    -- When specifying filters for Linux patches, you must specify a key-pair
    -- for @PRODUCT@. For example, using the Command Line Interface (CLI), the
    -- following command fails:
    --
    -- @aws ssm describe-available-patches --filters Key=CVE_ID,Values=CVE-2018-3615@
    --
    -- However, the following command succeeds:
    --
    -- @aws ssm describe-available-patches --filters Key=PRODUCT,Values=AmazonLinux2018.03 Key=CVE_ID,Values=CVE-2018-3615@
    --
    -- Supported keys for Linux managed node patches include the following:
    --
    -- -   __@PRODUCT@__
    --
    --     Sample values: @AmazonLinux2018.03@ | @AmazonLinux2.0@
    --
    -- -   __@NAME@__
    --
    --     Sample values: @kernel-headers@ | @samba-python@ | @php@
    --
    -- -   __@SEVERITY@__
    --
    --     Sample values: @Critical@ | @Important@ | @Medium@ | @Low@
    --
    -- -   __@EPOCH@__
    --
    --     Sample values: @0@ | @1@
    --
    -- -   __@VERSION@__
    --
    --     Sample values: @78.6.1@ | @4.10.16@
    --
    -- -   __@RELEASE@__
    --
    --     Sample values: @9.56.amzn1@ | @1.amzn2@
    --
    -- -   __@ARCH@__
    --
    --     Sample values: @i686@ | @x86_64@
    --
    -- -   __@REPOSITORY@__
    --
    --     Sample values: @Core@ | @Updates@
    --
    -- -   __@ADVISORY_ID@__
    --
    --     Sample values: @ALAS-2018-1058@ | @ALAS2-2021-1594@
    --
    -- -   __@CVE_ID@__
    --
    --     Sample values: @CVE-2018-3615@ | @CVE-2020-1472@
    --
    -- -   __@BUGZILLA_ID@__
    --
    --     Sample values: @1463241@
    filters :: Prelude.Maybe [PatchOrchestratorFilter],
    -- | The maximum number of patches to return (per page).
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailablePatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAvailablePatches_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'describeAvailablePatches_filters' - Each element in the array is a structure containing a key-value pair.
--
-- __Windows Server__
--
-- Supported keys for Windows Server managed node patches include the
-- following:
--
-- -   __@PATCH_SET@__
--
--     Sample values: @OS@ | @APPLICATION@
--
-- -   __@PRODUCT@__
--
--     Sample values: @WindowsServer2012@ | @Office 2010@ |
--     @MicrosoftDefenderAntivirus@
--
-- -   __@PRODUCT_FAMILY@__
--
--     Sample values: @Windows@ | @Office@
--
-- -   __@MSRC_SEVERITY@__
--
--     Sample values: @ServicePacks@ | @Important@ | @Moderate@
--
-- -   __@CLASSIFICATION@__
--
--     Sample values: @ServicePacks@ | @SecurityUpdates@ |
--     @DefinitionUpdates@
--
-- -   __@PATCH_ID@__
--
--     Sample values: @KB123456@ | @KB4516046@
--
-- __Linux__
--
-- When specifying filters for Linux patches, you must specify a key-pair
-- for @PRODUCT@. For example, using the Command Line Interface (CLI), the
-- following command fails:
--
-- @aws ssm describe-available-patches --filters Key=CVE_ID,Values=CVE-2018-3615@
--
-- However, the following command succeeds:
--
-- @aws ssm describe-available-patches --filters Key=PRODUCT,Values=AmazonLinux2018.03 Key=CVE_ID,Values=CVE-2018-3615@
--
-- Supported keys for Linux managed node patches include the following:
--
-- -   __@PRODUCT@__
--
--     Sample values: @AmazonLinux2018.03@ | @AmazonLinux2.0@
--
-- -   __@NAME@__
--
--     Sample values: @kernel-headers@ | @samba-python@ | @php@
--
-- -   __@SEVERITY@__
--
--     Sample values: @Critical@ | @Important@ | @Medium@ | @Low@
--
-- -   __@EPOCH@__
--
--     Sample values: @0@ | @1@
--
-- -   __@VERSION@__
--
--     Sample values: @78.6.1@ | @4.10.16@
--
-- -   __@RELEASE@__
--
--     Sample values: @9.56.amzn1@ | @1.amzn2@
--
-- -   __@ARCH@__
--
--     Sample values: @i686@ | @x86_64@
--
-- -   __@REPOSITORY@__
--
--     Sample values: @Core@ | @Updates@
--
-- -   __@ADVISORY_ID@__
--
--     Sample values: @ALAS-2018-1058@ | @ALAS2-2021-1594@
--
-- -   __@CVE_ID@__
--
--     Sample values: @CVE-2018-3615@ | @CVE-2020-1472@
--
-- -   __@BUGZILLA_ID@__
--
--     Sample values: @1463241@
--
-- 'maxResults', 'describeAvailablePatches_maxResults' - The maximum number of patches to return (per page).
newDescribeAvailablePatches ::
  DescribeAvailablePatches
newDescribeAvailablePatches =
  DescribeAvailablePatches'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAvailablePatches_nextToken :: Lens.Lens' DescribeAvailablePatches (Prelude.Maybe Prelude.Text)
describeAvailablePatches_nextToken = Lens.lens (\DescribeAvailablePatches' {nextToken} -> nextToken) (\s@DescribeAvailablePatches' {} a -> s {nextToken = a} :: DescribeAvailablePatches)

-- | Each element in the array is a structure containing a key-value pair.
--
-- __Windows Server__
--
-- Supported keys for Windows Server managed node patches include the
-- following:
--
-- -   __@PATCH_SET@__
--
--     Sample values: @OS@ | @APPLICATION@
--
-- -   __@PRODUCT@__
--
--     Sample values: @WindowsServer2012@ | @Office 2010@ |
--     @MicrosoftDefenderAntivirus@
--
-- -   __@PRODUCT_FAMILY@__
--
--     Sample values: @Windows@ | @Office@
--
-- -   __@MSRC_SEVERITY@__
--
--     Sample values: @ServicePacks@ | @Important@ | @Moderate@
--
-- -   __@CLASSIFICATION@__
--
--     Sample values: @ServicePacks@ | @SecurityUpdates@ |
--     @DefinitionUpdates@
--
-- -   __@PATCH_ID@__
--
--     Sample values: @KB123456@ | @KB4516046@
--
-- __Linux__
--
-- When specifying filters for Linux patches, you must specify a key-pair
-- for @PRODUCT@. For example, using the Command Line Interface (CLI), the
-- following command fails:
--
-- @aws ssm describe-available-patches --filters Key=CVE_ID,Values=CVE-2018-3615@
--
-- However, the following command succeeds:
--
-- @aws ssm describe-available-patches --filters Key=PRODUCT,Values=AmazonLinux2018.03 Key=CVE_ID,Values=CVE-2018-3615@
--
-- Supported keys for Linux managed node patches include the following:
--
-- -   __@PRODUCT@__
--
--     Sample values: @AmazonLinux2018.03@ | @AmazonLinux2.0@
--
-- -   __@NAME@__
--
--     Sample values: @kernel-headers@ | @samba-python@ | @php@
--
-- -   __@SEVERITY@__
--
--     Sample values: @Critical@ | @Important@ | @Medium@ | @Low@
--
-- -   __@EPOCH@__
--
--     Sample values: @0@ | @1@
--
-- -   __@VERSION@__
--
--     Sample values: @78.6.1@ | @4.10.16@
--
-- -   __@RELEASE@__
--
--     Sample values: @9.56.amzn1@ | @1.amzn2@
--
-- -   __@ARCH@__
--
--     Sample values: @i686@ | @x86_64@
--
-- -   __@REPOSITORY@__
--
--     Sample values: @Core@ | @Updates@
--
-- -   __@ADVISORY_ID@__
--
--     Sample values: @ALAS-2018-1058@ | @ALAS2-2021-1594@
--
-- -   __@CVE_ID@__
--
--     Sample values: @CVE-2018-3615@ | @CVE-2020-1472@
--
-- -   __@BUGZILLA_ID@__
--
--     Sample values: @1463241@
describeAvailablePatches_filters :: Lens.Lens' DescribeAvailablePatches (Prelude.Maybe [PatchOrchestratorFilter])
describeAvailablePatches_filters = Lens.lens (\DescribeAvailablePatches' {filters} -> filters) (\s@DescribeAvailablePatches' {} a -> s {filters = a} :: DescribeAvailablePatches) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of patches to return (per page).
describeAvailablePatches_maxResults :: Lens.Lens' DescribeAvailablePatches (Prelude.Maybe Prelude.Natural)
describeAvailablePatches_maxResults = Lens.lens (\DescribeAvailablePatches' {maxResults} -> maxResults) (\s@DescribeAvailablePatches' {} a -> s {maxResults = a} :: DescribeAvailablePatches)

instance Core.AWSPager DescribeAvailablePatches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAvailablePatchesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAvailablePatchesResponse_patches
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAvailablePatches_nextToken
          Lens..~ rs
          Lens.^? describeAvailablePatchesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAvailablePatches where
  type
    AWSResponse DescribeAvailablePatches =
      DescribeAvailablePatchesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAvailablePatchesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Patches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAvailablePatches where
  hashWithSalt _salt DescribeAvailablePatches' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeAvailablePatches where
  rnf DescribeAvailablePatches' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeAvailablePatches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeAvailablePatches" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAvailablePatches where
  toJSON DescribeAvailablePatches' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeAvailablePatches where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAvailablePatches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAvailablePatchesResponse' smart constructor.
data DescribeAvailablePatchesResponse = DescribeAvailablePatchesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of patches. Each entry in the array is a patch structure.
    patches :: Prelude.Maybe [Patch],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailablePatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAvailablePatchesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'patches', 'describeAvailablePatchesResponse_patches' - An array of patches. Each entry in the array is a patch structure.
--
-- 'httpStatus', 'describeAvailablePatchesResponse_httpStatus' - The response's http status code.
newDescribeAvailablePatchesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAvailablePatchesResponse
newDescribeAvailablePatchesResponse pHttpStatus_ =
  DescribeAvailablePatchesResponse'
    { nextToken =
        Prelude.Nothing,
      patches = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeAvailablePatchesResponse_nextToken :: Lens.Lens' DescribeAvailablePatchesResponse (Prelude.Maybe Prelude.Text)
describeAvailablePatchesResponse_nextToken = Lens.lens (\DescribeAvailablePatchesResponse' {nextToken} -> nextToken) (\s@DescribeAvailablePatchesResponse' {} a -> s {nextToken = a} :: DescribeAvailablePatchesResponse)

-- | An array of patches. Each entry in the array is a patch structure.
describeAvailablePatchesResponse_patches :: Lens.Lens' DescribeAvailablePatchesResponse (Prelude.Maybe [Patch])
describeAvailablePatchesResponse_patches = Lens.lens (\DescribeAvailablePatchesResponse' {patches} -> patches) (\s@DescribeAvailablePatchesResponse' {} a -> s {patches = a} :: DescribeAvailablePatchesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAvailablePatchesResponse_httpStatus :: Lens.Lens' DescribeAvailablePatchesResponse Prelude.Int
describeAvailablePatchesResponse_httpStatus = Lens.lens (\DescribeAvailablePatchesResponse' {httpStatus} -> httpStatus) (\s@DescribeAvailablePatchesResponse' {} a -> s {httpStatus = a} :: DescribeAvailablePatchesResponse)

instance
  Prelude.NFData
    DescribeAvailablePatchesResponse
  where
  rnf DescribeAvailablePatchesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf patches
      `Prelude.seq` Prelude.rnf httpStatus
