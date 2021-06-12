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
-- Module      : Network.AWS.CloudFormation.ListImports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all stacks that are importing an exported output value. To modify
-- or remove an exported output value, first use this action to see which
-- stacks are using it. To see the exported output values in your account,
-- see ListExports.
--
-- For more information about importing an exported output value, see the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html Fn::ImportValue>
-- function.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListImports
  ( -- * Creating a Request
    ListImports (..),
    newListImports,

    -- * Request Lenses
    listImports_nextToken,
    listImports_exportName,

    -- * Destructuring the Response
    ListImportsResponse (..),
    newListImportsResponse,

    -- * Response Lenses
    listImportsResponse_nextToken,
    listImportsResponse_imports,
    listImportsResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListImports' smart constructor.
data ListImports = ListImports'
  { -- | A string (provided by the ListImports response output) that identifies
    -- the next page of stacks that are importing the specified exported output
    -- value.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the exported output value. AWS CloudFormation returns the
    -- stack names that are importing this value.
    exportName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListImports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImports_nextToken' - A string (provided by the ListImports response output) that identifies
-- the next page of stacks that are importing the specified exported output
-- value.
--
-- 'exportName', 'listImports_exportName' - The name of the exported output value. AWS CloudFormation returns the
-- stack names that are importing this value.
newListImports ::
  -- | 'exportName'
  Core.Text ->
  ListImports
newListImports pExportName_ =
  ListImports'
    { nextToken = Core.Nothing,
      exportName = pExportName_
    }

-- | A string (provided by the ListImports response output) that identifies
-- the next page of stacks that are importing the specified exported output
-- value.
listImports_nextToken :: Lens.Lens' ListImports (Core.Maybe Core.Text)
listImports_nextToken = Lens.lens (\ListImports' {nextToken} -> nextToken) (\s@ListImports' {} a -> s {nextToken = a} :: ListImports)

-- | The name of the exported output value. AWS CloudFormation returns the
-- stack names that are importing this value.
listImports_exportName :: Lens.Lens' ListImports Core.Text
listImports_exportName = Lens.lens (\ListImports' {exportName} -> exportName) (\s@ListImports' {} a -> s {exportName = a} :: ListImports)

instance Core.AWSPager ListImports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImportsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listImportsResponse_imports Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listImports_nextToken
          Lens..~ rs
          Lens.^? listImportsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListImports where
  type AWSResponse ListImports = ListImportsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListImportsResult"
      ( \s h x ->
          ListImportsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Imports" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListImports

instance Core.NFData ListImports

instance Core.ToHeaders ListImports where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListImports where
  toPath = Core.const "/"

instance Core.ToQuery ListImports where
  toQuery ListImports' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ListImports" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "ExportName" Core.=: exportName
      ]

-- | /See:/ 'newListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | A string that identifies the next page of exports. If there is no
    -- additional page, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of stack names that are importing the specified exported output
    -- value.
    imports :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListImportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImportsResponse_nextToken' - A string that identifies the next page of exports. If there is no
-- additional page, this value is null.
--
-- 'imports', 'listImportsResponse_imports' - A list of stack names that are importing the specified exported output
-- value.
--
-- 'httpStatus', 'listImportsResponse_httpStatus' - The response's http status code.
newListImportsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListImportsResponse
newListImportsResponse pHttpStatus_ =
  ListImportsResponse'
    { nextToken = Core.Nothing,
      imports = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that identifies the next page of exports. If there is no
-- additional page, this value is null.
listImportsResponse_nextToken :: Lens.Lens' ListImportsResponse (Core.Maybe Core.Text)
listImportsResponse_nextToken = Lens.lens (\ListImportsResponse' {nextToken} -> nextToken) (\s@ListImportsResponse' {} a -> s {nextToken = a} :: ListImportsResponse)

-- | A list of stack names that are importing the specified exported output
-- value.
listImportsResponse_imports :: Lens.Lens' ListImportsResponse (Core.Maybe [Core.Text])
listImportsResponse_imports = Lens.lens (\ListImportsResponse' {imports} -> imports) (\s@ListImportsResponse' {} a -> s {imports = a} :: ListImportsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listImportsResponse_httpStatus :: Lens.Lens' ListImportsResponse Core.Int
listImportsResponse_httpStatus = Lens.lens (\ListImportsResponse' {httpStatus} -> httpStatus) (\s@ListImportsResponse' {} a -> s {httpStatus = a} :: ListImportsResponse)

instance Core.NFData ListImportsResponse
