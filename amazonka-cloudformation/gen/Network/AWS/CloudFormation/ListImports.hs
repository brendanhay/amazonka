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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListImports' smart constructor.
data ListImports = ListImports'
  { -- | A string (provided by the ListImports response output) that identifies
    -- the next page of stacks that are importing the specified exported output
    -- value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the exported output value. AWS CloudFormation returns the
    -- stack names that are importing this value.
    exportName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListImports
newListImports pExportName_ =
  ListImports'
    { nextToken = Prelude.Nothing,
      exportName = pExportName_
    }

-- | A string (provided by the ListImports response output) that identifies
-- the next page of stacks that are importing the specified exported output
-- value.
listImports_nextToken :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_nextToken = Lens.lens (\ListImports' {nextToken} -> nextToken) (\s@ListImports' {} a -> s {nextToken = a} :: ListImports)

-- | The name of the exported output value. AWS CloudFormation returns the
-- stack names that are importing this value.
listImports_exportName :: Lens.Lens' ListImports Prelude.Text
listImports_exportName = Lens.lens (\ListImports' {exportName} -> exportName) (\s@ListImports' {} a -> s {exportName = a} :: ListImports)

instance Pager.AWSPager ListImports where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listImportsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listImportsResponse_imports Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listImports_nextToken
          Lens..~ rs
          Lens.^? listImportsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListImports where
  type Rs ListImports = ListImportsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListImportsResult"
      ( \s h x ->
          ListImportsResponse'
            Prelude.<$> (x Prelude..@? "NextToken")
            Prelude.<*> ( x Prelude..@? "Imports" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImports

instance Prelude.NFData ListImports

instance Prelude.ToHeaders ListImports where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListImports where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListImports where
  toQuery ListImports' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListImports" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "ExportName" Prelude.=: exportName
      ]

-- | /See:/ 'newListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | A string that identifies the next page of exports. If there is no
    -- additional page, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of stack names that are importing the specified exported output
    -- value.
    imports :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListImportsResponse
newListImportsResponse pHttpStatus_ =
  ListImportsResponse'
    { nextToken = Prelude.Nothing,
      imports = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that identifies the next page of exports. If there is no
-- additional page, this value is null.
listImportsResponse_nextToken :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_nextToken = Lens.lens (\ListImportsResponse' {nextToken} -> nextToken) (\s@ListImportsResponse' {} a -> s {nextToken = a} :: ListImportsResponse)

-- | A list of stack names that are importing the specified exported output
-- value.
listImportsResponse_imports :: Lens.Lens' ListImportsResponse (Prelude.Maybe [Prelude.Text])
listImportsResponse_imports = Lens.lens (\ListImportsResponse' {imports} -> imports) (\s@ListImportsResponse' {} a -> s {imports = a} :: ListImportsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listImportsResponse_httpStatus :: Lens.Lens' ListImportsResponse Prelude.Int
listImportsResponse_httpStatus = Lens.lens (\ListImportsResponse' {httpStatus} -> httpStatus) (\s@ListImportsResponse' {} a -> s {httpStatus = a} :: ListImportsResponse)

instance Prelude.NFData ListImportsResponse
