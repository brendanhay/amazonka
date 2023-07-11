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
-- Module      : Amazonka.SSM.DescribeDocumentPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a Amazon Web Services Systems Manager
-- document (SSM document). If you created the document, you are the owner.
-- If a document is shared, it can either be shared privately (by
-- specifying a user\'s Amazon Web Services account ID) or publicly
-- (/All/).
module Amazonka.SSM.DescribeDocumentPermission
  ( -- * Creating a Request
    DescribeDocumentPermission (..),
    newDescribeDocumentPermission,

    -- * Request Lenses
    describeDocumentPermission_maxResults,
    describeDocumentPermission_nextToken,
    describeDocumentPermission_name,
    describeDocumentPermission_permissionType,

    -- * Destructuring the Response
    DescribeDocumentPermissionResponse (..),
    newDescribeDocumentPermissionResponse,

    -- * Response Lenses
    describeDocumentPermissionResponse_accountIds,
    describeDocumentPermissionResponse_accountSharingInfoList,
    describeDocumentPermissionResponse_nextToken,
    describeDocumentPermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeDocumentPermission' smart constructor.
data DescribeDocumentPermission = DescribeDocumentPermission'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the document for which you are the owner.
    name :: Prelude.Text,
    -- | The permission type for the document. The permission type can be
    -- /Share/.
    permissionType :: DocumentPermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeDocumentPermission_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describeDocumentPermission_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'name', 'describeDocumentPermission_name' - The name of the document for which you are the owner.
--
-- 'permissionType', 'describeDocumentPermission_permissionType' - The permission type for the document. The permission type can be
-- /Share/.
newDescribeDocumentPermission ::
  -- | 'name'
  Prelude.Text ->
  -- | 'permissionType'
  DocumentPermissionType ->
  DescribeDocumentPermission
newDescribeDocumentPermission pName_ pPermissionType_ =
  DescribeDocumentPermission'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_,
      permissionType = pPermissionType_
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeDocumentPermission_maxResults :: Lens.Lens' DescribeDocumentPermission (Prelude.Maybe Prelude.Natural)
describeDocumentPermission_maxResults = Lens.lens (\DescribeDocumentPermission' {maxResults} -> maxResults) (\s@DescribeDocumentPermission' {} a -> s {maxResults = a} :: DescribeDocumentPermission)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeDocumentPermission_nextToken :: Lens.Lens' DescribeDocumentPermission (Prelude.Maybe Prelude.Text)
describeDocumentPermission_nextToken = Lens.lens (\DescribeDocumentPermission' {nextToken} -> nextToken) (\s@DescribeDocumentPermission' {} a -> s {nextToken = a} :: DescribeDocumentPermission)

-- | The name of the document for which you are the owner.
describeDocumentPermission_name :: Lens.Lens' DescribeDocumentPermission Prelude.Text
describeDocumentPermission_name = Lens.lens (\DescribeDocumentPermission' {name} -> name) (\s@DescribeDocumentPermission' {} a -> s {name = a} :: DescribeDocumentPermission)

-- | The permission type for the document. The permission type can be
-- /Share/.
describeDocumentPermission_permissionType :: Lens.Lens' DescribeDocumentPermission DocumentPermissionType
describeDocumentPermission_permissionType = Lens.lens (\DescribeDocumentPermission' {permissionType} -> permissionType) (\s@DescribeDocumentPermission' {} a -> s {permissionType = a} :: DescribeDocumentPermission)

instance Core.AWSRequest DescribeDocumentPermission where
  type
    AWSResponse DescribeDocumentPermission =
      DescribeDocumentPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentPermissionResponse'
            Prelude.<$> (x Data..?> "AccountIds" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "AccountSharingInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDocumentPermission where
  hashWithSalt _salt DescribeDocumentPermission' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permissionType

instance Prelude.NFData DescribeDocumentPermission where
  rnf DescribeDocumentPermission' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionType

instance Data.ToHeaders DescribeDocumentPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeDocumentPermission" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDocumentPermission where
  toJSON DescribeDocumentPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("PermissionType" Data..= permissionType)
          ]
      )

instance Data.ToPath DescribeDocumentPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDocumentPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDocumentPermissionResponse' smart constructor.
data DescribeDocumentPermissionResponse = DescribeDocumentPermissionResponse'
  { -- | The account IDs that have permission to use this document. The ID can be
    -- either an Amazon Web Services account or /All/.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of Amazon Web Services accounts where the current document is
    -- shared and the version shared with each account.
    accountSharingInfoList :: Prelude.Maybe [AccountSharingInfo],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'describeDocumentPermissionResponse_accountIds' - The account IDs that have permission to use this document. The ID can be
-- either an Amazon Web Services account or /All/.
--
-- 'accountSharingInfoList', 'describeDocumentPermissionResponse_accountSharingInfoList' - A list of Amazon Web Services accounts where the current document is
-- shared and the version shared with each account.
--
-- 'nextToken', 'describeDocumentPermissionResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'describeDocumentPermissionResponse_httpStatus' - The response's http status code.
newDescribeDocumentPermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDocumentPermissionResponse
newDescribeDocumentPermissionResponse pHttpStatus_ =
  DescribeDocumentPermissionResponse'
    { accountIds =
        Prelude.Nothing,
      accountSharingInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The account IDs that have permission to use this document. The ID can be
-- either an Amazon Web Services account or /All/.
describeDocumentPermissionResponse_accountIds :: Lens.Lens' DescribeDocumentPermissionResponse (Prelude.Maybe [Prelude.Text])
describeDocumentPermissionResponse_accountIds = Lens.lens (\DescribeDocumentPermissionResponse' {accountIds} -> accountIds) (\s@DescribeDocumentPermissionResponse' {} a -> s {accountIds = a} :: DescribeDocumentPermissionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of Amazon Web Services accounts where the current document is
-- shared and the version shared with each account.
describeDocumentPermissionResponse_accountSharingInfoList :: Lens.Lens' DescribeDocumentPermissionResponse (Prelude.Maybe [AccountSharingInfo])
describeDocumentPermissionResponse_accountSharingInfoList = Lens.lens (\DescribeDocumentPermissionResponse' {accountSharingInfoList} -> accountSharingInfoList) (\s@DescribeDocumentPermissionResponse' {} a -> s {accountSharingInfoList = a} :: DescribeDocumentPermissionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeDocumentPermissionResponse_nextToken :: Lens.Lens' DescribeDocumentPermissionResponse (Prelude.Maybe Prelude.Text)
describeDocumentPermissionResponse_nextToken = Lens.lens (\DescribeDocumentPermissionResponse' {nextToken} -> nextToken) (\s@DescribeDocumentPermissionResponse' {} a -> s {nextToken = a} :: DescribeDocumentPermissionResponse)

-- | The response's http status code.
describeDocumentPermissionResponse_httpStatus :: Lens.Lens' DescribeDocumentPermissionResponse Prelude.Int
describeDocumentPermissionResponse_httpStatus = Lens.lens (\DescribeDocumentPermissionResponse' {httpStatus} -> httpStatus) (\s@DescribeDocumentPermissionResponse' {} a -> s {httpStatus = a} :: DescribeDocumentPermissionResponse)

instance
  Prelude.NFData
    DescribeDocumentPermissionResponse
  where
  rnf DescribeDocumentPermissionResponse' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf accountSharingInfoList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
