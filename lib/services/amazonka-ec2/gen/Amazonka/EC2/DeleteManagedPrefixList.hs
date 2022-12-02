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
-- Module      : Amazonka.EC2.DeleteManagedPrefixList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed prefix list. You must first remove all
-- references to the prefix list in your resources.
module Amazonka.EC2.DeleteManagedPrefixList
  ( -- * Creating a Request
    DeleteManagedPrefixList (..),
    newDeleteManagedPrefixList,

    -- * Request Lenses
    deleteManagedPrefixList_dryRun,
    deleteManagedPrefixList_prefixListId,

    -- * Destructuring the Response
    DeleteManagedPrefixListResponse (..),
    newDeleteManagedPrefixListResponse,

    -- * Response Lenses
    deleteManagedPrefixListResponse_prefixList,
    deleteManagedPrefixListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteManagedPrefixList' smart constructor.
data DeleteManagedPrefixList = DeleteManagedPrefixList'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteManagedPrefixList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteManagedPrefixList_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'prefixListId', 'deleteManagedPrefixList_prefixListId' - The ID of the prefix list.
newDeleteManagedPrefixList ::
  -- | 'prefixListId'
  Prelude.Text ->
  DeleteManagedPrefixList
newDeleteManagedPrefixList pPrefixListId_ =
  DeleteManagedPrefixList'
    { dryRun = Prelude.Nothing,
      prefixListId = pPrefixListId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteManagedPrefixList_dryRun :: Lens.Lens' DeleteManagedPrefixList (Prelude.Maybe Prelude.Bool)
deleteManagedPrefixList_dryRun = Lens.lens (\DeleteManagedPrefixList' {dryRun} -> dryRun) (\s@DeleteManagedPrefixList' {} a -> s {dryRun = a} :: DeleteManagedPrefixList)

-- | The ID of the prefix list.
deleteManagedPrefixList_prefixListId :: Lens.Lens' DeleteManagedPrefixList Prelude.Text
deleteManagedPrefixList_prefixListId = Lens.lens (\DeleteManagedPrefixList' {prefixListId} -> prefixListId) (\s@DeleteManagedPrefixList' {} a -> s {prefixListId = a} :: DeleteManagedPrefixList)

instance Core.AWSRequest DeleteManagedPrefixList where
  type
    AWSResponse DeleteManagedPrefixList =
      DeleteManagedPrefixListResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteManagedPrefixListResponse'
            Prelude.<$> (x Data..@? "prefixList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteManagedPrefixList where
  hashWithSalt _salt DeleteManagedPrefixList' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` prefixListId

instance Prelude.NFData DeleteManagedPrefixList where
  rnf DeleteManagedPrefixList' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf prefixListId

instance Data.ToHeaders DeleteManagedPrefixList where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteManagedPrefixList where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteManagedPrefixList where
  toQuery DeleteManagedPrefixList' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteManagedPrefixList" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "PrefixListId" Data.=: prefixListId
      ]

-- | /See:/ 'newDeleteManagedPrefixListResponse' smart constructor.
data DeleteManagedPrefixListResponse = DeleteManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Prelude.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteManagedPrefixListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixList', 'deleteManagedPrefixListResponse_prefixList' - Information about the prefix list.
--
-- 'httpStatus', 'deleteManagedPrefixListResponse_httpStatus' - The response's http status code.
newDeleteManagedPrefixListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteManagedPrefixListResponse
newDeleteManagedPrefixListResponse pHttpStatus_ =
  DeleteManagedPrefixListResponse'
    { prefixList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prefix list.
deleteManagedPrefixListResponse_prefixList :: Lens.Lens' DeleteManagedPrefixListResponse (Prelude.Maybe ManagedPrefixList)
deleteManagedPrefixListResponse_prefixList = Lens.lens (\DeleteManagedPrefixListResponse' {prefixList} -> prefixList) (\s@DeleteManagedPrefixListResponse' {} a -> s {prefixList = a} :: DeleteManagedPrefixListResponse)

-- | The response's http status code.
deleteManagedPrefixListResponse_httpStatus :: Lens.Lens' DeleteManagedPrefixListResponse Prelude.Int
deleteManagedPrefixListResponse_httpStatus = Lens.lens (\DeleteManagedPrefixListResponse' {httpStatus} -> httpStatus) (\s@DeleteManagedPrefixListResponse' {} a -> s {httpStatus = a} :: DeleteManagedPrefixListResponse)

instance
  Prelude.NFData
    DeleteManagedPrefixListResponse
  where
  rnf DeleteManagedPrefixListResponse' {..} =
    Prelude.rnf prefixList
      `Prelude.seq` Prelude.rnf httpStatus
