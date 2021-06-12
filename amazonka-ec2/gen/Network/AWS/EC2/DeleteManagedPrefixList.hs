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
-- Module      : Network.AWS.EC2.DeleteManagedPrefixList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed prefix list. You must first remove all
-- references to the prefix list in your resources.
module Network.AWS.EC2.DeleteManagedPrefixList
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteManagedPrefixList' smart constructor.
data DeleteManagedPrefixList = DeleteManagedPrefixList'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the prefix list.
    prefixListId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteManagedPrefixList
newDeleteManagedPrefixList pPrefixListId_ =
  DeleteManagedPrefixList'
    { dryRun = Core.Nothing,
      prefixListId = pPrefixListId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteManagedPrefixList_dryRun :: Lens.Lens' DeleteManagedPrefixList (Core.Maybe Core.Bool)
deleteManagedPrefixList_dryRun = Lens.lens (\DeleteManagedPrefixList' {dryRun} -> dryRun) (\s@DeleteManagedPrefixList' {} a -> s {dryRun = a} :: DeleteManagedPrefixList)

-- | The ID of the prefix list.
deleteManagedPrefixList_prefixListId :: Lens.Lens' DeleteManagedPrefixList Core.Text
deleteManagedPrefixList_prefixListId = Lens.lens (\DeleteManagedPrefixList' {prefixListId} -> prefixListId) (\s@DeleteManagedPrefixList' {} a -> s {prefixListId = a} :: DeleteManagedPrefixList)

instance Core.AWSRequest DeleteManagedPrefixList where
  type
    AWSResponse DeleteManagedPrefixList =
      DeleteManagedPrefixListResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteManagedPrefixListResponse'
            Core.<$> (x Core..@? "prefixList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteManagedPrefixList

instance Core.NFData DeleteManagedPrefixList

instance Core.ToHeaders DeleteManagedPrefixList where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteManagedPrefixList where
  toPath = Core.const "/"

instance Core.ToQuery DeleteManagedPrefixList where
  toQuery DeleteManagedPrefixList' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteManagedPrefixList" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "PrefixListId" Core.=: prefixListId
      ]

-- | /See:/ 'newDeleteManagedPrefixListResponse' smart constructor.
data DeleteManagedPrefixListResponse = DeleteManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Core.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteManagedPrefixListResponse
newDeleteManagedPrefixListResponse pHttpStatus_ =
  DeleteManagedPrefixListResponse'
    { prefixList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prefix list.
deleteManagedPrefixListResponse_prefixList :: Lens.Lens' DeleteManagedPrefixListResponse (Core.Maybe ManagedPrefixList)
deleteManagedPrefixListResponse_prefixList = Lens.lens (\DeleteManagedPrefixListResponse' {prefixList} -> prefixList) (\s@DeleteManagedPrefixListResponse' {} a -> s {prefixList = a} :: DeleteManagedPrefixListResponse)

-- | The response's http status code.
deleteManagedPrefixListResponse_httpStatus :: Lens.Lens' DeleteManagedPrefixListResponse Core.Int
deleteManagedPrefixListResponse_httpStatus = Lens.lens (\DeleteManagedPrefixListResponse' {httpStatus} -> httpStatus) (\s@DeleteManagedPrefixListResponse' {} a -> s {httpStatus = a} :: DeleteManagedPrefixListResponse)

instance Core.NFData DeleteManagedPrefixListResponse
