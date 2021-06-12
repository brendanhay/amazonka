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
-- Module      : Network.AWS.EC2.ModifyManagedPrefixList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified managed prefix list.
--
-- Adding or removing entries in a prefix list creates a new version of the
-- prefix list. Changing the name of the prefix list does not affect the
-- version.
--
-- If you specify a current version number that does not match the true
-- current version number, the request fails.
module Network.AWS.EC2.ModifyManagedPrefixList
  ( -- * Creating a Request
    ModifyManagedPrefixList (..),
    newModifyManagedPrefixList,

    -- * Request Lenses
    modifyManagedPrefixList_removeEntries,
    modifyManagedPrefixList_dryRun,
    modifyManagedPrefixList_currentVersion,
    modifyManagedPrefixList_prefixListName,
    modifyManagedPrefixList_addEntries,
    modifyManagedPrefixList_prefixListId,

    -- * Destructuring the Response
    ModifyManagedPrefixListResponse (..),
    newModifyManagedPrefixListResponse,

    -- * Response Lenses
    modifyManagedPrefixListResponse_prefixList,
    modifyManagedPrefixListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyManagedPrefixList' smart constructor.
data ModifyManagedPrefixList = ModifyManagedPrefixList'
  { -- | One or more entries to remove from the prefix list.
    removeEntries :: Core.Maybe [RemovePrefixListEntry],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The current version of the prefix list.
    currentVersion :: Core.Maybe Core.Integer,
    -- | A name for the prefix list.
    prefixListName :: Core.Maybe Core.Text,
    -- | One or more entries to add to the prefix list.
    addEntries :: Core.Maybe [AddPrefixListEntry],
    -- | The ID of the prefix list.
    prefixListId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyManagedPrefixList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeEntries', 'modifyManagedPrefixList_removeEntries' - One or more entries to remove from the prefix list.
--
-- 'dryRun', 'modifyManagedPrefixList_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'currentVersion', 'modifyManagedPrefixList_currentVersion' - The current version of the prefix list.
--
-- 'prefixListName', 'modifyManagedPrefixList_prefixListName' - A name for the prefix list.
--
-- 'addEntries', 'modifyManagedPrefixList_addEntries' - One or more entries to add to the prefix list.
--
-- 'prefixListId', 'modifyManagedPrefixList_prefixListId' - The ID of the prefix list.
newModifyManagedPrefixList ::
  -- | 'prefixListId'
  Core.Text ->
  ModifyManagedPrefixList
newModifyManagedPrefixList pPrefixListId_ =
  ModifyManagedPrefixList'
    { removeEntries =
        Core.Nothing,
      dryRun = Core.Nothing,
      currentVersion = Core.Nothing,
      prefixListName = Core.Nothing,
      addEntries = Core.Nothing,
      prefixListId = pPrefixListId_
    }

-- | One or more entries to remove from the prefix list.
modifyManagedPrefixList_removeEntries :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe [RemovePrefixListEntry])
modifyManagedPrefixList_removeEntries = Lens.lens (\ModifyManagedPrefixList' {removeEntries} -> removeEntries) (\s@ModifyManagedPrefixList' {} a -> s {removeEntries = a} :: ModifyManagedPrefixList) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyManagedPrefixList_dryRun :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Bool)
modifyManagedPrefixList_dryRun = Lens.lens (\ModifyManagedPrefixList' {dryRun} -> dryRun) (\s@ModifyManagedPrefixList' {} a -> s {dryRun = a} :: ModifyManagedPrefixList)

-- | The current version of the prefix list.
modifyManagedPrefixList_currentVersion :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Integer)
modifyManagedPrefixList_currentVersion = Lens.lens (\ModifyManagedPrefixList' {currentVersion} -> currentVersion) (\s@ModifyManagedPrefixList' {} a -> s {currentVersion = a} :: ModifyManagedPrefixList)

-- | A name for the prefix list.
modifyManagedPrefixList_prefixListName :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Text)
modifyManagedPrefixList_prefixListName = Lens.lens (\ModifyManagedPrefixList' {prefixListName} -> prefixListName) (\s@ModifyManagedPrefixList' {} a -> s {prefixListName = a} :: ModifyManagedPrefixList)

-- | One or more entries to add to the prefix list.
modifyManagedPrefixList_addEntries :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe [AddPrefixListEntry])
modifyManagedPrefixList_addEntries = Lens.lens (\ModifyManagedPrefixList' {addEntries} -> addEntries) (\s@ModifyManagedPrefixList' {} a -> s {addEntries = a} :: ModifyManagedPrefixList) Core.. Lens.mapping Lens._Coerce

-- | The ID of the prefix list.
modifyManagedPrefixList_prefixListId :: Lens.Lens' ModifyManagedPrefixList Core.Text
modifyManagedPrefixList_prefixListId = Lens.lens (\ModifyManagedPrefixList' {prefixListId} -> prefixListId) (\s@ModifyManagedPrefixList' {} a -> s {prefixListId = a} :: ModifyManagedPrefixList)

instance Core.AWSRequest ModifyManagedPrefixList where
  type
    AWSResponse ModifyManagedPrefixList =
      ModifyManagedPrefixListResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyManagedPrefixListResponse'
            Core.<$> (x Core..@? "prefixList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyManagedPrefixList

instance Core.NFData ModifyManagedPrefixList

instance Core.ToHeaders ModifyManagedPrefixList where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyManagedPrefixList where
  toPath = Core.const "/"

instance Core.ToQuery ModifyManagedPrefixList where
  toQuery ModifyManagedPrefixList' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyManagedPrefixList" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "RemoveEntry"
              Core.<$> removeEntries
          ),
        "DryRun" Core.=: dryRun,
        "CurrentVersion" Core.=: currentVersion,
        "PrefixListName" Core.=: prefixListName,
        Core.toQuery
          (Core.toQueryList "AddEntry" Core.<$> addEntries),
        "PrefixListId" Core.=: prefixListId
      ]

-- | /See:/ 'newModifyManagedPrefixListResponse' smart constructor.
data ModifyManagedPrefixListResponse = ModifyManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Core.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyManagedPrefixListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixList', 'modifyManagedPrefixListResponse_prefixList' - Information about the prefix list.
--
-- 'httpStatus', 'modifyManagedPrefixListResponse_httpStatus' - The response's http status code.
newModifyManagedPrefixListResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyManagedPrefixListResponse
newModifyManagedPrefixListResponse pHttpStatus_ =
  ModifyManagedPrefixListResponse'
    { prefixList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prefix list.
modifyManagedPrefixListResponse_prefixList :: Lens.Lens' ModifyManagedPrefixListResponse (Core.Maybe ManagedPrefixList)
modifyManagedPrefixListResponse_prefixList = Lens.lens (\ModifyManagedPrefixListResponse' {prefixList} -> prefixList) (\s@ModifyManagedPrefixListResponse' {} a -> s {prefixList = a} :: ModifyManagedPrefixListResponse)

-- | The response's http status code.
modifyManagedPrefixListResponse_httpStatus :: Lens.Lens' ModifyManagedPrefixListResponse Core.Int
modifyManagedPrefixListResponse_httpStatus = Lens.lens (\ModifyManagedPrefixListResponse' {httpStatus} -> httpStatus) (\s@ModifyManagedPrefixListResponse' {} a -> s {httpStatus = a} :: ModifyManagedPrefixListResponse)

instance Core.NFData ModifyManagedPrefixListResponse
