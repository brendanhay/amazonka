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
-- Module      : Amazonka.EC2.ModifyManagedPrefixList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.ModifyManagedPrefixList
  ( -- * Creating a Request
    ModifyManagedPrefixList (..),
    newModifyManagedPrefixList,

    -- * Request Lenses
    modifyManagedPrefixList_addEntries,
    modifyManagedPrefixList_currentVersion,
    modifyManagedPrefixList_dryRun,
    modifyManagedPrefixList_maxEntries,
    modifyManagedPrefixList_prefixListName,
    modifyManagedPrefixList_removeEntries,
    modifyManagedPrefixList_prefixListId,

    -- * Destructuring the Response
    ModifyManagedPrefixListResponse (..),
    newModifyManagedPrefixListResponse,

    -- * Response Lenses
    modifyManagedPrefixListResponse_prefixList,
    modifyManagedPrefixListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyManagedPrefixList' smart constructor.
data ModifyManagedPrefixList = ModifyManagedPrefixList'
  { -- | One or more entries to add to the prefix list.
    addEntries :: Prelude.Maybe [AddPrefixListEntry],
    -- | The current version of the prefix list.
    currentVersion :: Prelude.Maybe Prelude.Integer,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of entries for the prefix list. You cannot modify the
    -- entries of a prefix list and modify the size of a prefix list at the
    -- same time.
    --
    -- If any of the resources that reference the prefix list cannot support
    -- the new maximum size, the modify operation fails. Check the state
    -- message for the IDs of the first ten resources that do not support the
    -- new maximum size.
    maxEntries :: Prelude.Maybe Prelude.Int,
    -- | A name for the prefix list.
    prefixListName :: Prelude.Maybe Prelude.Text,
    -- | One or more entries to remove from the prefix list.
    removeEntries :: Prelude.Maybe [RemovePrefixListEntry],
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyManagedPrefixList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addEntries', 'modifyManagedPrefixList_addEntries' - One or more entries to add to the prefix list.
--
-- 'currentVersion', 'modifyManagedPrefixList_currentVersion' - The current version of the prefix list.
--
-- 'dryRun', 'modifyManagedPrefixList_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxEntries', 'modifyManagedPrefixList_maxEntries' - The maximum number of entries for the prefix list. You cannot modify the
-- entries of a prefix list and modify the size of a prefix list at the
-- same time.
--
-- If any of the resources that reference the prefix list cannot support
-- the new maximum size, the modify operation fails. Check the state
-- message for the IDs of the first ten resources that do not support the
-- new maximum size.
--
-- 'prefixListName', 'modifyManagedPrefixList_prefixListName' - A name for the prefix list.
--
-- 'removeEntries', 'modifyManagedPrefixList_removeEntries' - One or more entries to remove from the prefix list.
--
-- 'prefixListId', 'modifyManagedPrefixList_prefixListId' - The ID of the prefix list.
newModifyManagedPrefixList ::
  -- | 'prefixListId'
  Prelude.Text ->
  ModifyManagedPrefixList
newModifyManagedPrefixList pPrefixListId_ =
  ModifyManagedPrefixList'
    { addEntries =
        Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxEntries = Prelude.Nothing,
      prefixListName = Prelude.Nothing,
      removeEntries = Prelude.Nothing,
      prefixListId = pPrefixListId_
    }

-- | One or more entries to add to the prefix list.
modifyManagedPrefixList_addEntries :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe [AddPrefixListEntry])
modifyManagedPrefixList_addEntries = Lens.lens (\ModifyManagedPrefixList' {addEntries} -> addEntries) (\s@ModifyManagedPrefixList' {} a -> s {addEntries = a} :: ModifyManagedPrefixList) Prelude.. Lens.mapping Lens.coerced

-- | The current version of the prefix list.
modifyManagedPrefixList_currentVersion :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Integer)
modifyManagedPrefixList_currentVersion = Lens.lens (\ModifyManagedPrefixList' {currentVersion} -> currentVersion) (\s@ModifyManagedPrefixList' {} a -> s {currentVersion = a} :: ModifyManagedPrefixList)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyManagedPrefixList_dryRun :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Bool)
modifyManagedPrefixList_dryRun = Lens.lens (\ModifyManagedPrefixList' {dryRun} -> dryRun) (\s@ModifyManagedPrefixList' {} a -> s {dryRun = a} :: ModifyManagedPrefixList)

-- | The maximum number of entries for the prefix list. You cannot modify the
-- entries of a prefix list and modify the size of a prefix list at the
-- same time.
--
-- If any of the resources that reference the prefix list cannot support
-- the new maximum size, the modify operation fails. Check the state
-- message for the IDs of the first ten resources that do not support the
-- new maximum size.
modifyManagedPrefixList_maxEntries :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Int)
modifyManagedPrefixList_maxEntries = Lens.lens (\ModifyManagedPrefixList' {maxEntries} -> maxEntries) (\s@ModifyManagedPrefixList' {} a -> s {maxEntries = a} :: ModifyManagedPrefixList)

-- | A name for the prefix list.
modifyManagedPrefixList_prefixListName :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Text)
modifyManagedPrefixList_prefixListName = Lens.lens (\ModifyManagedPrefixList' {prefixListName} -> prefixListName) (\s@ModifyManagedPrefixList' {} a -> s {prefixListName = a} :: ModifyManagedPrefixList)

-- | One or more entries to remove from the prefix list.
modifyManagedPrefixList_removeEntries :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe [RemovePrefixListEntry])
modifyManagedPrefixList_removeEntries = Lens.lens (\ModifyManagedPrefixList' {removeEntries} -> removeEntries) (\s@ModifyManagedPrefixList' {} a -> s {removeEntries = a} :: ModifyManagedPrefixList) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the prefix list.
modifyManagedPrefixList_prefixListId :: Lens.Lens' ModifyManagedPrefixList Prelude.Text
modifyManagedPrefixList_prefixListId = Lens.lens (\ModifyManagedPrefixList' {prefixListId} -> prefixListId) (\s@ModifyManagedPrefixList' {} a -> s {prefixListId = a} :: ModifyManagedPrefixList)

instance Core.AWSRequest ModifyManagedPrefixList where
  type
    AWSResponse ModifyManagedPrefixList =
      ModifyManagedPrefixListResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyManagedPrefixListResponse'
            Prelude.<$> (x Data..@? "prefixList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyManagedPrefixList where
  hashWithSalt _salt ModifyManagedPrefixList' {..} =
    _salt
      `Prelude.hashWithSalt` addEntries
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxEntries
      `Prelude.hashWithSalt` prefixListName
      `Prelude.hashWithSalt` removeEntries
      `Prelude.hashWithSalt` prefixListId

instance Prelude.NFData ModifyManagedPrefixList where
  rnf ModifyManagedPrefixList' {..} =
    Prelude.rnf addEntries
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxEntries
      `Prelude.seq` Prelude.rnf prefixListName
      `Prelude.seq` Prelude.rnf removeEntries
      `Prelude.seq` Prelude.rnf prefixListId

instance Data.ToHeaders ModifyManagedPrefixList where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyManagedPrefixList where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyManagedPrefixList where
  toQuery ModifyManagedPrefixList' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyManagedPrefixList" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "AddEntry" Prelude.<$> addEntries),
        "CurrentVersion" Data.=: currentVersion,
        "DryRun" Data.=: dryRun,
        "MaxEntries" Data.=: maxEntries,
        "PrefixListName" Data.=: prefixListName,
        Data.toQuery
          ( Data.toQueryList "RemoveEntry"
              Prelude.<$> removeEntries
          ),
        "PrefixListId" Data.=: prefixListId
      ]

-- | /See:/ 'newModifyManagedPrefixListResponse' smart constructor.
data ModifyManagedPrefixListResponse = ModifyManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Prelude.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyManagedPrefixListResponse
newModifyManagedPrefixListResponse pHttpStatus_ =
  ModifyManagedPrefixListResponse'
    { prefixList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prefix list.
modifyManagedPrefixListResponse_prefixList :: Lens.Lens' ModifyManagedPrefixListResponse (Prelude.Maybe ManagedPrefixList)
modifyManagedPrefixListResponse_prefixList = Lens.lens (\ModifyManagedPrefixListResponse' {prefixList} -> prefixList) (\s@ModifyManagedPrefixListResponse' {} a -> s {prefixList = a} :: ModifyManagedPrefixListResponse)

-- | The response's http status code.
modifyManagedPrefixListResponse_httpStatus :: Lens.Lens' ModifyManagedPrefixListResponse Prelude.Int
modifyManagedPrefixListResponse_httpStatus = Lens.lens (\ModifyManagedPrefixListResponse' {httpStatus} -> httpStatus) (\s@ModifyManagedPrefixListResponse' {} a -> s {httpStatus = a} :: ModifyManagedPrefixListResponse)

instance
  Prelude.NFData
    ModifyManagedPrefixListResponse
  where
  rnf ModifyManagedPrefixListResponse' {..} =
    Prelude.rnf prefixList
      `Prelude.seq` Prelude.rnf httpStatus
