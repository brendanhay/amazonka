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
-- Module      : Network.AWS.CloudFront.DeleteKeyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a key group.
--
-- You cannot delete a key group that is referenced in a cache behavior.
-- First update your distributions to remove the key group from all cache
-- behaviors, then delete the key group.
--
-- To delete a key group, you must provide the key group’s identifier and
-- version. To get these values, use @ListKeyGroups@ followed by
-- @GetKeyGroup@ or @GetKeyGroupConfig@.
module Network.AWS.CloudFront.DeleteKeyGroup
  ( -- * Creating a Request
    DeleteKeyGroup (..),
    newDeleteKeyGroup,

    -- * Request Lenses
    deleteKeyGroup_ifMatch,
    deleteKeyGroup_id,

    -- * Destructuring the Response
    DeleteKeyGroupResponse (..),
    newDeleteKeyGroupResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteKeyGroup' smart constructor.
data DeleteKeyGroup = DeleteKeyGroup'
  { -- | The version of the key group that you are deleting. The version is the
    -- key group’s @ETag@ value. To get the @ETag@, use @GetKeyGroup@ or
    -- @GetKeyGroupConfig@.
    ifMatch :: Core.Maybe Core.Text,
    -- | The identifier of the key group that you are deleting. To get the
    -- identifier, use @ListKeyGroups@.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteKeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteKeyGroup_ifMatch' - The version of the key group that you are deleting. The version is the
-- key group’s @ETag@ value. To get the @ETag@, use @GetKeyGroup@ or
-- @GetKeyGroupConfig@.
--
-- 'id', 'deleteKeyGroup_id' - The identifier of the key group that you are deleting. To get the
-- identifier, use @ListKeyGroups@.
newDeleteKeyGroup ::
  -- | 'id'
  Core.Text ->
  DeleteKeyGroup
newDeleteKeyGroup pId_ =
  DeleteKeyGroup' {ifMatch = Core.Nothing, id = pId_}

-- | The version of the key group that you are deleting. The version is the
-- key group’s @ETag@ value. To get the @ETag@, use @GetKeyGroup@ or
-- @GetKeyGroupConfig@.
deleteKeyGroup_ifMatch :: Lens.Lens' DeleteKeyGroup (Core.Maybe Core.Text)
deleteKeyGroup_ifMatch = Lens.lens (\DeleteKeyGroup' {ifMatch} -> ifMatch) (\s@DeleteKeyGroup' {} a -> s {ifMatch = a} :: DeleteKeyGroup)

-- | The identifier of the key group that you are deleting. To get the
-- identifier, use @ListKeyGroups@.
deleteKeyGroup_id :: Lens.Lens' DeleteKeyGroup Core.Text
deleteKeyGroup_id = Lens.lens (\DeleteKeyGroup' {id} -> id) (\s@DeleteKeyGroup' {} a -> s {id = a} :: DeleteKeyGroup)

instance Core.AWSRequest DeleteKeyGroup where
  type
    AWSResponse DeleteKeyGroup =
      DeleteKeyGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteKeyGroupResponse'

instance Core.Hashable DeleteKeyGroup

instance Core.NFData DeleteKeyGroup

instance Core.ToHeaders DeleteKeyGroup where
  toHeaders DeleteKeyGroup' {..} =
    Core.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath DeleteKeyGroup where
  toPath DeleteKeyGroup' {..} =
    Core.mconcat
      ["/2020-05-31/key-group/", Core.toBS id]

instance Core.ToQuery DeleteKeyGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteKeyGroupResponse' smart constructor.
data DeleteKeyGroupResponse = DeleteKeyGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteKeyGroupResponse ::
  DeleteKeyGroupResponse
newDeleteKeyGroupResponse = DeleteKeyGroupResponse'

instance Core.NFData DeleteKeyGroupResponse
