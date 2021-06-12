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
-- Module      : Network.AWS.IAM.DeleteGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM group. The group must not contain any users or
-- have any attached policies.
module Network.AWS.IAM.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_groupName,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The name of the IAM group to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'deleteGroup_groupName' - The name of the IAM group to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteGroup ::
  -- | 'groupName'
  Core.Text ->
  DeleteGroup
newDeleteGroup pGroupName_ =
  DeleteGroup' {groupName = pGroupName_}

-- | The name of the IAM group to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteGroup_groupName :: Lens.Lens' DeleteGroup Core.Text
deleteGroup_groupName = Lens.lens (\DeleteGroup' {groupName} -> groupName) (\s@DeleteGroup' {} a -> s {groupName = a} :: DeleteGroup)

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteGroupResponse'

instance Core.Hashable DeleteGroup

instance Core.NFData DeleteGroup

instance Core.ToHeaders DeleteGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteGroup where
  toQuery DeleteGroup' {..} =
    Core.mconcat
      [ "Action" Core.=: ("DeleteGroup" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "GroupName" Core.=: groupName
      ]

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGroupResponse ::
  DeleteGroupResponse
newDeleteGroupResponse = DeleteGroupResponse'

instance Core.NFData DeleteGroupResponse
