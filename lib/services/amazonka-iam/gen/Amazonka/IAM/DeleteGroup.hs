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
-- Module      : Amazonka.IAM.DeleteGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM group. The group must not contain any users or
-- have any attached policies.
module Amazonka.IAM.DeleteGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The name of the IAM group to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteGroup
newDeleteGroup pGroupName_ =
  DeleteGroup' {groupName = pGroupName_}

-- | The name of the IAM group to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteGroup_groupName :: Lens.Lens' DeleteGroup Prelude.Text
deleteGroup_groupName = Lens.lens (\DeleteGroup' {groupName} -> groupName) (\s@DeleteGroup' {} a -> s {groupName = a} :: DeleteGroup)

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeleteGroupResponse'

instance Prelude.Hashable DeleteGroup where
  hashWithSalt _salt DeleteGroup' {..} =
    _salt `Prelude.hashWithSalt` groupName

instance Prelude.NFData DeleteGroup where
  rnf DeleteGroup' {..} = Prelude.rnf groupName

instance Data.ToHeaders DeleteGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGroup where
  toQuery DeleteGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Data.=: groupName
      ]

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGroupResponse ::
  DeleteGroupResponse
newDeleteGroupResponse = DeleteGroupResponse'

instance Prelude.NFData DeleteGroupResponse where
  rnf _ = ()
