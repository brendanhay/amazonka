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
-- Module      : Network.AWS.DAX.DeleteSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
--
-- You cannot delete a subnet group if it is associated with any DAX
-- clusters.
module Network.AWS.DAX.DeleteSubnetGroup
  ( -- * Creating a Request
    DeleteSubnetGroup (..),
    newDeleteSubnetGroup,

    -- * Request Lenses
    deleteSubnetGroup_subnetGroupName,

    -- * Destructuring the Response
    DeleteSubnetGroupResponse (..),
    newDeleteSubnetGroupResponse,

    -- * Response Lenses
    deleteSubnetGroupResponse_deletionMessage,
    deleteSubnetGroupResponse_httpStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSubnetGroup' smart constructor.
data DeleteSubnetGroup = DeleteSubnetGroup'
  { -- | The name of the subnet group to delete.
    subnetGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroupName', 'deleteSubnetGroup_subnetGroupName' - The name of the subnet group to delete.
newDeleteSubnetGroup ::
  -- | 'subnetGroupName'
  Prelude.Text ->
  DeleteSubnetGroup
newDeleteSubnetGroup pSubnetGroupName_ =
  DeleteSubnetGroup'
    { subnetGroupName =
        pSubnetGroupName_
    }

-- | The name of the subnet group to delete.
deleteSubnetGroup_subnetGroupName :: Lens.Lens' DeleteSubnetGroup Prelude.Text
deleteSubnetGroup_subnetGroupName = Lens.lens (\DeleteSubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@DeleteSubnetGroup' {} a -> s {subnetGroupName = a} :: DeleteSubnetGroup)

instance Prelude.AWSRequest DeleteSubnetGroup where
  type Rs DeleteSubnetGroup = DeleteSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSubnetGroupResponse'
            Prelude.<$> (x Prelude..?> "DeletionMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSubnetGroup

instance Prelude.NFData DeleteSubnetGroup

instance Prelude.ToHeaders DeleteSubnetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDAXV3.DeleteSubnetGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteSubnetGroup where
  toJSON DeleteSubnetGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SubnetGroupName" Prelude..= subnetGroupName)
          ]
      )

instance Prelude.ToPath DeleteSubnetGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSubnetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSubnetGroupResponse' smart constructor.
data DeleteSubnetGroupResponse = DeleteSubnetGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting
    -- the subnet group).
    deletionMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionMessage', 'deleteSubnetGroupResponse_deletionMessage' - A user-specified message for this action (i.e., a reason for deleting
-- the subnet group).
--
-- 'httpStatus', 'deleteSubnetGroupResponse_httpStatus' - The response's http status code.
newDeleteSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSubnetGroupResponse
newDeleteSubnetGroupResponse pHttpStatus_ =
  DeleteSubnetGroupResponse'
    { deletionMessage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-specified message for this action (i.e., a reason for deleting
-- the subnet group).
deleteSubnetGroupResponse_deletionMessage :: Lens.Lens' DeleteSubnetGroupResponse (Prelude.Maybe Prelude.Text)
deleteSubnetGroupResponse_deletionMessage = Lens.lens (\DeleteSubnetGroupResponse' {deletionMessage} -> deletionMessage) (\s@DeleteSubnetGroupResponse' {} a -> s {deletionMessage = a} :: DeleteSubnetGroupResponse)

-- | The response's http status code.
deleteSubnetGroupResponse_httpStatus :: Lens.Lens' DeleteSubnetGroupResponse Prelude.Int
deleteSubnetGroupResponse_httpStatus = Lens.lens (\DeleteSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteSubnetGroupResponse' {} a -> s {httpStatus = a} :: DeleteSubnetGroupResponse)

instance Prelude.NFData DeleteSubnetGroupResponse
