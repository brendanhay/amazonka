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
-- Module      : Network.AWS.IoT.DeleteBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the billing group.
module Network.AWS.IoT.DeleteBillingGroup
  ( -- * Creating a Request
    DeleteBillingGroup (..),
    newDeleteBillingGroup,

    -- * Request Lenses
    deleteBillingGroup_expectedVersion,
    deleteBillingGroup_billingGroupName,

    -- * Destructuring the Response
    DeleteBillingGroupResponse (..),
    newDeleteBillingGroupResponse,

    -- * Response Lenses
    deleteBillingGroupResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBillingGroup' smart constructor.
data DeleteBillingGroup = DeleteBillingGroup'
  { -- | The expected version of the billing group. If the version of the billing
    -- group does not match the expected version specified in the request, the
    -- @DeleteBillingGroup@ request is rejected with a
    -- @VersionConflictException@.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the billing group.
    billingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'deleteBillingGroup_expectedVersion' - The expected version of the billing group. If the version of the billing
-- group does not match the expected version specified in the request, the
-- @DeleteBillingGroup@ request is rejected with a
-- @VersionConflictException@.
--
-- 'billingGroupName', 'deleteBillingGroup_billingGroupName' - The name of the billing group.
newDeleteBillingGroup ::
  -- | 'billingGroupName'
  Prelude.Text ->
  DeleteBillingGroup
newDeleteBillingGroup pBillingGroupName_ =
  DeleteBillingGroup'
    { expectedVersion =
        Prelude.Nothing,
      billingGroupName = pBillingGroupName_
    }

-- | The expected version of the billing group. If the version of the billing
-- group does not match the expected version specified in the request, the
-- @DeleteBillingGroup@ request is rejected with a
-- @VersionConflictException@.
deleteBillingGroup_expectedVersion :: Lens.Lens' DeleteBillingGroup (Prelude.Maybe Prelude.Integer)
deleteBillingGroup_expectedVersion = Lens.lens (\DeleteBillingGroup' {expectedVersion} -> expectedVersion) (\s@DeleteBillingGroup' {} a -> s {expectedVersion = a} :: DeleteBillingGroup)

-- | The name of the billing group.
deleteBillingGroup_billingGroupName :: Lens.Lens' DeleteBillingGroup Prelude.Text
deleteBillingGroup_billingGroupName = Lens.lens (\DeleteBillingGroup' {billingGroupName} -> billingGroupName) (\s@DeleteBillingGroup' {} a -> s {billingGroupName = a} :: DeleteBillingGroup)

instance Prelude.AWSRequest DeleteBillingGroup where
  type
    Rs DeleteBillingGroup =
      DeleteBillingGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBillingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBillingGroup

instance Prelude.NFData DeleteBillingGroup

instance Prelude.ToHeaders DeleteBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteBillingGroup where
  toPath DeleteBillingGroup' {..} =
    Prelude.mconcat
      ["/billing-groups/", Prelude.toBS billingGroupName]

instance Prelude.ToQuery DeleteBillingGroup where
  toQuery DeleteBillingGroup' {..} =
    Prelude.mconcat
      ["expectedVersion" Prelude.=: expectedVersion]

-- | /See:/ 'newDeleteBillingGroupResponse' smart constructor.
data DeleteBillingGroupResponse = DeleteBillingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBillingGroupResponse_httpStatus' - The response's http status code.
newDeleteBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBillingGroupResponse
newDeleteBillingGroupResponse pHttpStatus_ =
  DeleteBillingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteBillingGroupResponse_httpStatus :: Lens.Lens' DeleteBillingGroupResponse Prelude.Int
deleteBillingGroupResponse_httpStatus = Lens.lens (\DeleteBillingGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteBillingGroupResponse' {} a -> s {httpStatus = a} :: DeleteBillingGroupResponse)

instance Prelude.NFData DeleteBillingGroupResponse
