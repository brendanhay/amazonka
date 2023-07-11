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
-- Module      : Amazonka.EC2.DeleteVerifiedAccessGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an Amazon Web Services Verified Access group.
module Amazonka.EC2.DeleteVerifiedAccessGroup
  ( -- * Creating a Request
    DeleteVerifiedAccessGroup (..),
    newDeleteVerifiedAccessGroup,

    -- * Request Lenses
    deleteVerifiedAccessGroup_clientToken,
    deleteVerifiedAccessGroup_dryRun,
    deleteVerifiedAccessGroup_verifiedAccessGroupId,

    -- * Destructuring the Response
    DeleteVerifiedAccessGroupResponse (..),
    newDeleteVerifiedAccessGroupResponse,

    -- * Response Lenses
    deleteVerifiedAccessGroupResponse_verifiedAccessGroup,
    deleteVerifiedAccessGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVerifiedAccessGroup' smart constructor.
data DeleteVerifiedAccessGroup = DeleteVerifiedAccessGroup'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteVerifiedAccessGroup_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'deleteVerifiedAccessGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessGroupId', 'deleteVerifiedAccessGroup_verifiedAccessGroupId' - The ID of the Amazon Web Services Verified Access group.
newDeleteVerifiedAccessGroup ::
  -- | 'verifiedAccessGroupId'
  Prelude.Text ->
  DeleteVerifiedAccessGroup
newDeleteVerifiedAccessGroup pVerifiedAccessGroupId_ =
  DeleteVerifiedAccessGroup'
    { clientToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      verifiedAccessGroupId = pVerifiedAccessGroupId_
    }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
deleteVerifiedAccessGroup_clientToken :: Lens.Lens' DeleteVerifiedAccessGroup (Prelude.Maybe Prelude.Text)
deleteVerifiedAccessGroup_clientToken = Lens.lens (\DeleteVerifiedAccessGroup' {clientToken} -> clientToken) (\s@DeleteVerifiedAccessGroup' {} a -> s {clientToken = a} :: DeleteVerifiedAccessGroup)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVerifiedAccessGroup_dryRun :: Lens.Lens' DeleteVerifiedAccessGroup (Prelude.Maybe Prelude.Bool)
deleteVerifiedAccessGroup_dryRun = Lens.lens (\DeleteVerifiedAccessGroup' {dryRun} -> dryRun) (\s@DeleteVerifiedAccessGroup' {} a -> s {dryRun = a} :: DeleteVerifiedAccessGroup)

-- | The ID of the Amazon Web Services Verified Access group.
deleteVerifiedAccessGroup_verifiedAccessGroupId :: Lens.Lens' DeleteVerifiedAccessGroup Prelude.Text
deleteVerifiedAccessGroup_verifiedAccessGroupId = Lens.lens (\DeleteVerifiedAccessGroup' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@DeleteVerifiedAccessGroup' {} a -> s {verifiedAccessGroupId = a} :: DeleteVerifiedAccessGroup)

instance Core.AWSRequest DeleteVerifiedAccessGroup where
  type
    AWSResponse DeleteVerifiedAccessGroup =
      DeleteVerifiedAccessGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVerifiedAccessGroupResponse'
            Prelude.<$> (x Data..@? "verifiedAccessGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVerifiedAccessGroup where
  hashWithSalt _salt DeleteVerifiedAccessGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` verifiedAccessGroupId

instance Prelude.NFData DeleteVerifiedAccessGroup where
  rnf DeleteVerifiedAccessGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessGroupId

instance Data.ToHeaders DeleteVerifiedAccessGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVerifiedAccessGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVerifiedAccessGroup where
  toQuery DeleteVerifiedAccessGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteVerifiedAccessGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "VerifiedAccessGroupId"
          Data.=: verifiedAccessGroupId
      ]

-- | /See:/ 'newDeleteVerifiedAccessGroupResponse' smart constructor.
data DeleteVerifiedAccessGroupResponse = DeleteVerifiedAccessGroupResponse'
  { -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroup :: Prelude.Maybe VerifiedAccessGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessGroup', 'deleteVerifiedAccessGroupResponse_verifiedAccessGroup' - The ID of the Amazon Web Services Verified Access group.
--
-- 'httpStatus', 'deleteVerifiedAccessGroupResponse_httpStatus' - The response's http status code.
newDeleteVerifiedAccessGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVerifiedAccessGroupResponse
newDeleteVerifiedAccessGroupResponse pHttpStatus_ =
  DeleteVerifiedAccessGroupResponse'
    { verifiedAccessGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Amazon Web Services Verified Access group.
deleteVerifiedAccessGroupResponse_verifiedAccessGroup :: Lens.Lens' DeleteVerifiedAccessGroupResponse (Prelude.Maybe VerifiedAccessGroup)
deleteVerifiedAccessGroupResponse_verifiedAccessGroup = Lens.lens (\DeleteVerifiedAccessGroupResponse' {verifiedAccessGroup} -> verifiedAccessGroup) (\s@DeleteVerifiedAccessGroupResponse' {} a -> s {verifiedAccessGroup = a} :: DeleteVerifiedAccessGroupResponse)

-- | The response's http status code.
deleteVerifiedAccessGroupResponse_httpStatus :: Lens.Lens' DeleteVerifiedAccessGroupResponse Prelude.Int
deleteVerifiedAccessGroupResponse_httpStatus = Lens.lens (\DeleteVerifiedAccessGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteVerifiedAccessGroupResponse' {} a -> s {httpStatus = a} :: DeleteVerifiedAccessGroupResponse)

instance
  Prelude.NFData
    DeleteVerifiedAccessGroupResponse
  where
  rnf DeleteVerifiedAccessGroupResponse' {..} =
    Prelude.rnf verifiedAccessGroup
      `Prelude.seq` Prelude.rnf httpStatus
