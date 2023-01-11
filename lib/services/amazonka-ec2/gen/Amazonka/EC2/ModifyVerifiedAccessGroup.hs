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
-- Module      : Amazonka.EC2.ModifyVerifiedAccessGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Verified Access group configuration.
module Amazonka.EC2.ModifyVerifiedAccessGroup
  ( -- * Creating a Request
    ModifyVerifiedAccessGroup (..),
    newModifyVerifiedAccessGroup,

    -- * Request Lenses
    modifyVerifiedAccessGroup_clientToken,
    modifyVerifiedAccessGroup_description,
    modifyVerifiedAccessGroup_dryRun,
    modifyVerifiedAccessGroup_verifiedAccessInstanceId,
    modifyVerifiedAccessGroup_verifiedAccessGroupId,

    -- * Destructuring the Response
    ModifyVerifiedAccessGroupResponse (..),
    newModifyVerifiedAccessGroupResponse,

    -- * Response Lenses
    modifyVerifiedAccessGroupResponse_verifiedAccessGroup,
    modifyVerifiedAccessGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVerifiedAccessGroup' smart constructor.
data ModifyVerifiedAccessGroup = ModifyVerifiedAccessGroup'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access group.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyVerifiedAccessGroup_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'modifyVerifiedAccessGroup_description' - A description for the Amazon Web Services Verified Access group.
--
-- 'dryRun', 'modifyVerifiedAccessGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessInstanceId', 'modifyVerifiedAccessGroup_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'verifiedAccessGroupId', 'modifyVerifiedAccessGroup_verifiedAccessGroupId' - The ID of the Amazon Web Services Verified Access group.
newModifyVerifiedAccessGroup ::
  -- | 'verifiedAccessGroupId'
  Prelude.Text ->
  ModifyVerifiedAccessGroup
newModifyVerifiedAccessGroup pVerifiedAccessGroupId_ =
  ModifyVerifiedAccessGroup'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      verifiedAccessInstanceId = Prelude.Nothing,
      verifiedAccessGroupId = pVerifiedAccessGroupId_
    }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyVerifiedAccessGroup_clientToken :: Lens.Lens' ModifyVerifiedAccessGroup (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessGroup_clientToken = Lens.lens (\ModifyVerifiedAccessGroup' {clientToken} -> clientToken) (\s@ModifyVerifiedAccessGroup' {} a -> s {clientToken = a} :: ModifyVerifiedAccessGroup)

-- | A description for the Amazon Web Services Verified Access group.
modifyVerifiedAccessGroup_description :: Lens.Lens' ModifyVerifiedAccessGroup (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessGroup_description = Lens.lens (\ModifyVerifiedAccessGroup' {description} -> description) (\s@ModifyVerifiedAccessGroup' {} a -> s {description = a} :: ModifyVerifiedAccessGroup)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVerifiedAccessGroup_dryRun :: Lens.Lens' ModifyVerifiedAccessGroup (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessGroup_dryRun = Lens.lens (\ModifyVerifiedAccessGroup' {dryRun} -> dryRun) (\s@ModifyVerifiedAccessGroup' {} a -> s {dryRun = a} :: ModifyVerifiedAccessGroup)

-- | The ID of the Amazon Web Services Verified Access instance.
modifyVerifiedAccessGroup_verifiedAccessInstanceId :: Lens.Lens' ModifyVerifiedAccessGroup (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessGroup_verifiedAccessInstanceId = Lens.lens (\ModifyVerifiedAccessGroup' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@ModifyVerifiedAccessGroup' {} a -> s {verifiedAccessInstanceId = a} :: ModifyVerifiedAccessGroup)

-- | The ID of the Amazon Web Services Verified Access group.
modifyVerifiedAccessGroup_verifiedAccessGroupId :: Lens.Lens' ModifyVerifiedAccessGroup Prelude.Text
modifyVerifiedAccessGroup_verifiedAccessGroupId = Lens.lens (\ModifyVerifiedAccessGroup' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@ModifyVerifiedAccessGroup' {} a -> s {verifiedAccessGroupId = a} :: ModifyVerifiedAccessGroup)

instance Core.AWSRequest ModifyVerifiedAccessGroup where
  type
    AWSResponse ModifyVerifiedAccessGroup =
      ModifyVerifiedAccessGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVerifiedAccessGroupResponse'
            Prelude.<$> (x Data..@? "verifiedAccessGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVerifiedAccessGroup where
  hashWithSalt _salt ModifyVerifiedAccessGroup' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` verifiedAccessInstanceId
      `Prelude.hashWithSalt` verifiedAccessGroupId

instance Prelude.NFData ModifyVerifiedAccessGroup where
  rnf ModifyVerifiedAccessGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessInstanceId
      `Prelude.seq` Prelude.rnf verifiedAccessGroupId

instance Data.ToHeaders ModifyVerifiedAccessGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVerifiedAccessGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVerifiedAccessGroup where
  toQuery ModifyVerifiedAccessGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVerifiedAccessGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "VerifiedAccessInstanceId"
          Data.=: verifiedAccessInstanceId,
        "VerifiedAccessGroupId"
          Data.=: verifiedAccessGroupId
      ]

-- | /See:/ 'newModifyVerifiedAccessGroupResponse' smart constructor.
data ModifyVerifiedAccessGroupResponse = ModifyVerifiedAccessGroupResponse'
  { -- | Details of Amazon Web Services Verified Access group.
    verifiedAccessGroup :: Prelude.Maybe VerifiedAccessGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessGroup', 'modifyVerifiedAccessGroupResponse_verifiedAccessGroup' - Details of Amazon Web Services Verified Access group.
--
-- 'httpStatus', 'modifyVerifiedAccessGroupResponse_httpStatus' - The response's http status code.
newModifyVerifiedAccessGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVerifiedAccessGroupResponse
newModifyVerifiedAccessGroupResponse pHttpStatus_ =
  ModifyVerifiedAccessGroupResponse'
    { verifiedAccessGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of Amazon Web Services Verified Access group.
modifyVerifiedAccessGroupResponse_verifiedAccessGroup :: Lens.Lens' ModifyVerifiedAccessGroupResponse (Prelude.Maybe VerifiedAccessGroup)
modifyVerifiedAccessGroupResponse_verifiedAccessGroup = Lens.lens (\ModifyVerifiedAccessGroupResponse' {verifiedAccessGroup} -> verifiedAccessGroup) (\s@ModifyVerifiedAccessGroupResponse' {} a -> s {verifiedAccessGroup = a} :: ModifyVerifiedAccessGroupResponse)

-- | The response's http status code.
modifyVerifiedAccessGroupResponse_httpStatus :: Lens.Lens' ModifyVerifiedAccessGroupResponse Prelude.Int
modifyVerifiedAccessGroupResponse_httpStatus = Lens.lens (\ModifyVerifiedAccessGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyVerifiedAccessGroupResponse' {} a -> s {httpStatus = a} :: ModifyVerifiedAccessGroupResponse)

instance
  Prelude.NFData
    ModifyVerifiedAccessGroupResponse
  where
  rnf ModifyVerifiedAccessGroupResponse' {..} =
    Prelude.rnf verifiedAccessGroup
      `Prelude.seq` Prelude.rnf httpStatus
