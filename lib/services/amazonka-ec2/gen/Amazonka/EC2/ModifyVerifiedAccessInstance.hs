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
-- Module      : Amazonka.EC2.ModifyVerifiedAccessInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of the specified Verified Access instance.
module Amazonka.EC2.ModifyVerifiedAccessInstance
  ( -- * Creating a Request
    ModifyVerifiedAccessInstance (..),
    newModifyVerifiedAccessInstance,

    -- * Request Lenses
    modifyVerifiedAccessInstance_clientToken,
    modifyVerifiedAccessInstance_description,
    modifyVerifiedAccessInstance_dryRun,
    modifyVerifiedAccessInstance_verifiedAccessInstanceId,

    -- * Destructuring the Response
    ModifyVerifiedAccessInstanceResponse (..),
    newModifyVerifiedAccessInstanceResponse,

    -- * Response Lenses
    modifyVerifiedAccessInstanceResponse_verifiedAccessInstance,
    modifyVerifiedAccessInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVerifiedAccessInstance' smart constructor.
data ModifyVerifiedAccessInstance = ModifyVerifiedAccessInstance'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyVerifiedAccessInstance_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'modifyVerifiedAccessInstance_description' - A description for the Amazon Web Services Verified Access instance.
--
-- 'dryRun', 'modifyVerifiedAccessInstance_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessInstanceId', 'modifyVerifiedAccessInstance_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newModifyVerifiedAccessInstance ::
  -- | 'verifiedAccessInstanceId'
  Prelude.Text ->
  ModifyVerifiedAccessInstance
newModifyVerifiedAccessInstance
  pVerifiedAccessInstanceId_ =
    ModifyVerifiedAccessInstance'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        verifiedAccessInstanceId =
          pVerifiedAccessInstanceId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyVerifiedAccessInstance_clientToken :: Lens.Lens' ModifyVerifiedAccessInstance (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessInstance_clientToken = Lens.lens (\ModifyVerifiedAccessInstance' {clientToken} -> clientToken) (\s@ModifyVerifiedAccessInstance' {} a -> s {clientToken = a} :: ModifyVerifiedAccessInstance)

-- | A description for the Amazon Web Services Verified Access instance.
modifyVerifiedAccessInstance_description :: Lens.Lens' ModifyVerifiedAccessInstance (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessInstance_description = Lens.lens (\ModifyVerifiedAccessInstance' {description} -> description) (\s@ModifyVerifiedAccessInstance' {} a -> s {description = a} :: ModifyVerifiedAccessInstance)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVerifiedAccessInstance_dryRun :: Lens.Lens' ModifyVerifiedAccessInstance (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessInstance_dryRun = Lens.lens (\ModifyVerifiedAccessInstance' {dryRun} -> dryRun) (\s@ModifyVerifiedAccessInstance' {} a -> s {dryRun = a} :: ModifyVerifiedAccessInstance)

-- | The ID of the Amazon Web Services Verified Access instance.
modifyVerifiedAccessInstance_verifiedAccessInstanceId :: Lens.Lens' ModifyVerifiedAccessInstance Prelude.Text
modifyVerifiedAccessInstance_verifiedAccessInstanceId = Lens.lens (\ModifyVerifiedAccessInstance' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@ModifyVerifiedAccessInstance' {} a -> s {verifiedAccessInstanceId = a} :: ModifyVerifiedAccessInstance)

instance Core.AWSRequest ModifyVerifiedAccessInstance where
  type
    AWSResponse ModifyVerifiedAccessInstance =
      ModifyVerifiedAccessInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVerifiedAccessInstanceResponse'
            Prelude.<$> (x Data..@? "verifiedAccessInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVerifiedAccessInstance
  where
  hashWithSalt _salt ModifyVerifiedAccessInstance' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` verifiedAccessInstanceId

instance Prelude.NFData ModifyVerifiedAccessInstance where
  rnf ModifyVerifiedAccessInstance' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessInstanceId

instance Data.ToHeaders ModifyVerifiedAccessInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVerifiedAccessInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVerifiedAccessInstance where
  toQuery ModifyVerifiedAccessInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyVerifiedAccessInstance" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "VerifiedAccessInstanceId"
          Data.=: verifiedAccessInstanceId
      ]

-- | /See:/ 'newModifyVerifiedAccessInstanceResponse' smart constructor.
data ModifyVerifiedAccessInstanceResponse = ModifyVerifiedAccessInstanceResponse'
  { -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstance :: Prelude.Maybe VerifiedAccessInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessInstance', 'modifyVerifiedAccessInstanceResponse_verifiedAccessInstance' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'httpStatus', 'modifyVerifiedAccessInstanceResponse_httpStatus' - The response's http status code.
newModifyVerifiedAccessInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVerifiedAccessInstanceResponse
newModifyVerifiedAccessInstanceResponse pHttpStatus_ =
  ModifyVerifiedAccessInstanceResponse'
    { verifiedAccessInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Amazon Web Services Verified Access instance.
modifyVerifiedAccessInstanceResponse_verifiedAccessInstance :: Lens.Lens' ModifyVerifiedAccessInstanceResponse (Prelude.Maybe VerifiedAccessInstance)
modifyVerifiedAccessInstanceResponse_verifiedAccessInstance = Lens.lens (\ModifyVerifiedAccessInstanceResponse' {verifiedAccessInstance} -> verifiedAccessInstance) (\s@ModifyVerifiedAccessInstanceResponse' {} a -> s {verifiedAccessInstance = a} :: ModifyVerifiedAccessInstanceResponse)

-- | The response's http status code.
modifyVerifiedAccessInstanceResponse_httpStatus :: Lens.Lens' ModifyVerifiedAccessInstanceResponse Prelude.Int
modifyVerifiedAccessInstanceResponse_httpStatus = Lens.lens (\ModifyVerifiedAccessInstanceResponse' {httpStatus} -> httpStatus) (\s@ModifyVerifiedAccessInstanceResponse' {} a -> s {httpStatus = a} :: ModifyVerifiedAccessInstanceResponse)

instance
  Prelude.NFData
    ModifyVerifiedAccessInstanceResponse
  where
  rnf ModifyVerifiedAccessInstanceResponse' {..} =
    Prelude.rnf verifiedAccessInstance
      `Prelude.seq` Prelude.rnf httpStatus
