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
-- Module      : Amazonka.EC2.ModifyInstanceCreditSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the credit option for CPU usage on a running or stopped
-- burstable performance instance. The credit options are @standard@ and
-- @unlimited@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.ModifyInstanceCreditSpecification
  ( -- * Creating a Request
    ModifyInstanceCreditSpecification (..),
    newModifyInstanceCreditSpecification,

    -- * Request Lenses
    modifyInstanceCreditSpecification_clientToken,
    modifyInstanceCreditSpecification_dryRun,
    modifyInstanceCreditSpecification_instanceCreditSpecifications,

    -- * Destructuring the Response
    ModifyInstanceCreditSpecificationResponse (..),
    newModifyInstanceCreditSpecificationResponse,

    -- * Response Lenses
    modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyInstanceCreditSpecification' smart constructor.
data ModifyInstanceCreditSpecification = ModifyInstanceCreditSpecification'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Information about the credit option for CPU usage.
    instanceCreditSpecifications :: [InstanceCreditSpecificationRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyInstanceCreditSpecification_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'modifyInstanceCreditSpecification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceCreditSpecifications', 'modifyInstanceCreditSpecification_instanceCreditSpecifications' - Information about the credit option for CPU usage.
newModifyInstanceCreditSpecification ::
  ModifyInstanceCreditSpecification
newModifyInstanceCreditSpecification =
  ModifyInstanceCreditSpecification'
    { clientToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceCreditSpecifications =
        Prelude.mempty
    }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyInstanceCreditSpecification_clientToken :: Lens.Lens' ModifyInstanceCreditSpecification (Prelude.Maybe Prelude.Text)
modifyInstanceCreditSpecification_clientToken = Lens.lens (\ModifyInstanceCreditSpecification' {clientToken} -> clientToken) (\s@ModifyInstanceCreditSpecification' {} a -> s {clientToken = a} :: ModifyInstanceCreditSpecification)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceCreditSpecification_dryRun :: Lens.Lens' ModifyInstanceCreditSpecification (Prelude.Maybe Prelude.Bool)
modifyInstanceCreditSpecification_dryRun = Lens.lens (\ModifyInstanceCreditSpecification' {dryRun} -> dryRun) (\s@ModifyInstanceCreditSpecification' {} a -> s {dryRun = a} :: ModifyInstanceCreditSpecification)

-- | Information about the credit option for CPU usage.
modifyInstanceCreditSpecification_instanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecification [InstanceCreditSpecificationRequest]
modifyInstanceCreditSpecification_instanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecification' {instanceCreditSpecifications} -> instanceCreditSpecifications) (\s@ModifyInstanceCreditSpecification' {} a -> s {instanceCreditSpecifications = a} :: ModifyInstanceCreditSpecification) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    ModifyInstanceCreditSpecification
  where
  type
    AWSResponse ModifyInstanceCreditSpecification =
      ModifyInstanceCreditSpecificationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceCreditSpecificationResponse'
            Prelude.<$> ( x
                            Data..@? "successfulInstanceCreditSpecificationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> ( x
                              Data..@? "unsuccessfulInstanceCreditSpecificationSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Data.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyInstanceCreditSpecification
  where
  hashWithSalt
    _salt
    ModifyInstanceCreditSpecification' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` instanceCreditSpecifications

instance
  Prelude.NFData
    ModifyInstanceCreditSpecification
  where
  rnf ModifyInstanceCreditSpecification' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceCreditSpecifications

instance
  Data.ToHeaders
    ModifyInstanceCreditSpecification
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyInstanceCreditSpecification
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyInstanceCreditSpecification
  where
  toQuery ModifyInstanceCreditSpecification' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyInstanceCreditSpecification" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        Data.toQueryList
          "InstanceCreditSpecification"
          instanceCreditSpecifications
      ]

-- | /See:/ 'newModifyInstanceCreditSpecificationResponse' smart constructor.
data ModifyInstanceCreditSpecificationResponse = ModifyInstanceCreditSpecificationResponse'
  { -- | Information about the instances whose credit option for CPU usage was
    -- successfully modified.
    successfulInstanceCreditSpecifications :: Prelude.Maybe [SuccessfulInstanceCreditSpecificationItem],
    -- | Information about the instances whose credit option for CPU usage was
    -- not modified.
    unsuccessfulInstanceCreditSpecifications :: Prelude.Maybe [UnsuccessfulInstanceCreditSpecificationItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceCreditSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulInstanceCreditSpecifications', 'modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications' - Information about the instances whose credit option for CPU usage was
-- successfully modified.
--
-- 'unsuccessfulInstanceCreditSpecifications', 'modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications' - Information about the instances whose credit option for CPU usage was
-- not modified.
--
-- 'httpStatus', 'modifyInstanceCreditSpecificationResponse_httpStatus' - The response's http status code.
newModifyInstanceCreditSpecificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstanceCreditSpecificationResponse
newModifyInstanceCreditSpecificationResponse
  pHttpStatus_ =
    ModifyInstanceCreditSpecificationResponse'
      { successfulInstanceCreditSpecifications =
          Prelude.Nothing,
        unsuccessfulInstanceCreditSpecifications =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the instances whose credit option for CPU usage was
-- successfully modified.
modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Prelude.Maybe [SuccessfulInstanceCreditSpecificationItem])
modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {successfulInstanceCreditSpecifications} -> successfulInstanceCreditSpecifications) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {successfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the instances whose credit option for CPU usage was
-- not modified.
modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Prelude.Maybe [UnsuccessfulInstanceCreditSpecificationItem])
modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {unsuccessfulInstanceCreditSpecifications} -> unsuccessfulInstanceCreditSpecifications) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {unsuccessfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
modifyInstanceCreditSpecificationResponse_httpStatus :: Lens.Lens' ModifyInstanceCreditSpecificationResponse Prelude.Int
modifyInstanceCreditSpecificationResponse_httpStatus = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {httpStatus = a} :: ModifyInstanceCreditSpecificationResponse)

instance
  Prelude.NFData
    ModifyInstanceCreditSpecificationResponse
  where
  rnf ModifyInstanceCreditSpecificationResponse' {..} =
    Prelude.rnf successfulInstanceCreditSpecifications
      `Prelude.seq` Prelude.rnf unsuccessfulInstanceCreditSpecifications
      `Prelude.seq` Prelude.rnf httpStatus
