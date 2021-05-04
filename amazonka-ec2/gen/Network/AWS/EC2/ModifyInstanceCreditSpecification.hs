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
-- Module      : Network.AWS.EC2.ModifyInstanceCreditSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.ModifyInstanceCreditSpecification
  ( -- * Creating a Request
    ModifyInstanceCreditSpecification (..),
    newModifyInstanceCreditSpecification,

    -- * Request Lenses
    modifyInstanceCreditSpecification_dryRun,
    modifyInstanceCreditSpecification_clientToken,
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstanceCreditSpecification' smart constructor.
data ModifyInstanceCreditSpecification = ModifyInstanceCreditSpecification'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the credit option for CPU usage.
    instanceCreditSpecifications :: [InstanceCreditSpecificationRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyInstanceCreditSpecification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'modifyInstanceCreditSpecification_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'instanceCreditSpecifications', 'modifyInstanceCreditSpecification_instanceCreditSpecifications' - Information about the credit option for CPU usage.
newModifyInstanceCreditSpecification ::
  ModifyInstanceCreditSpecification
newModifyInstanceCreditSpecification =
  ModifyInstanceCreditSpecification'
    { dryRun =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      instanceCreditSpecifications =
        Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceCreditSpecification_dryRun :: Lens.Lens' ModifyInstanceCreditSpecification (Prelude.Maybe Prelude.Bool)
modifyInstanceCreditSpecification_dryRun = Lens.lens (\ModifyInstanceCreditSpecification' {dryRun} -> dryRun) (\s@ModifyInstanceCreditSpecification' {} a -> s {dryRun = a} :: ModifyInstanceCreditSpecification)

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyInstanceCreditSpecification_clientToken :: Lens.Lens' ModifyInstanceCreditSpecification (Prelude.Maybe Prelude.Text)
modifyInstanceCreditSpecification_clientToken = Lens.lens (\ModifyInstanceCreditSpecification' {clientToken} -> clientToken) (\s@ModifyInstanceCreditSpecification' {} a -> s {clientToken = a} :: ModifyInstanceCreditSpecification)

-- | Information about the credit option for CPU usage.
modifyInstanceCreditSpecification_instanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecification [InstanceCreditSpecificationRequest]
modifyInstanceCreditSpecification_instanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecification' {instanceCreditSpecifications} -> instanceCreditSpecifications) (\s@ModifyInstanceCreditSpecification' {} a -> s {instanceCreditSpecifications = a} :: ModifyInstanceCreditSpecification) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    ModifyInstanceCreditSpecification
  where
  type
    Rs ModifyInstanceCreditSpecification =
      ModifyInstanceCreditSpecificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceCreditSpecificationResponse'
            Prelude.<$> ( x
                            Prelude..@? "successfulInstanceCreditSpecificationSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
              Prelude.<*> ( x
                              Prelude..@? "unsuccessfulInstanceCreditSpecificationSet"
                              Prelude..!@ Prelude.mempty
                              Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyInstanceCreditSpecification

instance
  Prelude.NFData
    ModifyInstanceCreditSpecification

instance
  Prelude.ToHeaders
    ModifyInstanceCreditSpecification
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ModifyInstanceCreditSpecification
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ModifyInstanceCreditSpecification
  where
  toQuery ModifyInstanceCreditSpecification' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyInstanceCreditSpecification" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "ClientToken" Prelude.=: clientToken,
        Prelude.toQueryList
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {successfulInstanceCreditSpecifications} -> successfulInstanceCreditSpecifications) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {successfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the instances whose credit option for CPU usage was
-- not modified.
modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Prelude.Maybe [UnsuccessfulInstanceCreditSpecificationItem])
modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {unsuccessfulInstanceCreditSpecifications} -> unsuccessfulInstanceCreditSpecifications) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {unsuccessfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
modifyInstanceCreditSpecificationResponse_httpStatus :: Lens.Lens' ModifyInstanceCreditSpecificationResponse Prelude.Int
modifyInstanceCreditSpecificationResponse_httpStatus = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {httpStatus = a} :: ModifyInstanceCreditSpecificationResponse)

instance
  Prelude.NFData
    ModifyInstanceCreditSpecificationResponse
