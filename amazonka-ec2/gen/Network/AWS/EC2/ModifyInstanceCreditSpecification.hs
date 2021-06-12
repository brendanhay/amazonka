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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstanceCreditSpecification' smart constructor.
data ModifyInstanceCreditSpecification = ModifyInstanceCreditSpecification'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | Information about the credit option for CPU usage.
    instanceCreditSpecifications :: [InstanceCreditSpecificationRequest]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      clientToken = Core.Nothing,
      instanceCreditSpecifications =
        Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceCreditSpecification_dryRun :: Lens.Lens' ModifyInstanceCreditSpecification (Core.Maybe Core.Bool)
modifyInstanceCreditSpecification_dryRun = Lens.lens (\ModifyInstanceCreditSpecification' {dryRun} -> dryRun) (\s@ModifyInstanceCreditSpecification' {} a -> s {dryRun = a} :: ModifyInstanceCreditSpecification)

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyInstanceCreditSpecification_clientToken :: Lens.Lens' ModifyInstanceCreditSpecification (Core.Maybe Core.Text)
modifyInstanceCreditSpecification_clientToken = Lens.lens (\ModifyInstanceCreditSpecification' {clientToken} -> clientToken) (\s@ModifyInstanceCreditSpecification' {} a -> s {clientToken = a} :: ModifyInstanceCreditSpecification)

-- | Information about the credit option for CPU usage.
modifyInstanceCreditSpecification_instanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecification [InstanceCreditSpecificationRequest]
modifyInstanceCreditSpecification_instanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecification' {instanceCreditSpecifications} -> instanceCreditSpecifications) (\s@ModifyInstanceCreditSpecification' {} a -> s {instanceCreditSpecifications = a} :: ModifyInstanceCreditSpecification) Core.. Lens._Coerce

instance
  Core.AWSRequest
    ModifyInstanceCreditSpecification
  where
  type
    AWSResponse ModifyInstanceCreditSpecification =
      ModifyInstanceCreditSpecificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceCreditSpecificationResponse'
            Core.<$> ( x
                         Core..@? "successfulInstanceCreditSpecificationSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> ( x
                         Core..@? "unsuccessfulInstanceCreditSpecificationSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ModifyInstanceCreditSpecification

instance
  Core.NFData
    ModifyInstanceCreditSpecification

instance
  Core.ToHeaders
    ModifyInstanceCreditSpecification
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ModifyInstanceCreditSpecification
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ModifyInstanceCreditSpecification
  where
  toQuery ModifyInstanceCreditSpecification' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ModifyInstanceCreditSpecification" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ClientToken" Core.=: clientToken,
        Core.toQueryList
          "InstanceCreditSpecification"
          instanceCreditSpecifications
      ]

-- | /See:/ 'newModifyInstanceCreditSpecificationResponse' smart constructor.
data ModifyInstanceCreditSpecificationResponse = ModifyInstanceCreditSpecificationResponse'
  { -- | Information about the instances whose credit option for CPU usage was
    -- successfully modified.
    successfulInstanceCreditSpecifications :: Core.Maybe [SuccessfulInstanceCreditSpecificationItem],
    -- | Information about the instances whose credit option for CPU usage was
    -- not modified.
    unsuccessfulInstanceCreditSpecifications :: Core.Maybe [UnsuccessfulInstanceCreditSpecificationItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyInstanceCreditSpecificationResponse
newModifyInstanceCreditSpecificationResponse
  pHttpStatus_ =
    ModifyInstanceCreditSpecificationResponse'
      { successfulInstanceCreditSpecifications =
          Core.Nothing,
        unsuccessfulInstanceCreditSpecifications =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the instances whose credit option for CPU usage was
-- successfully modified.
modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Core.Maybe [SuccessfulInstanceCreditSpecificationItem])
modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {successfulInstanceCreditSpecifications} -> successfulInstanceCreditSpecifications) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {successfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the instances whose credit option for CPU usage was
-- not modified.
modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Core.Maybe [UnsuccessfulInstanceCreditSpecificationItem])
modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {unsuccessfulInstanceCreditSpecifications} -> unsuccessfulInstanceCreditSpecifications) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {unsuccessfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
modifyInstanceCreditSpecificationResponse_httpStatus :: Lens.Lens' ModifyInstanceCreditSpecificationResponse Core.Int
modifyInstanceCreditSpecificationResponse_httpStatus = Lens.lens (\ModifyInstanceCreditSpecificationResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceCreditSpecificationResponse' {} a -> s {httpStatus = a} :: ModifyInstanceCreditSpecificationResponse)

instance
  Core.NFData
    ModifyInstanceCreditSpecificationResponse
