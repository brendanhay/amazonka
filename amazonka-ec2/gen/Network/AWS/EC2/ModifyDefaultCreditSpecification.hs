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
-- Module      : Network.AWS.EC2.ModifyDefaultCreditSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the default credit option for CPU usage of burstable
-- performance instances. The default credit option is set at the account
-- level per AWS Region, and is specified per instance family. All new
-- burstable performance instances in the account launch using the default
-- credit option.
--
-- @ModifyDefaultCreditSpecification@ is an asynchronous operation, which
-- works at an AWS Region level and modifies the credit option for each
-- Availability Zone. All zones in a Region are updated within five
-- minutes. But if instances are launched during this operation, they might
-- not get the new credit option until the zone is updated. To verify
-- whether the update has occurred, you can call
-- @GetDefaultCreditSpecification@ and check @DefaultCreditSpecification@
-- for updates.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.ModifyDefaultCreditSpecification
  ( -- * Creating a Request
    ModifyDefaultCreditSpecification (..),
    newModifyDefaultCreditSpecification,

    -- * Request Lenses
    modifyDefaultCreditSpecification_dryRun,
    modifyDefaultCreditSpecification_instanceFamily,
    modifyDefaultCreditSpecification_cpuCredits,

    -- * Destructuring the Response
    ModifyDefaultCreditSpecificationResponse (..),
    newModifyDefaultCreditSpecificationResponse,

    -- * Response Lenses
    modifyDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    modifyDefaultCreditSpecificationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyDefaultCreditSpecification' smart constructor.
data ModifyDefaultCreditSpecification = ModifyDefaultCreditSpecification'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The instance family.
    instanceFamily :: UnlimitedSupportedInstanceFamily,
    -- | The credit option for CPU usage of the instance family.
    --
    -- Valid Values: @standard@ | @unlimited@
    cpuCredits :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDefaultCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyDefaultCreditSpecification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceFamily', 'modifyDefaultCreditSpecification_instanceFamily' - The instance family.
--
-- 'cpuCredits', 'modifyDefaultCreditSpecification_cpuCredits' - The credit option for CPU usage of the instance family.
--
-- Valid Values: @standard@ | @unlimited@
newModifyDefaultCreditSpecification ::
  -- | 'instanceFamily'
  UnlimitedSupportedInstanceFamily ->
  -- | 'cpuCredits'
  Prelude.Text ->
  ModifyDefaultCreditSpecification
newModifyDefaultCreditSpecification
  pInstanceFamily_
  pCpuCredits_ =
    ModifyDefaultCreditSpecification'
      { dryRun =
          Prelude.Nothing,
        instanceFamily = pInstanceFamily_,
        cpuCredits = pCpuCredits_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyDefaultCreditSpecification_dryRun :: Lens.Lens' ModifyDefaultCreditSpecification (Prelude.Maybe Prelude.Bool)
modifyDefaultCreditSpecification_dryRun = Lens.lens (\ModifyDefaultCreditSpecification' {dryRun} -> dryRun) (\s@ModifyDefaultCreditSpecification' {} a -> s {dryRun = a} :: ModifyDefaultCreditSpecification)

-- | The instance family.
modifyDefaultCreditSpecification_instanceFamily :: Lens.Lens' ModifyDefaultCreditSpecification UnlimitedSupportedInstanceFamily
modifyDefaultCreditSpecification_instanceFamily = Lens.lens (\ModifyDefaultCreditSpecification' {instanceFamily} -> instanceFamily) (\s@ModifyDefaultCreditSpecification' {} a -> s {instanceFamily = a} :: ModifyDefaultCreditSpecification)

-- | The credit option for CPU usage of the instance family.
--
-- Valid Values: @standard@ | @unlimited@
modifyDefaultCreditSpecification_cpuCredits :: Lens.Lens' ModifyDefaultCreditSpecification Prelude.Text
modifyDefaultCreditSpecification_cpuCredits = Lens.lens (\ModifyDefaultCreditSpecification' {cpuCredits} -> cpuCredits) (\s@ModifyDefaultCreditSpecification' {} a -> s {cpuCredits = a} :: ModifyDefaultCreditSpecification)

instance
  Core.AWSRequest
    ModifyDefaultCreditSpecification
  where
  type
    AWSResponse ModifyDefaultCreditSpecification =
      ModifyDefaultCreditSpecificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyDefaultCreditSpecificationResponse'
            Prelude.<$> (x Core..@? "instanceFamilyCreditSpecification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyDefaultCreditSpecification

instance
  Prelude.NFData
    ModifyDefaultCreditSpecification

instance
  Core.ToHeaders
    ModifyDefaultCreditSpecification
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDefaultCreditSpecification where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyDefaultCreditSpecification
  where
  toQuery ModifyDefaultCreditSpecification' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyDefaultCreditSpecification" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "InstanceFamily" Core.=: instanceFamily,
        "CpuCredits" Core.=: cpuCredits
      ]

-- | /See:/ 'newModifyDefaultCreditSpecificationResponse' smart constructor.
data ModifyDefaultCreditSpecificationResponse = ModifyDefaultCreditSpecificationResponse'
  { -- | The default credit option for CPU usage of the instance family.
    instanceFamilyCreditSpecification :: Prelude.Maybe InstanceFamilyCreditSpecification,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDefaultCreditSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamilyCreditSpecification', 'modifyDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification' - The default credit option for CPU usage of the instance family.
--
-- 'httpStatus', 'modifyDefaultCreditSpecificationResponse_httpStatus' - The response's http status code.
newModifyDefaultCreditSpecificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDefaultCreditSpecificationResponse
newModifyDefaultCreditSpecificationResponse
  pHttpStatus_ =
    ModifyDefaultCreditSpecificationResponse'
      { instanceFamilyCreditSpecification =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The default credit option for CPU usage of the instance family.
modifyDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification :: Lens.Lens' ModifyDefaultCreditSpecificationResponse (Prelude.Maybe InstanceFamilyCreditSpecification)
modifyDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification = Lens.lens (\ModifyDefaultCreditSpecificationResponse' {instanceFamilyCreditSpecification} -> instanceFamilyCreditSpecification) (\s@ModifyDefaultCreditSpecificationResponse' {} a -> s {instanceFamilyCreditSpecification = a} :: ModifyDefaultCreditSpecificationResponse)

-- | The response's http status code.
modifyDefaultCreditSpecificationResponse_httpStatus :: Lens.Lens' ModifyDefaultCreditSpecificationResponse Prelude.Int
modifyDefaultCreditSpecificationResponse_httpStatus = Lens.lens (\ModifyDefaultCreditSpecificationResponse' {httpStatus} -> httpStatus) (\s@ModifyDefaultCreditSpecificationResponse' {} a -> s {httpStatus = a} :: ModifyDefaultCreditSpecificationResponse)

instance
  Prelude.NFData
    ModifyDefaultCreditSpecificationResponse
