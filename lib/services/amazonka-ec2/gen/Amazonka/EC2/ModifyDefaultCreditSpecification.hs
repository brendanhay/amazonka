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
-- Module      : Amazonka.EC2.ModifyDefaultCreditSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the default credit option for CPU usage of burstable
-- performance instances. The default credit option is set at the account
-- level per Amazon Web Services Region, and is specified per instance
-- family. All new burstable performance instances in the account launch
-- using the default credit option.
--
-- @ModifyDefaultCreditSpecification@ is an asynchronous operation, which
-- works at an Amazon Web Services Region level and modifies the credit
-- option for each Availability Zone. All zones in a Region are updated
-- within five minutes. But if instances are launched during this
-- operation, they might not get the new credit option until the zone is
-- updated. To verify whether the update has occurred, you can call
-- @GetDefaultCreditSpecification@ and check @DefaultCreditSpecification@
-- for updates.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.ModifyDefaultCreditSpecification
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyDefaultCreditSpecificationResponse'
            Prelude.<$> (x Data..@? "instanceFamilyCreditSpecification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyDefaultCreditSpecification
  where
  hashWithSalt
    _salt
    ModifyDefaultCreditSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` instanceFamily
        `Prelude.hashWithSalt` cpuCredits

instance
  Prelude.NFData
    ModifyDefaultCreditSpecification
  where
  rnf ModifyDefaultCreditSpecification' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf instanceFamily `Prelude.seq`
        Prelude.rnf cpuCredits

instance
  Data.ToHeaders
    ModifyDefaultCreditSpecification
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDefaultCreditSpecification where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyDefaultCreditSpecification
  where
  toQuery ModifyDefaultCreditSpecification' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyDefaultCreditSpecification" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceFamily" Data.=: instanceFamily,
        "CpuCredits" Data.=: cpuCredits
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
  where
  rnf ModifyDefaultCreditSpecificationResponse' {..} =
    Prelude.rnf instanceFamilyCreditSpecification `Prelude.seq`
      Prelude.rnf httpStatus
