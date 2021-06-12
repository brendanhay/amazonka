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
-- Module      : Network.AWS.EC2.GetDefaultCreditSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default credit option for CPU usage of a burstable
-- performance instance family.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.GetDefaultCreditSpecification
  ( -- * Creating a Request
    GetDefaultCreditSpecification (..),
    newGetDefaultCreditSpecification,

    -- * Request Lenses
    getDefaultCreditSpecification_dryRun,
    getDefaultCreditSpecification_instanceFamily,

    -- * Destructuring the Response
    GetDefaultCreditSpecificationResponse (..),
    newGetDefaultCreditSpecificationResponse,

    -- * Response Lenses
    getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    getDefaultCreditSpecificationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDefaultCreditSpecification' smart constructor.
data GetDefaultCreditSpecification = GetDefaultCreditSpecification'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The instance family.
    instanceFamily :: UnlimitedSupportedInstanceFamily
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDefaultCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getDefaultCreditSpecification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceFamily', 'getDefaultCreditSpecification_instanceFamily' - The instance family.
newGetDefaultCreditSpecification ::
  -- | 'instanceFamily'
  UnlimitedSupportedInstanceFamily ->
  GetDefaultCreditSpecification
newGetDefaultCreditSpecification pInstanceFamily_ =
  GetDefaultCreditSpecification'
    { dryRun =
        Core.Nothing,
      instanceFamily = pInstanceFamily_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getDefaultCreditSpecification_dryRun :: Lens.Lens' GetDefaultCreditSpecification (Core.Maybe Core.Bool)
getDefaultCreditSpecification_dryRun = Lens.lens (\GetDefaultCreditSpecification' {dryRun} -> dryRun) (\s@GetDefaultCreditSpecification' {} a -> s {dryRun = a} :: GetDefaultCreditSpecification)

-- | The instance family.
getDefaultCreditSpecification_instanceFamily :: Lens.Lens' GetDefaultCreditSpecification UnlimitedSupportedInstanceFamily
getDefaultCreditSpecification_instanceFamily = Lens.lens (\GetDefaultCreditSpecification' {instanceFamily} -> instanceFamily) (\s@GetDefaultCreditSpecification' {} a -> s {instanceFamily = a} :: GetDefaultCreditSpecification)

instance
  Core.AWSRequest
    GetDefaultCreditSpecification
  where
  type
    AWSResponse GetDefaultCreditSpecification =
      GetDefaultCreditSpecificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetDefaultCreditSpecificationResponse'
            Core.<$> (x Core..@? "instanceFamilyCreditSpecification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDefaultCreditSpecification

instance Core.NFData GetDefaultCreditSpecification

instance Core.ToHeaders GetDefaultCreditSpecification where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetDefaultCreditSpecification where
  toPath = Core.const "/"

instance Core.ToQuery GetDefaultCreditSpecification where
  toQuery GetDefaultCreditSpecification' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetDefaultCreditSpecification" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InstanceFamily" Core.=: instanceFamily
      ]

-- | /See:/ 'newGetDefaultCreditSpecificationResponse' smart constructor.
data GetDefaultCreditSpecificationResponse = GetDefaultCreditSpecificationResponse'
  { -- | The default credit option for CPU usage of the instance family.
    instanceFamilyCreditSpecification :: Core.Maybe InstanceFamilyCreditSpecification,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDefaultCreditSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamilyCreditSpecification', 'getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification' - The default credit option for CPU usage of the instance family.
--
-- 'httpStatus', 'getDefaultCreditSpecificationResponse_httpStatus' - The response's http status code.
newGetDefaultCreditSpecificationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDefaultCreditSpecificationResponse
newGetDefaultCreditSpecificationResponse pHttpStatus_ =
  GetDefaultCreditSpecificationResponse'
    { instanceFamilyCreditSpecification =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The default credit option for CPU usage of the instance family.
getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification :: Lens.Lens' GetDefaultCreditSpecificationResponse (Core.Maybe InstanceFamilyCreditSpecification)
getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification = Lens.lens (\GetDefaultCreditSpecificationResponse' {instanceFamilyCreditSpecification} -> instanceFamilyCreditSpecification) (\s@GetDefaultCreditSpecificationResponse' {} a -> s {instanceFamilyCreditSpecification = a} :: GetDefaultCreditSpecificationResponse)

-- | The response's http status code.
getDefaultCreditSpecificationResponse_httpStatus :: Lens.Lens' GetDefaultCreditSpecificationResponse Core.Int
getDefaultCreditSpecificationResponse_httpStatus = Lens.lens (\GetDefaultCreditSpecificationResponse' {httpStatus} -> httpStatus) (\s@GetDefaultCreditSpecificationResponse' {} a -> s {httpStatus = a} :: GetDefaultCreditSpecificationResponse)

instance
  Core.NFData
    GetDefaultCreditSpecificationResponse
