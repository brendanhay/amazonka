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
-- Module      : Amazonka.EC2.GetDefaultCreditSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.EC2.GetDefaultCreditSpecification
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDefaultCreditSpecification' smart constructor.
data GetDefaultCreditSpecification = GetDefaultCreditSpecification'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The instance family.
    instanceFamily :: UnlimitedSupportedInstanceFamily
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      instanceFamily = pInstanceFamily_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getDefaultCreditSpecification_dryRun :: Lens.Lens' GetDefaultCreditSpecification (Prelude.Maybe Prelude.Bool)
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetDefaultCreditSpecificationResponse'
            Prelude.<$> (x Data..@? "instanceFamilyCreditSpecification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDefaultCreditSpecification
  where
  hashWithSalt _salt GetDefaultCreditSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceFamily

instance Prelude.NFData GetDefaultCreditSpecification where
  rnf GetDefaultCreditSpecification' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceFamily

instance Data.ToHeaders GetDefaultCreditSpecification where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetDefaultCreditSpecification where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDefaultCreditSpecification where
  toQuery GetDefaultCreditSpecification' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetDefaultCreditSpecification" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceFamily" Data.=: instanceFamily
      ]

-- | /See:/ 'newGetDefaultCreditSpecificationResponse' smart constructor.
data GetDefaultCreditSpecificationResponse = GetDefaultCreditSpecificationResponse'
  { -- | The default credit option for CPU usage of the instance family.
    instanceFamilyCreditSpecification :: Prelude.Maybe InstanceFamilyCreditSpecification,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetDefaultCreditSpecificationResponse
newGetDefaultCreditSpecificationResponse pHttpStatus_ =
  GetDefaultCreditSpecificationResponse'
    { instanceFamilyCreditSpecification =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The default credit option for CPU usage of the instance family.
getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification :: Lens.Lens' GetDefaultCreditSpecificationResponse (Prelude.Maybe InstanceFamilyCreditSpecification)
getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification = Lens.lens (\GetDefaultCreditSpecificationResponse' {instanceFamilyCreditSpecification} -> instanceFamilyCreditSpecification) (\s@GetDefaultCreditSpecificationResponse' {} a -> s {instanceFamilyCreditSpecification = a} :: GetDefaultCreditSpecificationResponse)

-- | The response's http status code.
getDefaultCreditSpecificationResponse_httpStatus :: Lens.Lens' GetDefaultCreditSpecificationResponse Prelude.Int
getDefaultCreditSpecificationResponse_httpStatus = Lens.lens (\GetDefaultCreditSpecificationResponse' {httpStatus} -> httpStatus) (\s@GetDefaultCreditSpecificationResponse' {} a -> s {httpStatus = a} :: GetDefaultCreditSpecificationResponse)

instance
  Prelude.NFData
    GetDefaultCreditSpecificationResponse
  where
  rnf GetDefaultCreditSpecificationResponse' {..} =
    Prelude.rnf instanceFamilyCreditSpecification
      `Prelude.seq` Prelude.rnf httpStatus
