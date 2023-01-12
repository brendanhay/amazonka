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
-- Module      : Amazonka.EC2.ModifyInstanceCapacityReservationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Capacity Reservation settings for a stopped instance. Use
-- this action to configure an instance to target a specific Capacity
-- Reservation, run in any @open@ Capacity Reservation with matching
-- attributes, or run On-Demand Instance capacity.
module Amazonka.EC2.ModifyInstanceCapacityReservationAttributes
  ( -- * Creating a Request
    ModifyInstanceCapacityReservationAttributes (..),
    newModifyInstanceCapacityReservationAttributes,

    -- * Request Lenses
    modifyInstanceCapacityReservationAttributes_dryRun,
    modifyInstanceCapacityReservationAttributes_instanceId,
    modifyInstanceCapacityReservationAttributes_capacityReservationSpecification,

    -- * Destructuring the Response
    ModifyInstanceCapacityReservationAttributesResponse (..),
    newModifyInstanceCapacityReservationAttributesResponse,

    -- * Response Lenses
    modifyInstanceCapacityReservationAttributesResponse_return,
    modifyInstanceCapacityReservationAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyInstanceCapacityReservationAttributes' smart constructor.
data ModifyInstanceCapacityReservationAttributes = ModifyInstanceCapacityReservationAttributes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance to be modified.
    instanceId :: Prelude.Text,
    -- | Information about the Capacity Reservation targeting option.
    capacityReservationSpecification :: CapacityReservationSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceCapacityReservationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyInstanceCapacityReservationAttributes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'modifyInstanceCapacityReservationAttributes_instanceId' - The ID of the instance to be modified.
--
-- 'capacityReservationSpecification', 'modifyInstanceCapacityReservationAttributes_capacityReservationSpecification' - Information about the Capacity Reservation targeting option.
newModifyInstanceCapacityReservationAttributes ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'capacityReservationSpecification'
  CapacityReservationSpecification ->
  ModifyInstanceCapacityReservationAttributes
newModifyInstanceCapacityReservationAttributes
  pInstanceId_
  pCapacityReservationSpecification_ =
    ModifyInstanceCapacityReservationAttributes'
      { dryRun =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        capacityReservationSpecification =
          pCapacityReservationSpecification_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceCapacityReservationAttributes_dryRun :: Lens.Lens' ModifyInstanceCapacityReservationAttributes (Prelude.Maybe Prelude.Bool)
modifyInstanceCapacityReservationAttributes_dryRun = Lens.lens (\ModifyInstanceCapacityReservationAttributes' {dryRun} -> dryRun) (\s@ModifyInstanceCapacityReservationAttributes' {} a -> s {dryRun = a} :: ModifyInstanceCapacityReservationAttributes)

-- | The ID of the instance to be modified.
modifyInstanceCapacityReservationAttributes_instanceId :: Lens.Lens' ModifyInstanceCapacityReservationAttributes Prelude.Text
modifyInstanceCapacityReservationAttributes_instanceId = Lens.lens (\ModifyInstanceCapacityReservationAttributes' {instanceId} -> instanceId) (\s@ModifyInstanceCapacityReservationAttributes' {} a -> s {instanceId = a} :: ModifyInstanceCapacityReservationAttributes)

-- | Information about the Capacity Reservation targeting option.
modifyInstanceCapacityReservationAttributes_capacityReservationSpecification :: Lens.Lens' ModifyInstanceCapacityReservationAttributes CapacityReservationSpecification
modifyInstanceCapacityReservationAttributes_capacityReservationSpecification = Lens.lens (\ModifyInstanceCapacityReservationAttributes' {capacityReservationSpecification} -> capacityReservationSpecification) (\s@ModifyInstanceCapacityReservationAttributes' {} a -> s {capacityReservationSpecification = a} :: ModifyInstanceCapacityReservationAttributes)

instance
  Core.AWSRequest
    ModifyInstanceCapacityReservationAttributes
  where
  type
    AWSResponse
      ModifyInstanceCapacityReservationAttributes =
      ModifyInstanceCapacityReservationAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceCapacityReservationAttributesResponse'
            Prelude.<$> (x Data..@? "return")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyInstanceCapacityReservationAttributes
  where
  hashWithSalt
    _salt
    ModifyInstanceCapacityReservationAttributes' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` capacityReservationSpecification

instance
  Prelude.NFData
    ModifyInstanceCapacityReservationAttributes
  where
  rnf ModifyInstanceCapacityReservationAttributes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf capacityReservationSpecification

instance
  Data.ToHeaders
    ModifyInstanceCapacityReservationAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyInstanceCapacityReservationAttributes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyInstanceCapacityReservationAttributes
  where
  toQuery
    ModifyInstanceCapacityReservationAttributes' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "ModifyInstanceCapacityReservationAttributes" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "InstanceId" Data.=: instanceId,
          "CapacityReservationSpecification"
            Data.=: capacityReservationSpecification
        ]

-- | /See:/ 'newModifyInstanceCapacityReservationAttributesResponse' smart constructor.
data ModifyInstanceCapacityReservationAttributesResponse = ModifyInstanceCapacityReservationAttributesResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceCapacityReservationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyInstanceCapacityReservationAttributesResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyInstanceCapacityReservationAttributesResponse_httpStatus' - The response's http status code.
newModifyInstanceCapacityReservationAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstanceCapacityReservationAttributesResponse
newModifyInstanceCapacityReservationAttributesResponse
  pHttpStatus_ =
    ModifyInstanceCapacityReservationAttributesResponse'
      { return' =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyInstanceCapacityReservationAttributesResponse_return :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse (Prelude.Maybe Prelude.Bool)
modifyInstanceCapacityReservationAttributesResponse_return = Lens.lens (\ModifyInstanceCapacityReservationAttributesResponse' {return'} -> return') (\s@ModifyInstanceCapacityReservationAttributesResponse' {} a -> s {return' = a} :: ModifyInstanceCapacityReservationAttributesResponse)

-- | The response's http status code.
modifyInstanceCapacityReservationAttributesResponse_httpStatus :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse Prelude.Int
modifyInstanceCapacityReservationAttributesResponse_httpStatus = Lens.lens (\ModifyInstanceCapacityReservationAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceCapacityReservationAttributesResponse' {} a -> s {httpStatus = a} :: ModifyInstanceCapacityReservationAttributesResponse)

instance
  Prelude.NFData
    ModifyInstanceCapacityReservationAttributesResponse
  where
  rnf
    ModifyInstanceCapacityReservationAttributesResponse' {..} =
      Prelude.rnf return'
        `Prelude.seq` Prelude.rnf httpStatus
