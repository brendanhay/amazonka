{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCreditSpecificationRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the credit option for CPU usage of a burstable performance
-- instance.
--
-- /See:/ 'newInstanceCreditSpecificationRequest' smart constructor.
data InstanceCreditSpecificationRequest = InstanceCreditSpecificationRequest'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The credit option for CPU usage of the instance. Valid values are
    -- @standard@ and @unlimited@.
    cpuCredits :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceCreditSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceCreditSpecificationRequest_instanceId' - The ID of the instance.
--
-- 'cpuCredits', 'instanceCreditSpecificationRequest_cpuCredits' - The credit option for CPU usage of the instance. Valid values are
-- @standard@ and @unlimited@.
newInstanceCreditSpecificationRequest ::
  InstanceCreditSpecificationRequest
newInstanceCreditSpecificationRequest =
  InstanceCreditSpecificationRequest'
    { instanceId =
        Core.Nothing,
      cpuCredits = Core.Nothing
    }

-- | The ID of the instance.
instanceCreditSpecificationRequest_instanceId :: Lens.Lens' InstanceCreditSpecificationRequest (Core.Maybe Core.Text)
instanceCreditSpecificationRequest_instanceId = Lens.lens (\InstanceCreditSpecificationRequest' {instanceId} -> instanceId) (\s@InstanceCreditSpecificationRequest' {} a -> s {instanceId = a} :: InstanceCreditSpecificationRequest)

-- | The credit option for CPU usage of the instance. Valid values are
-- @standard@ and @unlimited@.
instanceCreditSpecificationRequest_cpuCredits :: Lens.Lens' InstanceCreditSpecificationRequest (Core.Maybe Core.Text)
instanceCreditSpecificationRequest_cpuCredits = Lens.lens (\InstanceCreditSpecificationRequest' {cpuCredits} -> cpuCredits) (\s@InstanceCreditSpecificationRequest' {} a -> s {cpuCredits = a} :: InstanceCreditSpecificationRequest)

instance
  Core.Hashable
    InstanceCreditSpecificationRequest

instance
  Core.NFData
    InstanceCreditSpecificationRequest

instance
  Core.ToQuery
    InstanceCreditSpecificationRequest
  where
  toQuery InstanceCreditSpecificationRequest' {..} =
    Core.mconcat
      [ "InstanceId" Core.=: instanceId,
        "CpuCredits" Core.=: cpuCredits
      ]
