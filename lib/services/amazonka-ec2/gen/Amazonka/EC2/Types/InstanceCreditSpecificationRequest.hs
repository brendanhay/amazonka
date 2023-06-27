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
-- Module      : Amazonka.EC2.Types.InstanceCreditSpecificationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceCreditSpecificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the credit option for CPU usage of a burstable performance
-- instance.
--
-- /See:/ 'newInstanceCreditSpecificationRequest' smart constructor.
data InstanceCreditSpecificationRequest = InstanceCreditSpecificationRequest'
  { -- | The credit option for CPU usage of the instance.
    --
    -- Valid values: @standard@ | @unlimited@
    --
    -- T3 instances with @host@ tenancy do not support the @unlimited@ CPU
    -- credit option.
    cpuCredits :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCreditSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCredits', 'instanceCreditSpecificationRequest_cpuCredits' - The credit option for CPU usage of the instance.
--
-- Valid values: @standard@ | @unlimited@
--
-- T3 instances with @host@ tenancy do not support the @unlimited@ CPU
-- credit option.
--
-- 'instanceId', 'instanceCreditSpecificationRequest_instanceId' - The ID of the instance.
newInstanceCreditSpecificationRequest ::
  -- | 'instanceId'
  Prelude.Text ->
  InstanceCreditSpecificationRequest
newInstanceCreditSpecificationRequest pInstanceId_ =
  InstanceCreditSpecificationRequest'
    { cpuCredits =
        Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The credit option for CPU usage of the instance.
--
-- Valid values: @standard@ | @unlimited@
--
-- T3 instances with @host@ tenancy do not support the @unlimited@ CPU
-- credit option.
instanceCreditSpecificationRequest_cpuCredits :: Lens.Lens' InstanceCreditSpecificationRequest (Prelude.Maybe Prelude.Text)
instanceCreditSpecificationRequest_cpuCredits = Lens.lens (\InstanceCreditSpecificationRequest' {cpuCredits} -> cpuCredits) (\s@InstanceCreditSpecificationRequest' {} a -> s {cpuCredits = a} :: InstanceCreditSpecificationRequest)

-- | The ID of the instance.
instanceCreditSpecificationRequest_instanceId :: Lens.Lens' InstanceCreditSpecificationRequest Prelude.Text
instanceCreditSpecificationRequest_instanceId = Lens.lens (\InstanceCreditSpecificationRequest' {instanceId} -> instanceId) (\s@InstanceCreditSpecificationRequest' {} a -> s {instanceId = a} :: InstanceCreditSpecificationRequest)

instance
  Prelude.Hashable
    InstanceCreditSpecificationRequest
  where
  hashWithSalt
    _salt
    InstanceCreditSpecificationRequest' {..} =
      _salt
        `Prelude.hashWithSalt` cpuCredits
        `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    InstanceCreditSpecificationRequest
  where
  rnf InstanceCreditSpecificationRequest' {..} =
    Prelude.rnf cpuCredits
      `Prelude.seq` Prelude.rnf instanceId

instance
  Data.ToQuery
    InstanceCreditSpecificationRequest
  where
  toQuery InstanceCreditSpecificationRequest' {..} =
    Prelude.mconcat
      [ "CpuCredits" Data.=: cpuCredits,
        "InstanceId" Data.=: instanceId
      ]
