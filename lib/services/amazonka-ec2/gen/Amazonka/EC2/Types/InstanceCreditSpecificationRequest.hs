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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The credit option for CPU usage of the instance.
    --
    -- Valid values: @standard@ | @unlimited@
    --
    -- T3 instances with @host@ tenancy do not support the @unlimited@ CPU
    -- credit option.
    cpuCredits :: Prelude.Maybe Prelude.Text
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
-- 'instanceId', 'instanceCreditSpecificationRequest_instanceId' - The ID of the instance.
--
-- 'cpuCredits', 'instanceCreditSpecificationRequest_cpuCredits' - The credit option for CPU usage of the instance.
--
-- Valid values: @standard@ | @unlimited@
--
-- T3 instances with @host@ tenancy do not support the @unlimited@ CPU
-- credit option.
newInstanceCreditSpecificationRequest ::
  InstanceCreditSpecificationRequest
newInstanceCreditSpecificationRequest =
  InstanceCreditSpecificationRequest'
    { instanceId =
        Prelude.Nothing,
      cpuCredits = Prelude.Nothing
    }

-- | The ID of the instance.
instanceCreditSpecificationRequest_instanceId :: Lens.Lens' InstanceCreditSpecificationRequest (Prelude.Maybe Prelude.Text)
instanceCreditSpecificationRequest_instanceId = Lens.lens (\InstanceCreditSpecificationRequest' {instanceId} -> instanceId) (\s@InstanceCreditSpecificationRequest' {} a -> s {instanceId = a} :: InstanceCreditSpecificationRequest)

-- | The credit option for CPU usage of the instance.
--
-- Valid values: @standard@ | @unlimited@
--
-- T3 instances with @host@ tenancy do not support the @unlimited@ CPU
-- credit option.
instanceCreditSpecificationRequest_cpuCredits :: Lens.Lens' InstanceCreditSpecificationRequest (Prelude.Maybe Prelude.Text)
instanceCreditSpecificationRequest_cpuCredits = Lens.lens (\InstanceCreditSpecificationRequest' {cpuCredits} -> cpuCredits) (\s@InstanceCreditSpecificationRequest' {} a -> s {cpuCredits = a} :: InstanceCreditSpecificationRequest)

instance
  Prelude.Hashable
    InstanceCreditSpecificationRequest
  where
  hashWithSalt
    _salt
    InstanceCreditSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` cpuCredits

instance
  Prelude.NFData
    InstanceCreditSpecificationRequest
  where
  rnf InstanceCreditSpecificationRequest' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf cpuCredits

instance
  Data.ToQuery
    InstanceCreditSpecificationRequest
  where
  toQuery InstanceCreditSpecificationRequest' {..} =
    Prelude.mconcat
      [ "InstanceId" Data.=: instanceId,
        "CpuCredits" Data.=: cpuCredits
      ]
