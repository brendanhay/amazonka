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
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCreditSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the credit option for CPU usage of a burstable performance
-- instance.
--
-- /See:/ 'newInstanceCreditSpecification' smart constructor.
data InstanceCreditSpecification = InstanceCreditSpecification'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The credit option for CPU usage of the instance. Valid values are
    -- @standard@ and @unlimited@.
    cpuCredits :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceCreditSpecification_instanceId' - The ID of the instance.
--
-- 'cpuCredits', 'instanceCreditSpecification_cpuCredits' - The credit option for CPU usage of the instance. Valid values are
-- @standard@ and @unlimited@.
newInstanceCreditSpecification ::
  InstanceCreditSpecification
newInstanceCreditSpecification =
  InstanceCreditSpecification'
    { instanceId =
        Core.Nothing,
      cpuCredits = Core.Nothing
    }

-- | The ID of the instance.
instanceCreditSpecification_instanceId :: Lens.Lens' InstanceCreditSpecification (Core.Maybe Core.Text)
instanceCreditSpecification_instanceId = Lens.lens (\InstanceCreditSpecification' {instanceId} -> instanceId) (\s@InstanceCreditSpecification' {} a -> s {instanceId = a} :: InstanceCreditSpecification)

-- | The credit option for CPU usage of the instance. Valid values are
-- @standard@ and @unlimited@.
instanceCreditSpecification_cpuCredits :: Lens.Lens' InstanceCreditSpecification (Core.Maybe Core.Text)
instanceCreditSpecification_cpuCredits = Lens.lens (\InstanceCreditSpecification' {cpuCredits} -> cpuCredits) (\s@InstanceCreditSpecification' {} a -> s {cpuCredits = a} :: InstanceCreditSpecification)

instance Core.FromXML InstanceCreditSpecification where
  parseXML x =
    InstanceCreditSpecification'
      Core.<$> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "cpuCredits")

instance Core.Hashable InstanceCreditSpecification

instance Core.NFData InstanceCreditSpecification
