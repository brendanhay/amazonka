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
-- Module      : Amazonka.EC2.Types.InstanceCreditSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceCreditSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the credit option for CPU usage of a burstable performance
-- instance.
--
-- /See:/ 'newInstanceCreditSpecification' smart constructor.
data InstanceCreditSpecification = InstanceCreditSpecification'
  { -- | The credit option for CPU usage of the instance.
    --
    -- Valid values: @standard@ | @unlimited@
    cpuCredits :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCredits', 'instanceCreditSpecification_cpuCredits' - The credit option for CPU usage of the instance.
--
-- Valid values: @standard@ | @unlimited@
--
-- 'instanceId', 'instanceCreditSpecification_instanceId' - The ID of the instance.
newInstanceCreditSpecification ::
  InstanceCreditSpecification
newInstanceCreditSpecification =
  InstanceCreditSpecification'
    { cpuCredits =
        Prelude.Nothing,
      instanceId = Prelude.Nothing
    }

-- | The credit option for CPU usage of the instance.
--
-- Valid values: @standard@ | @unlimited@
instanceCreditSpecification_cpuCredits :: Lens.Lens' InstanceCreditSpecification (Prelude.Maybe Prelude.Text)
instanceCreditSpecification_cpuCredits = Lens.lens (\InstanceCreditSpecification' {cpuCredits} -> cpuCredits) (\s@InstanceCreditSpecification' {} a -> s {cpuCredits = a} :: InstanceCreditSpecification)

-- | The ID of the instance.
instanceCreditSpecification_instanceId :: Lens.Lens' InstanceCreditSpecification (Prelude.Maybe Prelude.Text)
instanceCreditSpecification_instanceId = Lens.lens (\InstanceCreditSpecification' {instanceId} -> instanceId) (\s@InstanceCreditSpecification' {} a -> s {instanceId = a} :: InstanceCreditSpecification)

instance Data.FromXML InstanceCreditSpecification where
  parseXML x =
    InstanceCreditSpecification'
      Prelude.<$> (x Data..@? "cpuCredits")
      Prelude.<*> (x Data..@? "instanceId")

instance Prelude.Hashable InstanceCreditSpecification where
  hashWithSalt _salt InstanceCreditSpecification' {..} =
    _salt `Prelude.hashWithSalt` cpuCredits
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData InstanceCreditSpecification where
  rnf InstanceCreditSpecification' {..} =
    Prelude.rnf cpuCredits
      `Prelude.seq` Prelude.rnf instanceId
