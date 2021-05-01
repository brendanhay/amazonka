{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the credit option for CPU usage of a burstable performance
-- instance.
--
-- /See:/ 'newInstanceCreditSpecification' smart constructor.
data InstanceCreditSpecification = InstanceCreditSpecification'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The credit option for CPU usage of the instance. Valid values are
    -- @standard@ and @unlimited@.
    cpuCredits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      cpuCredits = Prelude.Nothing
    }

-- | The ID of the instance.
instanceCreditSpecification_instanceId :: Lens.Lens' InstanceCreditSpecification (Prelude.Maybe Prelude.Text)
instanceCreditSpecification_instanceId = Lens.lens (\InstanceCreditSpecification' {instanceId} -> instanceId) (\s@InstanceCreditSpecification' {} a -> s {instanceId = a} :: InstanceCreditSpecification)

-- | The credit option for CPU usage of the instance. Valid values are
-- @standard@ and @unlimited@.
instanceCreditSpecification_cpuCredits :: Lens.Lens' InstanceCreditSpecification (Prelude.Maybe Prelude.Text)
instanceCreditSpecification_cpuCredits = Lens.lens (\InstanceCreditSpecification' {cpuCredits} -> cpuCredits) (\s@InstanceCreditSpecification' {} a -> s {cpuCredits = a} :: InstanceCreditSpecification)

instance Prelude.FromXML InstanceCreditSpecification where
  parseXML x =
    InstanceCreditSpecification'
      Prelude.<$> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "cpuCredits")

instance Prelude.Hashable InstanceCreditSpecification

instance Prelude.NFData InstanceCreditSpecification
