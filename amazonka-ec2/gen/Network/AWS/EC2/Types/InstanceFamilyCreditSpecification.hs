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
-- Module      : Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceFamilyCreditSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the default credit option for CPU usage of a burstable
-- performance instance family.
--
-- /See:/ 'newInstanceFamilyCreditSpecification' smart constructor.
data InstanceFamilyCreditSpecification = InstanceFamilyCreditSpecification'
  { -- | The instance family.
    instanceFamily :: Prelude.Maybe UnlimitedSupportedInstanceFamily,
    -- | The default credit option for CPU usage of the instance family. Valid
    -- values are @standard@ and @unlimited@.
    cpuCredits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceFamilyCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'instanceFamilyCreditSpecification_instanceFamily' - The instance family.
--
-- 'cpuCredits', 'instanceFamilyCreditSpecification_cpuCredits' - The default credit option for CPU usage of the instance family. Valid
-- values are @standard@ and @unlimited@.
newInstanceFamilyCreditSpecification ::
  InstanceFamilyCreditSpecification
newInstanceFamilyCreditSpecification =
  InstanceFamilyCreditSpecification'
    { instanceFamily =
        Prelude.Nothing,
      cpuCredits = Prelude.Nothing
    }

-- | The instance family.
instanceFamilyCreditSpecification_instanceFamily :: Lens.Lens' InstanceFamilyCreditSpecification (Prelude.Maybe UnlimitedSupportedInstanceFamily)
instanceFamilyCreditSpecification_instanceFamily = Lens.lens (\InstanceFamilyCreditSpecification' {instanceFamily} -> instanceFamily) (\s@InstanceFamilyCreditSpecification' {} a -> s {instanceFamily = a} :: InstanceFamilyCreditSpecification)

-- | The default credit option for CPU usage of the instance family. Valid
-- values are @standard@ and @unlimited@.
instanceFamilyCreditSpecification_cpuCredits :: Lens.Lens' InstanceFamilyCreditSpecification (Prelude.Maybe Prelude.Text)
instanceFamilyCreditSpecification_cpuCredits = Lens.lens (\InstanceFamilyCreditSpecification' {cpuCredits} -> cpuCredits) (\s@InstanceFamilyCreditSpecification' {} a -> s {cpuCredits = a} :: InstanceFamilyCreditSpecification)

instance
  Prelude.FromXML
    InstanceFamilyCreditSpecification
  where
  parseXML x =
    InstanceFamilyCreditSpecification'
      Prelude.<$> (x Prelude..@? "instanceFamily")
      Prelude.<*> (x Prelude..@? "cpuCredits")

instance
  Prelude.Hashable
    InstanceFamilyCreditSpecification

instance
  Prelude.NFData
    InstanceFamilyCreditSpecification
