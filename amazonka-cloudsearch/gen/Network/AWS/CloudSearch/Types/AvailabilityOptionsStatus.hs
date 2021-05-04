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
-- Module      : Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus where

import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status and configuration of the domain\'s availability options.
--
-- /See:/ 'newAvailabilityOptionsStatus' smart constructor.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus'
  { -- | The availability options configured for the domain.
    options :: Prelude.Bool,
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'availabilityOptionsStatus_options' - The availability options configured for the domain.
--
-- 'status', 'availabilityOptionsStatus_status' - Undocumented member.
newAvailabilityOptionsStatus ::
  -- | 'options'
  Prelude.Bool ->
  -- | 'status'
  OptionStatus ->
  AvailabilityOptionsStatus
newAvailabilityOptionsStatus pOptions_ pStatus_ =
  AvailabilityOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | The availability options configured for the domain.
availabilityOptionsStatus_options :: Lens.Lens' AvailabilityOptionsStatus Prelude.Bool
availabilityOptionsStatus_options = Lens.lens (\AvailabilityOptionsStatus' {options} -> options) (\s@AvailabilityOptionsStatus' {} a -> s {options = a} :: AvailabilityOptionsStatus)

-- | Undocumented member.
availabilityOptionsStatus_status :: Lens.Lens' AvailabilityOptionsStatus OptionStatus
availabilityOptionsStatus_status = Lens.lens (\AvailabilityOptionsStatus' {status} -> status) (\s@AvailabilityOptionsStatus' {} a -> s {status = a} :: AvailabilityOptionsStatus)

instance Prelude.FromXML AvailabilityOptionsStatus where
  parseXML x =
    AvailabilityOptionsStatus'
      Prelude.<$> (x Prelude..@ "Options")
      Prelude.<*> (x Prelude..@ "Status")

instance Prelude.Hashable AvailabilityOptionsStatus

instance Prelude.NFData AvailabilityOptionsStatus
