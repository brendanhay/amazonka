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
-- Module      : Amazonka.EC2.Types.AvailabilityZoneMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AvailabilityZoneMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a message about an Availability Zone, Local Zone, or
-- Wavelength Zone.
--
-- /See:/ 'newAvailabilityZoneMessage' smart constructor.
data AvailabilityZoneMessage = AvailabilityZoneMessage'
  { -- | The message about the Availability Zone, Local Zone, or Wavelength Zone.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZoneMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'availabilityZoneMessage_message' - The message about the Availability Zone, Local Zone, or Wavelength Zone.
newAvailabilityZoneMessage ::
  AvailabilityZoneMessage
newAvailabilityZoneMessage =
  AvailabilityZoneMessage' {message = Prelude.Nothing}

-- | The message about the Availability Zone, Local Zone, or Wavelength Zone.
availabilityZoneMessage_message :: Lens.Lens' AvailabilityZoneMessage (Prelude.Maybe Prelude.Text)
availabilityZoneMessage_message = Lens.lens (\AvailabilityZoneMessage' {message} -> message) (\s@AvailabilityZoneMessage' {} a -> s {message = a} :: AvailabilityZoneMessage)

instance Data.FromXML AvailabilityZoneMessage where
  parseXML x =
    AvailabilityZoneMessage'
      Prelude.<$> (x Data..@? "message")

instance Prelude.Hashable AvailabilityZoneMessage where
  hashWithSalt _salt AvailabilityZoneMessage' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData AvailabilityZoneMessage where
  rnf AvailabilityZoneMessage' {..} =
    Prelude.rnf message
