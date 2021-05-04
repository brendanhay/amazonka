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
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneMessage where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a message about an Availability Zone, Local Zone, or
-- Wavelength Zone.
--
-- /See:/ 'newAvailabilityZoneMessage' smart constructor.
data AvailabilityZoneMessage = AvailabilityZoneMessage'
  { -- | The message about the Availability Zone, Local Zone, or Wavelength Zone.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML AvailabilityZoneMessage where
  parseXML x =
    AvailabilityZoneMessage'
      Prelude.<$> (x Prelude..@? "message")

instance Prelude.Hashable AvailabilityZoneMessage

instance Prelude.NFData AvailabilityZoneMessage
