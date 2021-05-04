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
-- Module      : Network.AWS.EMR.Types.InstanceFleetStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetStateChangeReason where

import Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides status change reason details for the instance fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleetStateChangeReason' smart constructor.
data InstanceFleetStateChangeReason = InstanceFleetStateChangeReason'
  { -- | An explanatory message.
    message :: Prelude.Maybe Prelude.Text,
    -- | A code corresponding to the reason the state change occurred.
    code :: Prelude.Maybe InstanceFleetStateChangeReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceFleetStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'instanceFleetStateChangeReason_message' - An explanatory message.
--
-- 'code', 'instanceFleetStateChangeReason_code' - A code corresponding to the reason the state change occurred.
newInstanceFleetStateChangeReason ::
  InstanceFleetStateChangeReason
newInstanceFleetStateChangeReason =
  InstanceFleetStateChangeReason'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | An explanatory message.
instanceFleetStateChangeReason_message :: Lens.Lens' InstanceFleetStateChangeReason (Prelude.Maybe Prelude.Text)
instanceFleetStateChangeReason_message = Lens.lens (\InstanceFleetStateChangeReason' {message} -> message) (\s@InstanceFleetStateChangeReason' {} a -> s {message = a} :: InstanceFleetStateChangeReason)

-- | A code corresponding to the reason the state change occurred.
instanceFleetStateChangeReason_code :: Lens.Lens' InstanceFleetStateChangeReason (Prelude.Maybe InstanceFleetStateChangeReasonCode)
instanceFleetStateChangeReason_code = Lens.lens (\InstanceFleetStateChangeReason' {code} -> code) (\s@InstanceFleetStateChangeReason' {} a -> s {code = a} :: InstanceFleetStateChangeReason)

instance
  Prelude.FromJSON
    InstanceFleetStateChangeReason
  where
  parseJSON =
    Prelude.withObject
      "InstanceFleetStateChangeReason"
      ( \x ->
          InstanceFleetStateChangeReason'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Code")
      )

instance
  Prelude.Hashable
    InstanceFleetStateChangeReason

instance
  Prelude.NFData
    InstanceFleetStateChangeReason
