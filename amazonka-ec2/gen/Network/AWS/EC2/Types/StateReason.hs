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
-- Module      : Network.AWS.EC2.Types.StateReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StateReason where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a state change.
--
-- /See:/ 'newStateReason' smart constructor.
data StateReason = StateReason'
  { -- | The message for the state change.
    --
    -- -   @Server.InsufficientInstanceCapacity@: There was insufficient
    --     capacity available to satisfy the launch request.
    --
    -- -   @Server.InternalError@: An internal error caused the instance to
    --     terminate during launch.
    --
    -- -   @Server.ScheduledStop@: The instance was stopped due to a scheduled
    --     retirement.
    --
    -- -   @Server.SpotInstanceShutdown@: The instance was stopped because the
    --     number of Spot requests with a maximum price equal to or higher than
    --     the Spot price exceeded available capacity or because of an increase
    --     in the Spot price.
    --
    -- -   @Server.SpotInstanceTermination@: The instance was terminated
    --     because the number of Spot requests with a maximum price equal to or
    --     higher than the Spot price exceeded available capacity or because of
    --     an increase in the Spot price.
    --
    -- -   @Client.InstanceInitiatedShutdown@: The instance was shut down using
    --     the @shutdown -h@ command from the instance.
    --
    -- -   @Client.InstanceTerminated@: The instance was terminated or rebooted
    --     during AMI creation.
    --
    -- -   @Client.InternalError@: A client error caused the instance to
    --     terminate during launch.
    --
    -- -   @Client.InvalidSnapshot.NotFound@: The specified snapshot was not
    --     found.
    --
    -- -   @Client.UserInitiatedHibernate@: Hibernation was initiated on the
    --     instance.
    --
    -- -   @Client.UserInitiatedShutdown@: The instance was shut down using the
    --     Amazon EC2 API.
    --
    -- -   @Client.VolumeLimitExceeded@: The limit on the number of EBS volumes
    --     or total storage was exceeded. Decrease usage or request an increase
    --     in your account limits.
    message :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the state change.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StateReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'stateReason_message' - The message for the state change.
--
-- -   @Server.InsufficientInstanceCapacity@: There was insufficient
--     capacity available to satisfy the launch request.
--
-- -   @Server.InternalError@: An internal error caused the instance to
--     terminate during launch.
--
-- -   @Server.ScheduledStop@: The instance was stopped due to a scheduled
--     retirement.
--
-- -   @Server.SpotInstanceShutdown@: The instance was stopped because the
--     number of Spot requests with a maximum price equal to or higher than
--     the Spot price exceeded available capacity or because of an increase
--     in the Spot price.
--
-- -   @Server.SpotInstanceTermination@: The instance was terminated
--     because the number of Spot requests with a maximum price equal to or
--     higher than the Spot price exceeded available capacity or because of
--     an increase in the Spot price.
--
-- -   @Client.InstanceInitiatedShutdown@: The instance was shut down using
--     the @shutdown -h@ command from the instance.
--
-- -   @Client.InstanceTerminated@: The instance was terminated or rebooted
--     during AMI creation.
--
-- -   @Client.InternalError@: A client error caused the instance to
--     terminate during launch.
--
-- -   @Client.InvalidSnapshot.NotFound@: The specified snapshot was not
--     found.
--
-- -   @Client.UserInitiatedHibernate@: Hibernation was initiated on the
--     instance.
--
-- -   @Client.UserInitiatedShutdown@: The instance was shut down using the
--     Amazon EC2 API.
--
-- -   @Client.VolumeLimitExceeded@: The limit on the number of EBS volumes
--     or total storage was exceeded. Decrease usage or request an increase
--     in your account limits.
--
-- 'code', 'stateReason_code' - The reason code for the state change.
newStateReason ::
  StateReason
newStateReason =
  StateReason'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The message for the state change.
--
-- -   @Server.InsufficientInstanceCapacity@: There was insufficient
--     capacity available to satisfy the launch request.
--
-- -   @Server.InternalError@: An internal error caused the instance to
--     terminate during launch.
--
-- -   @Server.ScheduledStop@: The instance was stopped due to a scheduled
--     retirement.
--
-- -   @Server.SpotInstanceShutdown@: The instance was stopped because the
--     number of Spot requests with a maximum price equal to or higher than
--     the Spot price exceeded available capacity or because of an increase
--     in the Spot price.
--
-- -   @Server.SpotInstanceTermination@: The instance was terminated
--     because the number of Spot requests with a maximum price equal to or
--     higher than the Spot price exceeded available capacity or because of
--     an increase in the Spot price.
--
-- -   @Client.InstanceInitiatedShutdown@: The instance was shut down using
--     the @shutdown -h@ command from the instance.
--
-- -   @Client.InstanceTerminated@: The instance was terminated or rebooted
--     during AMI creation.
--
-- -   @Client.InternalError@: A client error caused the instance to
--     terminate during launch.
--
-- -   @Client.InvalidSnapshot.NotFound@: The specified snapshot was not
--     found.
--
-- -   @Client.UserInitiatedHibernate@: Hibernation was initiated on the
--     instance.
--
-- -   @Client.UserInitiatedShutdown@: The instance was shut down using the
--     Amazon EC2 API.
--
-- -   @Client.VolumeLimitExceeded@: The limit on the number of EBS volumes
--     or total storage was exceeded. Decrease usage or request an increase
--     in your account limits.
stateReason_message :: Lens.Lens' StateReason (Prelude.Maybe Prelude.Text)
stateReason_message = Lens.lens (\StateReason' {message} -> message) (\s@StateReason' {} a -> s {message = a} :: StateReason)

-- | The reason code for the state change.
stateReason_code :: Lens.Lens' StateReason (Prelude.Maybe Prelude.Text)
stateReason_code = Lens.lens (\StateReason' {code} -> code) (\s@StateReason' {} a -> s {code = a} :: StateReason)

instance Prelude.FromXML StateReason where
  parseXML x =
    StateReason'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable StateReason

instance Prelude.NFData StateReason
