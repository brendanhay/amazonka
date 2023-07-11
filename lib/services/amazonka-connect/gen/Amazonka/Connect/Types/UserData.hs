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
-- Module      : Amazonka.Connect.Types.UserData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserData where

import Amazonka.Connect.Types.AgentContactReference
import Amazonka.Connect.Types.AgentStatusReference
import Amazonka.Connect.Types.Channel
import Amazonka.Connect.Types.HierarchyPathReference
import Amazonka.Connect.Types.RoutingProfileReference
import Amazonka.Connect.Types.UserReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data for a user.
--
-- /See:/ 'newUserData' smart constructor.
data UserData = UserData'
  { -- | A map of active slots by channel. The key is a channel name. The value
    -- is an integer: the number of active slots.
    activeSlotsByChannel :: Prelude.Maybe (Prelude.HashMap Channel Prelude.Natural),
    -- | A map of available slots by channel. The key is a channel name. The
    -- value is an integer: the available number of slots.
    availableSlotsByChannel :: Prelude.Maybe (Prelude.HashMap Channel Prelude.Natural),
    -- | A list of contact reference information.
    contacts :: Prelude.Maybe [AgentContactReference],
    -- | Contains information about the levels of a hierarchy group assigned to a
    -- user.
    hierarchyPath :: Prelude.Maybe HierarchyPathReference,
    -- | A map of maximum slots by channel. The key is a channel name. The value
    -- is an integer: the maximum number of slots. This is calculated from
    -- <https://docs.aws.amazon.com/connect/latest/APIReference/API_MediaConcurrency.html MediaConcurrency>
    -- of the @RoutingProfile@ assigned to the agent.
    maxSlotsByChannel :: Prelude.Maybe (Prelude.HashMap Channel Prelude.Natural),
    -- | The Next status of the agent.
    nextStatus :: Prelude.Maybe Prelude.Text,
    -- | Information about the routing profile that is assigned to the user.
    routingProfile :: Prelude.Maybe RoutingProfileReference,
    -- | The status of the agent that they manually set in their Contact Control
    -- Panel (CCP), or that the supervisor manually changes in the real-time
    -- metrics report.
    status :: Prelude.Maybe AgentStatusReference,
    -- | Information about the user for the data that is returned. It contains
    -- the @resourceId@ and ARN of the user.
    user :: Prelude.Maybe UserReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeSlotsByChannel', 'userData_activeSlotsByChannel' - A map of active slots by channel. The key is a channel name. The value
-- is an integer: the number of active slots.
--
-- 'availableSlotsByChannel', 'userData_availableSlotsByChannel' - A map of available slots by channel. The key is a channel name. The
-- value is an integer: the available number of slots.
--
-- 'contacts', 'userData_contacts' - A list of contact reference information.
--
-- 'hierarchyPath', 'userData_hierarchyPath' - Contains information about the levels of a hierarchy group assigned to a
-- user.
--
-- 'maxSlotsByChannel', 'userData_maxSlotsByChannel' - A map of maximum slots by channel. The key is a channel name. The value
-- is an integer: the maximum number of slots. This is calculated from
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_MediaConcurrency.html MediaConcurrency>
-- of the @RoutingProfile@ assigned to the agent.
--
-- 'nextStatus', 'userData_nextStatus' - The Next status of the agent.
--
-- 'routingProfile', 'userData_routingProfile' - Information about the routing profile that is assigned to the user.
--
-- 'status', 'userData_status' - The status of the agent that they manually set in their Contact Control
-- Panel (CCP), or that the supervisor manually changes in the real-time
-- metrics report.
--
-- 'user', 'userData_user' - Information about the user for the data that is returned. It contains
-- the @resourceId@ and ARN of the user.
newUserData ::
  UserData
newUserData =
  UserData'
    { activeSlotsByChannel = Prelude.Nothing,
      availableSlotsByChannel = Prelude.Nothing,
      contacts = Prelude.Nothing,
      hierarchyPath = Prelude.Nothing,
      maxSlotsByChannel = Prelude.Nothing,
      nextStatus = Prelude.Nothing,
      routingProfile = Prelude.Nothing,
      status = Prelude.Nothing,
      user = Prelude.Nothing
    }

-- | A map of active slots by channel. The key is a channel name. The value
-- is an integer: the number of active slots.
userData_activeSlotsByChannel :: Lens.Lens' UserData (Prelude.Maybe (Prelude.HashMap Channel Prelude.Natural))
userData_activeSlotsByChannel = Lens.lens (\UserData' {activeSlotsByChannel} -> activeSlotsByChannel) (\s@UserData' {} a -> s {activeSlotsByChannel = a} :: UserData) Prelude.. Lens.mapping Lens.coerced

-- | A map of available slots by channel. The key is a channel name. The
-- value is an integer: the available number of slots.
userData_availableSlotsByChannel :: Lens.Lens' UserData (Prelude.Maybe (Prelude.HashMap Channel Prelude.Natural))
userData_availableSlotsByChannel = Lens.lens (\UserData' {availableSlotsByChannel} -> availableSlotsByChannel) (\s@UserData' {} a -> s {availableSlotsByChannel = a} :: UserData) Prelude.. Lens.mapping Lens.coerced

-- | A list of contact reference information.
userData_contacts :: Lens.Lens' UserData (Prelude.Maybe [AgentContactReference])
userData_contacts = Lens.lens (\UserData' {contacts} -> contacts) (\s@UserData' {} a -> s {contacts = a} :: UserData) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the levels of a hierarchy group assigned to a
-- user.
userData_hierarchyPath :: Lens.Lens' UserData (Prelude.Maybe HierarchyPathReference)
userData_hierarchyPath = Lens.lens (\UserData' {hierarchyPath} -> hierarchyPath) (\s@UserData' {} a -> s {hierarchyPath = a} :: UserData)

-- | A map of maximum slots by channel. The key is a channel name. The value
-- is an integer: the maximum number of slots. This is calculated from
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_MediaConcurrency.html MediaConcurrency>
-- of the @RoutingProfile@ assigned to the agent.
userData_maxSlotsByChannel :: Lens.Lens' UserData (Prelude.Maybe (Prelude.HashMap Channel Prelude.Natural))
userData_maxSlotsByChannel = Lens.lens (\UserData' {maxSlotsByChannel} -> maxSlotsByChannel) (\s@UserData' {} a -> s {maxSlotsByChannel = a} :: UserData) Prelude.. Lens.mapping Lens.coerced

-- | The Next status of the agent.
userData_nextStatus :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_nextStatus = Lens.lens (\UserData' {nextStatus} -> nextStatus) (\s@UserData' {} a -> s {nextStatus = a} :: UserData)

-- | Information about the routing profile that is assigned to the user.
userData_routingProfile :: Lens.Lens' UserData (Prelude.Maybe RoutingProfileReference)
userData_routingProfile = Lens.lens (\UserData' {routingProfile} -> routingProfile) (\s@UserData' {} a -> s {routingProfile = a} :: UserData)

-- | The status of the agent that they manually set in their Contact Control
-- Panel (CCP), or that the supervisor manually changes in the real-time
-- metrics report.
userData_status :: Lens.Lens' UserData (Prelude.Maybe AgentStatusReference)
userData_status = Lens.lens (\UserData' {status} -> status) (\s@UserData' {} a -> s {status = a} :: UserData)

-- | Information about the user for the data that is returned. It contains
-- the @resourceId@ and ARN of the user.
userData_user :: Lens.Lens' UserData (Prelude.Maybe UserReference)
userData_user = Lens.lens (\UserData' {user} -> user) (\s@UserData' {} a -> s {user = a} :: UserData)

instance Data.FromJSON UserData where
  parseJSON =
    Data.withObject
      "UserData"
      ( \x ->
          UserData'
            Prelude.<$> ( x
                            Data..:? "ActiveSlotsByChannel"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "AvailableSlotsByChannel"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Contacts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "HierarchyPath")
            Prelude.<*> ( x
                            Data..:? "MaxSlotsByChannel"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NextStatus")
            Prelude.<*> (x Data..:? "RoutingProfile")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "User")
      )

instance Prelude.Hashable UserData where
  hashWithSalt _salt UserData' {..} =
    _salt
      `Prelude.hashWithSalt` activeSlotsByChannel
      `Prelude.hashWithSalt` availableSlotsByChannel
      `Prelude.hashWithSalt` contacts
      `Prelude.hashWithSalt` hierarchyPath
      `Prelude.hashWithSalt` maxSlotsByChannel
      `Prelude.hashWithSalt` nextStatus
      `Prelude.hashWithSalt` routingProfile
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` user

instance Prelude.NFData UserData where
  rnf UserData' {..} =
    Prelude.rnf activeSlotsByChannel
      `Prelude.seq` Prelude.rnf availableSlotsByChannel
      `Prelude.seq` Prelude.rnf contacts
      `Prelude.seq` Prelude.rnf hierarchyPath
      `Prelude.seq` Prelude.rnf maxSlotsByChannel
      `Prelude.seq` Prelude.rnf nextStatus
      `Prelude.seq` Prelude.rnf routingProfile
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf user
