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
-- Module      : Amazonka.IoTSiteWise.Types.PropertyNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.PropertyNotification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.PropertyNotificationState
import qualified Amazonka.Prelude as Prelude

-- | Contains asset property value notification information. When the
-- notification state is enabled, IoT SiteWise publishes property value
-- updates to a unique MQTT topic. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/interact-with-other-services.html Interacting with other services>
-- in the /IoT SiteWise User Guide/.
--
-- /See:/ 'newPropertyNotification' smart constructor.
data PropertyNotification = PropertyNotification'
  { -- | The MQTT topic to which IoT SiteWise publishes property value update
    -- notifications.
    topic :: Prelude.Text,
    -- | The current notification state.
    state :: PropertyNotificationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topic', 'propertyNotification_topic' - The MQTT topic to which IoT SiteWise publishes property value update
-- notifications.
--
-- 'state', 'propertyNotification_state' - The current notification state.
newPropertyNotification ::
  -- | 'topic'
  Prelude.Text ->
  -- | 'state'
  PropertyNotificationState ->
  PropertyNotification
newPropertyNotification pTopic_ pState_ =
  PropertyNotification'
    { topic = pTopic_,
      state = pState_
    }

-- | The MQTT topic to which IoT SiteWise publishes property value update
-- notifications.
propertyNotification_topic :: Lens.Lens' PropertyNotification Prelude.Text
propertyNotification_topic = Lens.lens (\PropertyNotification' {topic} -> topic) (\s@PropertyNotification' {} a -> s {topic = a} :: PropertyNotification)

-- | The current notification state.
propertyNotification_state :: Lens.Lens' PropertyNotification PropertyNotificationState
propertyNotification_state = Lens.lens (\PropertyNotification' {state} -> state) (\s@PropertyNotification' {} a -> s {state = a} :: PropertyNotification)

instance Data.FromJSON PropertyNotification where
  parseJSON =
    Data.withObject
      "PropertyNotification"
      ( \x ->
          PropertyNotification'
            Prelude.<$> (x Data..: "topic")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable PropertyNotification where
  hashWithSalt _salt PropertyNotification' {..} =
    _salt
      `Prelude.hashWithSalt` topic
      `Prelude.hashWithSalt` state

instance Prelude.NFData PropertyNotification where
  rnf PropertyNotification' {..} =
    Prelude.rnf topic `Prelude.seq` Prelude.rnf state
