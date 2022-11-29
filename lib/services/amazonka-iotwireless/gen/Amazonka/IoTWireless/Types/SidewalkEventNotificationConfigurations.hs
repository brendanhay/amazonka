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
-- Module      : Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import qualified Amazonka.Prelude as Prelude

-- | @SidewalkEventNotificationConfigurations@ object, which is the event
-- configuration object for Sidewalk-related event topics.
--
-- /See:/ 'newSidewalkEventNotificationConfigurations' smart constructor.
data SidewalkEventNotificationConfigurations = SidewalkEventNotificationConfigurations'
  { -- | Denotes whether the Amazon ID event topic is enabled or disabled.
    amazonIdEventTopic :: Prelude.Maybe EventNotificationTopicStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkEventNotificationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonIdEventTopic', 'sidewalkEventNotificationConfigurations_amazonIdEventTopic' - Denotes whether the Amazon ID event topic is enabled or disabled.
newSidewalkEventNotificationConfigurations ::
  SidewalkEventNotificationConfigurations
newSidewalkEventNotificationConfigurations =
  SidewalkEventNotificationConfigurations'
    { amazonIdEventTopic =
        Prelude.Nothing
    }

-- | Denotes whether the Amazon ID event topic is enabled or disabled.
sidewalkEventNotificationConfigurations_amazonIdEventTopic :: Lens.Lens' SidewalkEventNotificationConfigurations (Prelude.Maybe EventNotificationTopicStatus)
sidewalkEventNotificationConfigurations_amazonIdEventTopic = Lens.lens (\SidewalkEventNotificationConfigurations' {amazonIdEventTopic} -> amazonIdEventTopic) (\s@SidewalkEventNotificationConfigurations' {} a -> s {amazonIdEventTopic = a} :: SidewalkEventNotificationConfigurations)

instance
  Core.FromJSON
    SidewalkEventNotificationConfigurations
  where
  parseJSON =
    Core.withObject
      "SidewalkEventNotificationConfigurations"
      ( \x ->
          SidewalkEventNotificationConfigurations'
            Prelude.<$> (x Core..:? "AmazonIdEventTopic")
      )

instance
  Prelude.Hashable
    SidewalkEventNotificationConfigurations
  where
  hashWithSalt
    _salt
    SidewalkEventNotificationConfigurations' {..} =
      _salt `Prelude.hashWithSalt` amazonIdEventTopic

instance
  Prelude.NFData
    SidewalkEventNotificationConfigurations
  where
  rnf SidewalkEventNotificationConfigurations' {..} =
    Prelude.rnf amazonIdEventTopic

instance
  Core.ToJSON
    SidewalkEventNotificationConfigurations
  where
  toJSON SidewalkEventNotificationConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AmazonIdEventTopic" Core..=)
              Prelude.<$> amazonIdEventTopic
          ]
      )
