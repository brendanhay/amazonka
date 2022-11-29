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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types.EventDestination
import Amazonka.PinpointSmsVoiceV2.Types.MessageType
import qualified Amazonka.Prelude as Prelude

-- | Information related to a given configuration set in your Amazon Web
-- Services account.
--
-- /See:/ 'newConfigurationSetInformation' smart constructor.
data ConfigurationSetInformation = ConfigurationSetInformation'
  { -- | The default sender ID used by the ConfigurationSet.
    defaultSenderId :: Prelude.Maybe Prelude.Text,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    defaultMessageType :: Prelude.Maybe MessageType,
    -- | The Resource Name (ARN) of the ConfigurationSet.
    configurationSetArn :: Prelude.Text,
    -- | The name of the ConfigurationSet.
    configurationSetName :: Prelude.Text,
    -- | An array of EventDestination objects that describe any events to log and
    -- where to log them.
    eventDestinations :: [EventDestination],
    -- | The time when the ConfigurationSet was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationSetInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultSenderId', 'configurationSetInformation_defaultSenderId' - The default sender ID used by the ConfigurationSet.
--
-- 'defaultMessageType', 'configurationSetInformation_defaultMessageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'configurationSetArn', 'configurationSetInformation_configurationSetArn' - The Resource Name (ARN) of the ConfigurationSet.
--
-- 'configurationSetName', 'configurationSetInformation_configurationSetName' - The name of the ConfigurationSet.
--
-- 'eventDestinations', 'configurationSetInformation_eventDestinations' - An array of EventDestination objects that describe any events to log and
-- where to log them.
--
-- 'createdTimestamp', 'configurationSetInformation_createdTimestamp' - The time when the ConfigurationSet was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
newConfigurationSetInformation ::
  -- | 'configurationSetArn'
  Prelude.Text ->
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'createdTimestamp'
  Prelude.UTCTime ->
  ConfigurationSetInformation
newConfigurationSetInformation
  pConfigurationSetArn_
  pConfigurationSetName_
  pCreatedTimestamp_ =
    ConfigurationSetInformation'
      { defaultSenderId =
          Prelude.Nothing,
        defaultMessageType = Prelude.Nothing,
        configurationSetArn = pConfigurationSetArn_,
        configurationSetName = pConfigurationSetName_,
        eventDestinations = Prelude.mempty,
        createdTimestamp =
          Core._Time Lens.# pCreatedTimestamp_
      }

-- | The default sender ID used by the ConfigurationSet.
configurationSetInformation_defaultSenderId :: Lens.Lens' ConfigurationSetInformation (Prelude.Maybe Prelude.Text)
configurationSetInformation_defaultSenderId = Lens.lens (\ConfigurationSetInformation' {defaultSenderId} -> defaultSenderId) (\s@ConfigurationSetInformation' {} a -> s {defaultSenderId = a} :: ConfigurationSetInformation)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
configurationSetInformation_defaultMessageType :: Lens.Lens' ConfigurationSetInformation (Prelude.Maybe MessageType)
configurationSetInformation_defaultMessageType = Lens.lens (\ConfigurationSetInformation' {defaultMessageType} -> defaultMessageType) (\s@ConfigurationSetInformation' {} a -> s {defaultMessageType = a} :: ConfigurationSetInformation)

-- | The Resource Name (ARN) of the ConfigurationSet.
configurationSetInformation_configurationSetArn :: Lens.Lens' ConfigurationSetInformation Prelude.Text
configurationSetInformation_configurationSetArn = Lens.lens (\ConfigurationSetInformation' {configurationSetArn} -> configurationSetArn) (\s@ConfigurationSetInformation' {} a -> s {configurationSetArn = a} :: ConfigurationSetInformation)

-- | The name of the ConfigurationSet.
configurationSetInformation_configurationSetName :: Lens.Lens' ConfigurationSetInformation Prelude.Text
configurationSetInformation_configurationSetName = Lens.lens (\ConfigurationSetInformation' {configurationSetName} -> configurationSetName) (\s@ConfigurationSetInformation' {} a -> s {configurationSetName = a} :: ConfigurationSetInformation)

-- | An array of EventDestination objects that describe any events to log and
-- where to log them.
configurationSetInformation_eventDestinations :: Lens.Lens' ConfigurationSetInformation [EventDestination]
configurationSetInformation_eventDestinations = Lens.lens (\ConfigurationSetInformation' {eventDestinations} -> eventDestinations) (\s@ConfigurationSetInformation' {} a -> s {eventDestinations = a} :: ConfigurationSetInformation) Prelude.. Lens.coerced

-- | The time when the ConfigurationSet was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
configurationSetInformation_createdTimestamp :: Lens.Lens' ConfigurationSetInformation Prelude.UTCTime
configurationSetInformation_createdTimestamp = Lens.lens (\ConfigurationSetInformation' {createdTimestamp} -> createdTimestamp) (\s@ConfigurationSetInformation' {} a -> s {createdTimestamp = a} :: ConfigurationSetInformation) Prelude.. Core._Time

instance Core.FromJSON ConfigurationSetInformation where
  parseJSON =
    Core.withObject
      "ConfigurationSetInformation"
      ( \x ->
          ConfigurationSetInformation'
            Prelude.<$> (x Core..:? "DefaultSenderId")
            Prelude.<*> (x Core..:? "DefaultMessageType")
            Prelude.<*> (x Core..: "ConfigurationSetArn")
            Prelude.<*> (x Core..: "ConfigurationSetName")
            Prelude.<*> ( x Core..:? "EventDestinations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "CreatedTimestamp")
      )

instance Prelude.Hashable ConfigurationSetInformation where
  hashWithSalt _salt ConfigurationSetInformation' {..} =
    _salt `Prelude.hashWithSalt` defaultSenderId
      `Prelude.hashWithSalt` defaultMessageType
      `Prelude.hashWithSalt` configurationSetArn
      `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` eventDestinations
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData ConfigurationSetInformation where
  rnf ConfigurationSetInformation' {..} =
    Prelude.rnf defaultSenderId
      `Prelude.seq` Prelude.rnf defaultMessageType
      `Prelude.seq` Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestinations
      `Prelude.seq` Prelude.rnf createdTimestamp
