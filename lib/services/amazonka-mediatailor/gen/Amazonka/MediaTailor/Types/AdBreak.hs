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
-- Module      : Amazonka.MediaTailor.Types.AdBreak
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.AdBreak where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.MessageType
import Amazonka.MediaTailor.Types.SlateSource
import Amazonka.MediaTailor.Types.SpliceInsertMessage
import Amazonka.MediaTailor.Types.TimeSignalMessage
import qualified Amazonka.Prelude as Prelude

-- | Ad break configuration parameters.
--
-- /See:/ 'newAdBreak' smart constructor.
data AdBreak = AdBreak'
  { -- | The SCTE-35 ad insertion type. Accepted value: @SPLICE_INSERT@,
    -- @TIME_SIGNAL@.
    messageType :: Prelude.Maybe MessageType,
    -- | How long (in milliseconds) after the beginning of the program that an ad
    -- starts. This value must fall within 100ms of a segment boundary,
    -- otherwise the ad break will be skipped.
    offsetMillis :: Prelude.Maybe Prelude.Integer,
    -- | Ad break slate configuration.
    slate :: Prelude.Maybe SlateSource,
    -- | This defines the SCTE-35 @splice_insert()@ message inserted around the
    -- ad. For information about using @splice_insert()@, see the SCTE-35
    -- specficiaiton, section 9.7.3.1.
    spliceInsertMessage :: Prelude.Maybe SpliceInsertMessage,
    -- | Defines the SCTE-35 @time_signal@ message inserted around the ad.
    --
    -- Programs on a channel\'s schedule can be configured with one or more ad
    -- breaks. You can attach a @splice_insert@ SCTE-35 message to the ad
    -- break. This message provides basic metadata about the ad break.
    --
    -- See section 9.7.4 of the 2022 SCTE-35 specification for more
    -- information.
    timeSignalMessage :: Prelude.Maybe TimeSignalMessage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdBreak' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageType', 'adBreak_messageType' - The SCTE-35 ad insertion type. Accepted value: @SPLICE_INSERT@,
-- @TIME_SIGNAL@.
--
-- 'offsetMillis', 'adBreak_offsetMillis' - How long (in milliseconds) after the beginning of the program that an ad
-- starts. This value must fall within 100ms of a segment boundary,
-- otherwise the ad break will be skipped.
--
-- 'slate', 'adBreak_slate' - Ad break slate configuration.
--
-- 'spliceInsertMessage', 'adBreak_spliceInsertMessage' - This defines the SCTE-35 @splice_insert()@ message inserted around the
-- ad. For information about using @splice_insert()@, see the SCTE-35
-- specficiaiton, section 9.7.3.1.
--
-- 'timeSignalMessage', 'adBreak_timeSignalMessage' - Defines the SCTE-35 @time_signal@ message inserted around the ad.
--
-- Programs on a channel\'s schedule can be configured with one or more ad
-- breaks. You can attach a @splice_insert@ SCTE-35 message to the ad
-- break. This message provides basic metadata about the ad break.
--
-- See section 9.7.4 of the 2022 SCTE-35 specification for more
-- information.
newAdBreak ::
  AdBreak
newAdBreak =
  AdBreak'
    { messageType = Prelude.Nothing,
      offsetMillis = Prelude.Nothing,
      slate = Prelude.Nothing,
      spliceInsertMessage = Prelude.Nothing,
      timeSignalMessage = Prelude.Nothing
    }

-- | The SCTE-35 ad insertion type. Accepted value: @SPLICE_INSERT@,
-- @TIME_SIGNAL@.
adBreak_messageType :: Lens.Lens' AdBreak (Prelude.Maybe MessageType)
adBreak_messageType = Lens.lens (\AdBreak' {messageType} -> messageType) (\s@AdBreak' {} a -> s {messageType = a} :: AdBreak)

-- | How long (in milliseconds) after the beginning of the program that an ad
-- starts. This value must fall within 100ms of a segment boundary,
-- otherwise the ad break will be skipped.
adBreak_offsetMillis :: Lens.Lens' AdBreak (Prelude.Maybe Prelude.Integer)
adBreak_offsetMillis = Lens.lens (\AdBreak' {offsetMillis} -> offsetMillis) (\s@AdBreak' {} a -> s {offsetMillis = a} :: AdBreak)

-- | Ad break slate configuration.
adBreak_slate :: Lens.Lens' AdBreak (Prelude.Maybe SlateSource)
adBreak_slate = Lens.lens (\AdBreak' {slate} -> slate) (\s@AdBreak' {} a -> s {slate = a} :: AdBreak)

-- | This defines the SCTE-35 @splice_insert()@ message inserted around the
-- ad. For information about using @splice_insert()@, see the SCTE-35
-- specficiaiton, section 9.7.3.1.
adBreak_spliceInsertMessage :: Lens.Lens' AdBreak (Prelude.Maybe SpliceInsertMessage)
adBreak_spliceInsertMessage = Lens.lens (\AdBreak' {spliceInsertMessage} -> spliceInsertMessage) (\s@AdBreak' {} a -> s {spliceInsertMessage = a} :: AdBreak)

-- | Defines the SCTE-35 @time_signal@ message inserted around the ad.
--
-- Programs on a channel\'s schedule can be configured with one or more ad
-- breaks. You can attach a @splice_insert@ SCTE-35 message to the ad
-- break. This message provides basic metadata about the ad break.
--
-- See section 9.7.4 of the 2022 SCTE-35 specification for more
-- information.
adBreak_timeSignalMessage :: Lens.Lens' AdBreak (Prelude.Maybe TimeSignalMessage)
adBreak_timeSignalMessage = Lens.lens (\AdBreak' {timeSignalMessage} -> timeSignalMessage) (\s@AdBreak' {} a -> s {timeSignalMessage = a} :: AdBreak)

instance Data.FromJSON AdBreak where
  parseJSON =
    Data.withObject
      "AdBreak"
      ( \x ->
          AdBreak'
            Prelude.<$> (x Data..:? "MessageType")
            Prelude.<*> (x Data..:? "OffsetMillis")
            Prelude.<*> (x Data..:? "Slate")
            Prelude.<*> (x Data..:? "SpliceInsertMessage")
            Prelude.<*> (x Data..:? "TimeSignalMessage")
      )

instance Prelude.Hashable AdBreak where
  hashWithSalt _salt AdBreak' {..} =
    _salt
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` offsetMillis
      `Prelude.hashWithSalt` slate
      `Prelude.hashWithSalt` spliceInsertMessage
      `Prelude.hashWithSalt` timeSignalMessage

instance Prelude.NFData AdBreak where
  rnf AdBreak' {..} =
    Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf offsetMillis
      `Prelude.seq` Prelude.rnf slate
      `Prelude.seq` Prelude.rnf spliceInsertMessage
      `Prelude.seq` Prelude.rnf timeSignalMessage

instance Data.ToJSON AdBreak where
  toJSON AdBreak' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MessageType" Data..=) Prelude.<$> messageType,
            ("OffsetMillis" Data..=) Prelude.<$> offsetMillis,
            ("Slate" Data..=) Prelude.<$> slate,
            ("SpliceInsertMessage" Data..=)
              Prelude.<$> spliceInsertMessage,
            ("TimeSignalMessage" Data..=)
              Prelude.<$> timeSignalMessage
          ]
      )
