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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.AdBreak where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.MessageType
import Amazonka.MediaTailor.Types.SlateSource
import Amazonka.MediaTailor.Types.SpliceInsertMessage
import qualified Amazonka.Prelude as Prelude

-- | Ad break configuration parameters.
--
-- /See:/ 'newAdBreak' smart constructor.
data AdBreak = AdBreak'
  { -- | The SCTE-35 ad insertion type. Accepted value: SPLICE_INSERT.
    messageType :: Prelude.Maybe MessageType,
    -- | How long (in milliseconds) after the beginning of the program that an ad
    -- starts. This value must fall within 100ms of a segment boundary,
    -- otherwise the ad break will be skipped.
    offsetMillis :: Prelude.Maybe Prelude.Integer,
    -- | This defines the SCTE-35 splice_insert() message inserted around the ad.
    -- For information about using splice_insert(), see the SCTE-35
    -- specficiaiton, section 9.7.3.1.
    spliceInsertMessage :: Prelude.Maybe SpliceInsertMessage,
    -- | Ad break slate configuration.
    slate :: Prelude.Maybe SlateSource
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
-- 'messageType', 'adBreak_messageType' - The SCTE-35 ad insertion type. Accepted value: SPLICE_INSERT.
--
-- 'offsetMillis', 'adBreak_offsetMillis' - How long (in milliseconds) after the beginning of the program that an ad
-- starts. This value must fall within 100ms of a segment boundary,
-- otherwise the ad break will be skipped.
--
-- 'spliceInsertMessage', 'adBreak_spliceInsertMessage' - This defines the SCTE-35 splice_insert() message inserted around the ad.
-- For information about using splice_insert(), see the SCTE-35
-- specficiaiton, section 9.7.3.1.
--
-- 'slate', 'adBreak_slate' - Ad break slate configuration.
newAdBreak ::
  AdBreak
newAdBreak =
  AdBreak'
    { messageType = Prelude.Nothing,
      offsetMillis = Prelude.Nothing,
      spliceInsertMessage = Prelude.Nothing,
      slate = Prelude.Nothing
    }

-- | The SCTE-35 ad insertion type. Accepted value: SPLICE_INSERT.
adBreak_messageType :: Lens.Lens' AdBreak (Prelude.Maybe MessageType)
adBreak_messageType = Lens.lens (\AdBreak' {messageType} -> messageType) (\s@AdBreak' {} a -> s {messageType = a} :: AdBreak)

-- | How long (in milliseconds) after the beginning of the program that an ad
-- starts. This value must fall within 100ms of a segment boundary,
-- otherwise the ad break will be skipped.
adBreak_offsetMillis :: Lens.Lens' AdBreak (Prelude.Maybe Prelude.Integer)
adBreak_offsetMillis = Lens.lens (\AdBreak' {offsetMillis} -> offsetMillis) (\s@AdBreak' {} a -> s {offsetMillis = a} :: AdBreak)

-- | This defines the SCTE-35 splice_insert() message inserted around the ad.
-- For information about using splice_insert(), see the SCTE-35
-- specficiaiton, section 9.7.3.1.
adBreak_spliceInsertMessage :: Lens.Lens' AdBreak (Prelude.Maybe SpliceInsertMessage)
adBreak_spliceInsertMessage = Lens.lens (\AdBreak' {spliceInsertMessage} -> spliceInsertMessage) (\s@AdBreak' {} a -> s {spliceInsertMessage = a} :: AdBreak)

-- | Ad break slate configuration.
adBreak_slate :: Lens.Lens' AdBreak (Prelude.Maybe SlateSource)
adBreak_slate = Lens.lens (\AdBreak' {slate} -> slate) (\s@AdBreak' {} a -> s {slate = a} :: AdBreak)

instance Core.FromJSON AdBreak where
  parseJSON =
    Core.withObject
      "AdBreak"
      ( \x ->
          AdBreak'
            Prelude.<$> (x Core..:? "MessageType")
            Prelude.<*> (x Core..:? "OffsetMillis")
            Prelude.<*> (x Core..:? "SpliceInsertMessage")
            Prelude.<*> (x Core..:? "Slate")
      )

instance Prelude.Hashable AdBreak where
  hashWithSalt _salt AdBreak' {..} =
    _salt `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` offsetMillis
      `Prelude.hashWithSalt` spliceInsertMessage
      `Prelude.hashWithSalt` slate

instance Prelude.NFData AdBreak where
  rnf AdBreak' {..} =
    Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf offsetMillis
      `Prelude.seq` Prelude.rnf spliceInsertMessage
      `Prelude.seq` Prelude.rnf slate

instance Core.ToJSON AdBreak where
  toJSON AdBreak' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MessageType" Core..=) Prelude.<$> messageType,
            ("OffsetMillis" Core..=) Prelude.<$> offsetMillis,
            ("SpliceInsertMessage" Core..=)
              Prelude.<$> spliceInsertMessage,
            ("Slate" Core..=) Prelude.<$> slate
          ]
      )
