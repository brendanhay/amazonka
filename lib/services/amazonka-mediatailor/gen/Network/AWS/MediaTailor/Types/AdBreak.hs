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
-- Module      : Network.AWS.MediaTailor.Types.AdBreak
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaTailor.Types.AdBreak where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types.MessageType
import Network.AWS.MediaTailor.Types.SlateSource
import Network.AWS.MediaTailor.Types.SpliceInsertMessage
import qualified Network.AWS.Prelude as Prelude

-- | Ad break configuration parameters.
--
-- /See:/ 'newAdBreak' smart constructor.
data AdBreak = AdBreak'
  { -- | This defines the SCTE-35 splice_insert() message inserted around the ad.
    -- For information about using splice_insert(), see the SCTE-35
    -- specficiaiton, section 9.7.3.1.
    spliceInsertMessage :: Prelude.Maybe SpliceInsertMessage,
    -- | The SCTE-35 ad insertion type. Accepted value: SPLICE_INSERT.
    messageType :: Prelude.Maybe MessageType,
    -- | Ad break slate configuration.
    slate :: Prelude.Maybe SlateSource,
    -- | How long (in milliseconds) after the beginning of the program that an ad
    -- starts. This value must fall within 100ms of a segment boundary,
    -- otherwise the ad break will be skipped.
    offsetMillis :: Prelude.Maybe Prelude.Integer
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
-- 'spliceInsertMessage', 'adBreak_spliceInsertMessage' - This defines the SCTE-35 splice_insert() message inserted around the ad.
-- For information about using splice_insert(), see the SCTE-35
-- specficiaiton, section 9.7.3.1.
--
-- 'messageType', 'adBreak_messageType' - The SCTE-35 ad insertion type. Accepted value: SPLICE_INSERT.
--
-- 'slate', 'adBreak_slate' - Ad break slate configuration.
--
-- 'offsetMillis', 'adBreak_offsetMillis' - How long (in milliseconds) after the beginning of the program that an ad
-- starts. This value must fall within 100ms of a segment boundary,
-- otherwise the ad break will be skipped.
newAdBreak ::
  AdBreak
newAdBreak =
  AdBreak'
    { spliceInsertMessage = Prelude.Nothing,
      messageType = Prelude.Nothing,
      slate = Prelude.Nothing,
      offsetMillis = Prelude.Nothing
    }

-- | This defines the SCTE-35 splice_insert() message inserted around the ad.
-- For information about using splice_insert(), see the SCTE-35
-- specficiaiton, section 9.7.3.1.
adBreak_spliceInsertMessage :: Lens.Lens' AdBreak (Prelude.Maybe SpliceInsertMessage)
adBreak_spliceInsertMessage = Lens.lens (\AdBreak' {spliceInsertMessage} -> spliceInsertMessage) (\s@AdBreak' {} a -> s {spliceInsertMessage = a} :: AdBreak)

-- | The SCTE-35 ad insertion type. Accepted value: SPLICE_INSERT.
adBreak_messageType :: Lens.Lens' AdBreak (Prelude.Maybe MessageType)
adBreak_messageType = Lens.lens (\AdBreak' {messageType} -> messageType) (\s@AdBreak' {} a -> s {messageType = a} :: AdBreak)

-- | Ad break slate configuration.
adBreak_slate :: Lens.Lens' AdBreak (Prelude.Maybe SlateSource)
adBreak_slate = Lens.lens (\AdBreak' {slate} -> slate) (\s@AdBreak' {} a -> s {slate = a} :: AdBreak)

-- | How long (in milliseconds) after the beginning of the program that an ad
-- starts. This value must fall within 100ms of a segment boundary,
-- otherwise the ad break will be skipped.
adBreak_offsetMillis :: Lens.Lens' AdBreak (Prelude.Maybe Prelude.Integer)
adBreak_offsetMillis = Lens.lens (\AdBreak' {offsetMillis} -> offsetMillis) (\s@AdBreak' {} a -> s {offsetMillis = a} :: AdBreak)

instance Core.FromJSON AdBreak where
  parseJSON =
    Core.withObject
      "AdBreak"
      ( \x ->
          AdBreak'
            Prelude.<$> (x Core..:? "SpliceInsertMessage")
            Prelude.<*> (x Core..:? "MessageType")
            Prelude.<*> (x Core..:? "Slate")
            Prelude.<*> (x Core..:? "OffsetMillis")
      )

instance Prelude.Hashable AdBreak

instance Prelude.NFData AdBreak

instance Core.ToJSON AdBreak where
  toJSON AdBreak' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SpliceInsertMessage" Core..=)
              Prelude.<$> spliceInsertMessage,
            ("MessageType" Core..=) Prelude.<$> messageType,
            ("Slate" Core..=) Prelude.<$> slate,
            ("OffsetMillis" Core..=) Prelude.<$> offsetMillis
          ]
      )
