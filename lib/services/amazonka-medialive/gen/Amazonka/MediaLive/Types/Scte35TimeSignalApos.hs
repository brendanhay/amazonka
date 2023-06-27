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
-- Module      : Amazonka.MediaLive.Types.Scte35TimeSignalApos
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35TimeSignalApos where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
import Amazonka.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
import qualified Amazonka.Prelude as Prelude

-- | Atypical configuration that applies segment breaks only on SCTE-35 time
-- signal placement opportunities and breaks.
--
-- /See:/ 'newScte35TimeSignalApos' smart constructor.
data Scte35TimeSignalApos = Scte35TimeSignalApos'
  { -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
    -- does not apply to OOB messages.
    adAvailOffset :: Prelude.Maybe Prelude.Int,
    -- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set
    -- to 0 will no longer trigger blackouts or Ad Avail slates
    noRegionalBlackoutFlag :: Prelude.Maybe Scte35AposNoRegionalBlackoutBehavior,
    -- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set
    -- to 0 will no longer trigger blackouts or Ad Avail slates
    webDeliveryAllowedFlag :: Prelude.Maybe Scte35AposWebDeliveryAllowedBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scte35TimeSignalApos' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adAvailOffset', 'scte35TimeSignalApos_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
-- does not apply to OOB messages.
--
-- 'noRegionalBlackoutFlag', 'scte35TimeSignalApos_noRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
--
-- 'webDeliveryAllowedFlag', 'scte35TimeSignalApos_webDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
newScte35TimeSignalApos ::
  Scte35TimeSignalApos
newScte35TimeSignalApos =
  Scte35TimeSignalApos'
    { adAvailOffset =
        Prelude.Nothing,
      noRegionalBlackoutFlag = Prelude.Nothing,
      webDeliveryAllowedFlag = Prelude.Nothing
    }

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
-- does not apply to OOB messages.
scte35TimeSignalApos_adAvailOffset :: Lens.Lens' Scte35TimeSignalApos (Prelude.Maybe Prelude.Int)
scte35TimeSignalApos_adAvailOffset = Lens.lens (\Scte35TimeSignalApos' {adAvailOffset} -> adAvailOffset) (\s@Scte35TimeSignalApos' {} a -> s {adAvailOffset = a} :: Scte35TimeSignalApos)

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
scte35TimeSignalApos_noRegionalBlackoutFlag :: Lens.Lens' Scte35TimeSignalApos (Prelude.Maybe Scte35AposNoRegionalBlackoutBehavior)
scte35TimeSignalApos_noRegionalBlackoutFlag = Lens.lens (\Scte35TimeSignalApos' {noRegionalBlackoutFlag} -> noRegionalBlackoutFlag) (\s@Scte35TimeSignalApos' {} a -> s {noRegionalBlackoutFlag = a} :: Scte35TimeSignalApos)

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
scte35TimeSignalApos_webDeliveryAllowedFlag :: Lens.Lens' Scte35TimeSignalApos (Prelude.Maybe Scte35AposWebDeliveryAllowedBehavior)
scte35TimeSignalApos_webDeliveryAllowedFlag = Lens.lens (\Scte35TimeSignalApos' {webDeliveryAllowedFlag} -> webDeliveryAllowedFlag) (\s@Scte35TimeSignalApos' {} a -> s {webDeliveryAllowedFlag = a} :: Scte35TimeSignalApos)

instance Data.FromJSON Scte35TimeSignalApos where
  parseJSON =
    Data.withObject
      "Scte35TimeSignalApos"
      ( \x ->
          Scte35TimeSignalApos'
            Prelude.<$> (x Data..:? "adAvailOffset")
            Prelude.<*> (x Data..:? "noRegionalBlackoutFlag")
            Prelude.<*> (x Data..:? "webDeliveryAllowedFlag")
      )

instance Prelude.Hashable Scte35TimeSignalApos where
  hashWithSalt _salt Scte35TimeSignalApos' {..} =
    _salt
      `Prelude.hashWithSalt` adAvailOffset
      `Prelude.hashWithSalt` noRegionalBlackoutFlag
      `Prelude.hashWithSalt` webDeliveryAllowedFlag

instance Prelude.NFData Scte35TimeSignalApos where
  rnf Scte35TimeSignalApos' {..} =
    Prelude.rnf adAvailOffset
      `Prelude.seq` Prelude.rnf noRegionalBlackoutFlag
      `Prelude.seq` Prelude.rnf webDeliveryAllowedFlag

instance Data.ToJSON Scte35TimeSignalApos where
  toJSON Scte35TimeSignalApos' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adAvailOffset" Data..=) Prelude.<$> adAvailOffset,
            ("noRegionalBlackoutFlag" Data..=)
              Prelude.<$> noRegionalBlackoutFlag,
            ("webDeliveryAllowedFlag" Data..=)
              Prelude.<$> webDeliveryAllowedFlag
          ]
      )
