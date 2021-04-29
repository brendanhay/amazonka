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
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalApos
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35TimeSignalApos where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
import qualified Network.AWS.Prelude as Prelude

-- | Scte35 Time Signal Apos
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Scte35TimeSignalApos where
  parseJSON =
    Prelude.withObject
      "Scte35TimeSignalApos"
      ( \x ->
          Scte35TimeSignalApos'
            Prelude.<$> (x Prelude..:? "adAvailOffset")
            Prelude.<*> (x Prelude..:? "noRegionalBlackoutFlag")
            Prelude.<*> (x Prelude..:? "webDeliveryAllowedFlag")
      )

instance Prelude.Hashable Scte35TimeSignalApos

instance Prelude.NFData Scte35TimeSignalApos

instance Prelude.ToJSON Scte35TimeSignalApos where
  toJSON Scte35TimeSignalApos' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("adAvailOffset" Prelude..=)
              Prelude.<$> adAvailOffset,
            ("noRegionalBlackoutFlag" Prelude..=)
              Prelude.<$> noRegionalBlackoutFlag,
            ("webDeliveryAllowedFlag" Prelude..=)
              Prelude.<$> webDeliveryAllowedFlag
          ]
      )
