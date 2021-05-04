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
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsert
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsert where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
import qualified Network.AWS.Prelude as Prelude

-- | Scte35 Splice Insert
--
-- /See:/ 'newScte35SpliceInsert' smart constructor.
data Scte35SpliceInsert = Scte35SpliceInsert'
  { -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
    -- does not apply to OOB messages.
    adAvailOffset :: Prelude.Maybe Prelude.Int,
    -- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set
    -- to 0 will no longer trigger blackouts or Ad Avail slates
    noRegionalBlackoutFlag :: Prelude.Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior,
    -- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set
    -- to 0 will no longer trigger blackouts or Ad Avail slates
    webDeliveryAllowedFlag :: Prelude.Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scte35SpliceInsert' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adAvailOffset', 'scte35SpliceInsert_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
-- does not apply to OOB messages.
--
-- 'noRegionalBlackoutFlag', 'scte35SpliceInsert_noRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
--
-- 'webDeliveryAllowedFlag', 'scte35SpliceInsert_webDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
newScte35SpliceInsert ::
  Scte35SpliceInsert
newScte35SpliceInsert =
  Scte35SpliceInsert'
    { adAvailOffset =
        Prelude.Nothing,
      noRegionalBlackoutFlag = Prelude.Nothing,
      webDeliveryAllowedFlag = Prelude.Nothing
    }

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
-- does not apply to OOB messages.
scte35SpliceInsert_adAvailOffset :: Lens.Lens' Scte35SpliceInsert (Prelude.Maybe Prelude.Int)
scte35SpliceInsert_adAvailOffset = Lens.lens (\Scte35SpliceInsert' {adAvailOffset} -> adAvailOffset) (\s@Scte35SpliceInsert' {} a -> s {adAvailOffset = a} :: Scte35SpliceInsert)

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
scte35SpliceInsert_noRegionalBlackoutFlag :: Lens.Lens' Scte35SpliceInsert (Prelude.Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior)
scte35SpliceInsert_noRegionalBlackoutFlag = Lens.lens (\Scte35SpliceInsert' {noRegionalBlackoutFlag} -> noRegionalBlackoutFlag) (\s@Scte35SpliceInsert' {} a -> s {noRegionalBlackoutFlag = a} :: Scte35SpliceInsert)

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set
-- to 0 will no longer trigger blackouts or Ad Avail slates
scte35SpliceInsert_webDeliveryAllowedFlag :: Lens.Lens' Scte35SpliceInsert (Prelude.Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior)
scte35SpliceInsert_webDeliveryAllowedFlag = Lens.lens (\Scte35SpliceInsert' {webDeliveryAllowedFlag} -> webDeliveryAllowedFlag) (\s@Scte35SpliceInsert' {} a -> s {webDeliveryAllowedFlag = a} :: Scte35SpliceInsert)

instance Prelude.FromJSON Scte35SpliceInsert where
  parseJSON =
    Prelude.withObject
      "Scte35SpliceInsert"
      ( \x ->
          Scte35SpliceInsert'
            Prelude.<$> (x Prelude..:? "adAvailOffset")
            Prelude.<*> (x Prelude..:? "noRegionalBlackoutFlag")
            Prelude.<*> (x Prelude..:? "webDeliveryAllowedFlag")
      )

instance Prelude.Hashable Scte35SpliceInsert

instance Prelude.NFData Scte35SpliceInsert

instance Prelude.ToJSON Scte35SpliceInsert where
  toJSON Scte35SpliceInsert' {..} =
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
