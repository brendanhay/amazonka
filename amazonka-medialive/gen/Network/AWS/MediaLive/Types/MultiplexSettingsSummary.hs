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
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettingsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettingsSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains summary configuration for a Multiplex event.
--
-- /See:/ 'newMultiplexSettingsSummary' smart constructor.
data MultiplexSettingsSummary = MultiplexSettingsSummary'
  { -- | Transport stream bit rate.
    transportStreamBitrate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MultiplexSettingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transportStreamBitrate', 'multiplexSettingsSummary_transportStreamBitrate' - Transport stream bit rate.
newMultiplexSettingsSummary ::
  MultiplexSettingsSummary
newMultiplexSettingsSummary =
  MultiplexSettingsSummary'
    { transportStreamBitrate =
        Prelude.Nothing
    }

-- | Transport stream bit rate.
multiplexSettingsSummary_transportStreamBitrate :: Lens.Lens' MultiplexSettingsSummary (Prelude.Maybe Prelude.Natural)
multiplexSettingsSummary_transportStreamBitrate = Lens.lens (\MultiplexSettingsSummary' {transportStreamBitrate} -> transportStreamBitrate) (\s@MultiplexSettingsSummary' {} a -> s {transportStreamBitrate = a} :: MultiplexSettingsSummary)

instance Prelude.FromJSON MultiplexSettingsSummary where
  parseJSON =
    Prelude.withObject
      "MultiplexSettingsSummary"
      ( \x ->
          MultiplexSettingsSummary'
            Prelude.<$> (x Prelude..:? "transportStreamBitrate")
      )

instance Prelude.Hashable MultiplexSettingsSummary

instance Prelude.NFData MultiplexSettingsSummary
