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
-- Module      : Network.AWS.Pinpoint.Types.RecencyDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RecencyDimension where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Duration
import Network.AWS.Pinpoint.Types.RecencyType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies criteria for including or excluding endpoints from a segment
-- based on how recently an endpoint was active.
--
-- /See:/ 'newRecencyDimension' smart constructor.
data RecencyDimension = RecencyDimension'
  { -- | The duration to use when determining whether an endpoint is active or
    -- inactive.
    duration :: Duration,
    -- | The type of recency dimension to use for the segment. Valid values are:
    -- ACTIVE, endpoints that were active within the specified duration are
    -- included in the segment; and, INACTIVE, endpoints that weren\'t active
    -- within the specified duration are included in the segment.
    recencyType :: RecencyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RecencyDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'recencyDimension_duration' - The duration to use when determining whether an endpoint is active or
-- inactive.
--
-- 'recencyType', 'recencyDimension_recencyType' - The type of recency dimension to use for the segment. Valid values are:
-- ACTIVE, endpoints that were active within the specified duration are
-- included in the segment; and, INACTIVE, endpoints that weren\'t active
-- within the specified duration are included in the segment.
newRecencyDimension ::
  -- | 'duration'
  Duration ->
  -- | 'recencyType'
  RecencyType ->
  RecencyDimension
newRecencyDimension pDuration_ pRecencyType_ =
  RecencyDimension'
    { duration = pDuration_,
      recencyType = pRecencyType_
    }

-- | The duration to use when determining whether an endpoint is active or
-- inactive.
recencyDimension_duration :: Lens.Lens' RecencyDimension Duration
recencyDimension_duration = Lens.lens (\RecencyDimension' {duration} -> duration) (\s@RecencyDimension' {} a -> s {duration = a} :: RecencyDimension)

-- | The type of recency dimension to use for the segment. Valid values are:
-- ACTIVE, endpoints that were active within the specified duration are
-- included in the segment; and, INACTIVE, endpoints that weren\'t active
-- within the specified duration are included in the segment.
recencyDimension_recencyType :: Lens.Lens' RecencyDimension RecencyType
recencyDimension_recencyType = Lens.lens (\RecencyDimension' {recencyType} -> recencyType) (\s@RecencyDimension' {} a -> s {recencyType = a} :: RecencyDimension)

instance Prelude.FromJSON RecencyDimension where
  parseJSON =
    Prelude.withObject
      "RecencyDimension"
      ( \x ->
          RecencyDimension'
            Prelude.<$> (x Prelude..: "Duration")
            Prelude.<*> (x Prelude..: "RecencyType")
      )

instance Prelude.Hashable RecencyDimension

instance Prelude.NFData RecencyDimension

instance Prelude.ToJSON RecencyDimension where
  toJSON RecencyDimension' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Duration" Prelude..= duration),
            Prelude.Just ("RecencyType" Prelude..= recencyType)
          ]
      )
