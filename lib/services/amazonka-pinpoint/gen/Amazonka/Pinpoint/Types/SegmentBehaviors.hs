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
-- Module      : Amazonka.Pinpoint.Types.SegmentBehaviors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentBehaviors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.RecencyDimension
import qualified Amazonka.Prelude as Prelude

-- | Specifies dimension settings for including or excluding endpoints from a
-- segment based on how recently an endpoint was active.
--
-- /See:/ 'newSegmentBehaviors' smart constructor.
data SegmentBehaviors = SegmentBehaviors'
  { -- | The dimension settings that are based on how recently an endpoint was
    -- active.
    recency :: Prelude.Maybe RecencyDimension
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentBehaviors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recency', 'segmentBehaviors_recency' - The dimension settings that are based on how recently an endpoint was
-- active.
newSegmentBehaviors ::
  SegmentBehaviors
newSegmentBehaviors =
  SegmentBehaviors' {recency = Prelude.Nothing}

-- | The dimension settings that are based on how recently an endpoint was
-- active.
segmentBehaviors_recency :: Lens.Lens' SegmentBehaviors (Prelude.Maybe RecencyDimension)
segmentBehaviors_recency = Lens.lens (\SegmentBehaviors' {recency} -> recency) (\s@SegmentBehaviors' {} a -> s {recency = a} :: SegmentBehaviors)

instance Data.FromJSON SegmentBehaviors where
  parseJSON =
    Data.withObject
      "SegmentBehaviors"
      ( \x ->
          SegmentBehaviors' Prelude.<$> (x Data..:? "Recency")
      )

instance Prelude.Hashable SegmentBehaviors where
  hashWithSalt _salt SegmentBehaviors' {..} =
    _salt `Prelude.hashWithSalt` recency

instance Prelude.NFData SegmentBehaviors where
  rnf SegmentBehaviors' {..} = Prelude.rnf recency

instance Data.ToJSON SegmentBehaviors where
  toJSON SegmentBehaviors' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Recency" Data..=) Prelude.<$> recency]
      )
