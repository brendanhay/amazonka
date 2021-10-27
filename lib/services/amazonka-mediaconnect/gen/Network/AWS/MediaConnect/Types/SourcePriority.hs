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
-- Module      : Network.AWS.MediaConnect.Types.SourcePriority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConnect.Types.SourcePriority where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The priority you want to assign to a source. You can have a primary
-- stream and a backup stream or two equally prioritized streams.
--
-- /See:/ 'newSourcePriority' smart constructor.
data SourcePriority = SourcePriority'
  { -- | The name of the source you choose as the primary source for this flow.
    primarySource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourcePriority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primarySource', 'sourcePriority_primarySource' - The name of the source you choose as the primary source for this flow.
newSourcePriority ::
  SourcePriority
newSourcePriority =
  SourcePriority' {primarySource = Prelude.Nothing}

-- | The name of the source you choose as the primary source for this flow.
sourcePriority_primarySource :: Lens.Lens' SourcePriority (Prelude.Maybe Prelude.Text)
sourcePriority_primarySource = Lens.lens (\SourcePriority' {primarySource} -> primarySource) (\s@SourcePriority' {} a -> s {primarySource = a} :: SourcePriority)

instance Core.FromJSON SourcePriority where
  parseJSON =
    Core.withObject
      "SourcePriority"
      ( \x ->
          SourcePriority'
            Prelude.<$> (x Core..:? "primarySource")
      )

instance Prelude.Hashable SourcePriority

instance Prelude.NFData SourcePriority

instance Core.ToJSON SourcePriority where
  toJSON SourcePriority' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("primarySource" Core..=)
              Prelude.<$> primarySource
          ]
      )
