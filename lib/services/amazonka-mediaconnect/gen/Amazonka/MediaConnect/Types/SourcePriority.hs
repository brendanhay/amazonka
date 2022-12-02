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
-- Module      : Amazonka.MediaConnect.Types.SourcePriority
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.SourcePriority where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON SourcePriority where
  parseJSON =
    Data.withObject
      "SourcePriority"
      ( \x ->
          SourcePriority'
            Prelude.<$> (x Data..:? "primarySource")
      )

instance Prelude.Hashable SourcePriority where
  hashWithSalt _salt SourcePriority' {..} =
    _salt `Prelude.hashWithSalt` primarySource

instance Prelude.NFData SourcePriority where
  rnf SourcePriority' {..} = Prelude.rnf primarySource

instance Data.ToJSON SourcePriority where
  toJSON SourcePriority' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("primarySource" Data..=)
              Prelude.<$> primarySource
          ]
      )
