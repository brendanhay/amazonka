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
-- Module      : Amazonka.MacieV2.Types.Scoping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Scoping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.JobScopingBlock
import qualified Amazonka.Prelude as Prelude

-- | Specifies one or more property- and tag-based conditions that define
-- criteria for including or excluding S3 objects from a classification
-- job. Exclude conditions take precedence over include conditions.
--
-- /See:/ 'newScoping' smart constructor.
data Scoping = Scoping'
  { -- | The property- and tag-based conditions that determine which objects to
    -- exclude from the analysis.
    excludes :: Prelude.Maybe JobScopingBlock,
    -- | The property- and tag-based conditions that determine which objects to
    -- include in the analysis.
    includes :: Prelude.Maybe JobScopingBlock
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scoping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludes', 'scoping_excludes' - The property- and tag-based conditions that determine which objects to
-- exclude from the analysis.
--
-- 'includes', 'scoping_includes' - The property- and tag-based conditions that determine which objects to
-- include in the analysis.
newScoping ::
  Scoping
newScoping =
  Scoping'
    { excludes = Prelude.Nothing,
      includes = Prelude.Nothing
    }

-- | The property- and tag-based conditions that determine which objects to
-- exclude from the analysis.
scoping_excludes :: Lens.Lens' Scoping (Prelude.Maybe JobScopingBlock)
scoping_excludes = Lens.lens (\Scoping' {excludes} -> excludes) (\s@Scoping' {} a -> s {excludes = a} :: Scoping)

-- | The property- and tag-based conditions that determine which objects to
-- include in the analysis.
scoping_includes :: Lens.Lens' Scoping (Prelude.Maybe JobScopingBlock)
scoping_includes = Lens.lens (\Scoping' {includes} -> includes) (\s@Scoping' {} a -> s {includes = a} :: Scoping)

instance Data.FromJSON Scoping where
  parseJSON =
    Data.withObject
      "Scoping"
      ( \x ->
          Scoping'
            Prelude.<$> (x Data..:? "excludes")
            Prelude.<*> (x Data..:? "includes")
      )

instance Prelude.Hashable Scoping where
  hashWithSalt _salt Scoping' {..} =
    _salt
      `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` includes

instance Prelude.NFData Scoping where
  rnf Scoping' {..} =
    Prelude.rnf excludes `Prelude.seq`
      Prelude.rnf includes

instance Data.ToJSON Scoping where
  toJSON Scoping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("excludes" Data..=) Prelude.<$> excludes,
            ("includes" Data..=) Prelude.<$> includes
          ]
      )
