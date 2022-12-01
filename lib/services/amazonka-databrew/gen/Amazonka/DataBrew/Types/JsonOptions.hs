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
-- Module      : Amazonka.DataBrew.Types.JsonOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.JsonOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the JSON-specific options that define how input is to be
-- interpreted by Glue DataBrew.
--
-- /See:/ 'newJsonOptions' smart constructor.
data JsonOptions = JsonOptions'
  { -- | A value that specifies whether JSON input contains embedded new line
    -- characters.
    multiLine :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JsonOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiLine', 'jsonOptions_multiLine' - A value that specifies whether JSON input contains embedded new line
-- characters.
newJsonOptions ::
  JsonOptions
newJsonOptions =
  JsonOptions' {multiLine = Prelude.Nothing}

-- | A value that specifies whether JSON input contains embedded new line
-- characters.
jsonOptions_multiLine :: Lens.Lens' JsonOptions (Prelude.Maybe Prelude.Bool)
jsonOptions_multiLine = Lens.lens (\JsonOptions' {multiLine} -> multiLine) (\s@JsonOptions' {} a -> s {multiLine = a} :: JsonOptions)

instance Core.FromJSON JsonOptions where
  parseJSON =
    Core.withObject
      "JsonOptions"
      ( \x ->
          JsonOptions' Prelude.<$> (x Core..:? "MultiLine")
      )

instance Prelude.Hashable JsonOptions where
  hashWithSalt _salt JsonOptions' {..} =
    _salt `Prelude.hashWithSalt` multiLine

instance Prelude.NFData JsonOptions where
  rnf JsonOptions' {..} = Prelude.rnf multiLine

instance Core.ToJSON JsonOptions where
  toJSON JsonOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("MultiLine" Core..=) Prelude.<$> multiLine]
      )
