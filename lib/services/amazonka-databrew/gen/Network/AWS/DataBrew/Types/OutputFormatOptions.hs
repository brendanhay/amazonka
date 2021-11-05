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
-- Module      : Network.AWS.DataBrew.Types.OutputFormatOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataBrew.Types.OutputFormatOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.DataBrew.Types.CsvOutputOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a set of options that define the structure of comma-separated
-- (CSV) job output.
--
-- /See:/ 'newOutputFormatOptions' smart constructor.
data OutputFormatOptions = OutputFormatOptions'
  { -- | Represents a set of options that define the structure of comma-separated
    -- value (CSV) job output.
    csv :: Prelude.Maybe CsvOutputOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputFormatOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'outputFormatOptions_csv' - Represents a set of options that define the structure of comma-separated
-- value (CSV) job output.
newOutputFormatOptions ::
  OutputFormatOptions
newOutputFormatOptions =
  OutputFormatOptions' {csv = Prelude.Nothing}

-- | Represents a set of options that define the structure of comma-separated
-- value (CSV) job output.
outputFormatOptions_csv :: Lens.Lens' OutputFormatOptions (Prelude.Maybe CsvOutputOptions)
outputFormatOptions_csv = Lens.lens (\OutputFormatOptions' {csv} -> csv) (\s@OutputFormatOptions' {} a -> s {csv = a} :: OutputFormatOptions)

instance Core.FromJSON OutputFormatOptions where
  parseJSON =
    Core.withObject
      "OutputFormatOptions"
      ( \x ->
          OutputFormatOptions' Prelude.<$> (x Core..:? "Csv")
      )

instance Prelude.Hashable OutputFormatOptions

instance Prelude.NFData OutputFormatOptions

instance Core.ToJSON OutputFormatOptions where
  toJSON OutputFormatOptions' {..} =
    Core.object
      (Prelude.catMaybes [("Csv" Core..=) Prelude.<$> csv])
