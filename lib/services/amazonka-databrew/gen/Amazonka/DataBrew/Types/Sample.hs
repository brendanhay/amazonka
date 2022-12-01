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
-- Module      : Amazonka.DataBrew.Types.Sample
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Sample where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types.SampleType
import qualified Amazonka.Prelude as Prelude

-- | Represents the sample size and sampling type for DataBrew to use for
-- interactive data analysis.
--
-- /See:/ 'newSample' smart constructor.
data Sample = Sample'
  { -- | The number of rows in the sample.
    size :: Prelude.Maybe Prelude.Natural,
    -- | The way in which DataBrew obtains rows from a dataset.
    type' :: SampleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Sample' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'sample_size' - The number of rows in the sample.
--
-- 'type'', 'sample_type' - The way in which DataBrew obtains rows from a dataset.
newSample ::
  -- | 'type''
  SampleType ->
  Sample
newSample pType_ =
  Sample' {size = Prelude.Nothing, type' = pType_}

-- | The number of rows in the sample.
sample_size :: Lens.Lens' Sample (Prelude.Maybe Prelude.Natural)
sample_size = Lens.lens (\Sample' {size} -> size) (\s@Sample' {} a -> s {size = a} :: Sample)

-- | The way in which DataBrew obtains rows from a dataset.
sample_type :: Lens.Lens' Sample SampleType
sample_type = Lens.lens (\Sample' {type'} -> type') (\s@Sample' {} a -> s {type' = a} :: Sample)

instance Core.FromJSON Sample where
  parseJSON =
    Core.withObject
      "Sample"
      ( \x ->
          Sample'
            Prelude.<$> (x Core..:? "Size") Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable Sample where
  hashWithSalt _salt Sample' {..} =
    _salt `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Sample where
  rnf Sample' {..} =
    Prelude.rnf size `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON Sample where
  toJSON Sample' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Size" Core..=) Prelude.<$> size,
            Prelude.Just ("Type" Core..= type')
          ]
      )
