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
-- Module      : Amazonka.AppFlow.Types.Range
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The range of values that the property supports.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | Minimum value supported by the field.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | Maximum value supported by the field.
    maximum :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimum', 'range_minimum' - Minimum value supported by the field.
--
-- 'maximum', 'range_maximum' - Maximum value supported by the field.
newRange ::
  Range
newRange =
  Range'
    { minimum = Prelude.Nothing,
      maximum = Prelude.Nothing
    }

-- | Minimum value supported by the field.
range_minimum :: Lens.Lens' Range (Prelude.Maybe Prelude.Double)
range_minimum = Lens.lens (\Range' {minimum} -> minimum) (\s@Range' {} a -> s {minimum = a} :: Range)

-- | Maximum value supported by the field.
range_maximum :: Lens.Lens' Range (Prelude.Maybe Prelude.Double)
range_maximum = Lens.lens (\Range' {maximum} -> maximum) (\s@Range' {} a -> s {maximum = a} :: Range)

instance Core.FromJSON Range where
  parseJSON =
    Core.withObject
      "Range"
      ( \x ->
          Range'
            Prelude.<$> (x Core..:? "minimum")
            Prelude.<*> (x Core..:? "maximum")
      )

instance Prelude.Hashable Range where
  hashWithSalt _salt Range' {..} =
    _salt `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` maximum

instance Prelude.NFData Range where
  rnf Range' {..} =
    Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf maximum
