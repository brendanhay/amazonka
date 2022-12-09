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
-- Module      : Amazonka.SecurityHub.Types.NumberFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.NumberFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A number filter for querying findings.
--
-- /See:/ 'newNumberFilter' smart constructor.
data NumberFilter = NumberFilter'
  { -- | The equal-to condition to be applied to a single field when querying for
    -- findings.
    eq :: Prelude.Maybe Prelude.Double,
    -- | The greater-than-equal condition to be applied to a single field when
    -- querying for findings.
    gte :: Prelude.Maybe Prelude.Double,
    -- | The less-than-equal condition to be applied to a single field when
    -- querying for findings.
    lte :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eq', 'numberFilter_eq' - The equal-to condition to be applied to a single field when querying for
-- findings.
--
-- 'gte', 'numberFilter_gte' - The greater-than-equal condition to be applied to a single field when
-- querying for findings.
--
-- 'lte', 'numberFilter_lte' - The less-than-equal condition to be applied to a single field when
-- querying for findings.
newNumberFilter ::
  NumberFilter
newNumberFilter =
  NumberFilter'
    { eq = Prelude.Nothing,
      gte = Prelude.Nothing,
      lte = Prelude.Nothing
    }

-- | The equal-to condition to be applied to a single field when querying for
-- findings.
numberFilter_eq :: Lens.Lens' NumberFilter (Prelude.Maybe Prelude.Double)
numberFilter_eq = Lens.lens (\NumberFilter' {eq} -> eq) (\s@NumberFilter' {} a -> s {eq = a} :: NumberFilter)

-- | The greater-than-equal condition to be applied to a single field when
-- querying for findings.
numberFilter_gte :: Lens.Lens' NumberFilter (Prelude.Maybe Prelude.Double)
numberFilter_gte = Lens.lens (\NumberFilter' {gte} -> gte) (\s@NumberFilter' {} a -> s {gte = a} :: NumberFilter)

-- | The less-than-equal condition to be applied to a single field when
-- querying for findings.
numberFilter_lte :: Lens.Lens' NumberFilter (Prelude.Maybe Prelude.Double)
numberFilter_lte = Lens.lens (\NumberFilter' {lte} -> lte) (\s@NumberFilter' {} a -> s {lte = a} :: NumberFilter)

instance Data.FromJSON NumberFilter where
  parseJSON =
    Data.withObject
      "NumberFilter"
      ( \x ->
          NumberFilter'
            Prelude.<$> (x Data..:? "Eq")
            Prelude.<*> (x Data..:? "Gte")
            Prelude.<*> (x Data..:? "Lte")
      )

instance Prelude.Hashable NumberFilter where
  hashWithSalt _salt NumberFilter' {..} =
    _salt `Prelude.hashWithSalt` eq
      `Prelude.hashWithSalt` gte
      `Prelude.hashWithSalt` lte

instance Prelude.NFData NumberFilter where
  rnf NumberFilter' {..} =
    Prelude.rnf eq
      `Prelude.seq` Prelude.rnf gte
      `Prelude.seq` Prelude.rnf lte

instance Data.ToJSON NumberFilter where
  toJSON NumberFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Eq" Data..=) Prelude.<$> eq,
            ("Gte" Data..=) Prelude.<$> gte,
            ("Lte" Data..=) Prelude.<$> lte
          ]
      )
