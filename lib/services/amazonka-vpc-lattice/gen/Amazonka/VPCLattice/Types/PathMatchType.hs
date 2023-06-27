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
-- Module      : Amazonka.VPCLattice.Types.PathMatchType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.PathMatchType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a path match type. Each rule can include only one of the
-- following types of paths.
--
-- /See:/ 'newPathMatchType' smart constructor.
data PathMatchType = PathMatchType'
  { -- | An exact match of the path.
    exact :: Prelude.Maybe Prelude.Text,
    -- | A prefix match of the path.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathMatchType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exact', 'pathMatchType_exact' - An exact match of the path.
--
-- 'prefix', 'pathMatchType_prefix' - A prefix match of the path.
newPathMatchType ::
  PathMatchType
newPathMatchType =
  PathMatchType'
    { exact = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | An exact match of the path.
pathMatchType_exact :: Lens.Lens' PathMatchType (Prelude.Maybe Prelude.Text)
pathMatchType_exact = Lens.lens (\PathMatchType' {exact} -> exact) (\s@PathMatchType' {} a -> s {exact = a} :: PathMatchType)

-- | A prefix match of the path.
pathMatchType_prefix :: Lens.Lens' PathMatchType (Prelude.Maybe Prelude.Text)
pathMatchType_prefix = Lens.lens (\PathMatchType' {prefix} -> prefix) (\s@PathMatchType' {} a -> s {prefix = a} :: PathMatchType)

instance Data.FromJSON PathMatchType where
  parseJSON =
    Data.withObject
      "PathMatchType"
      ( \x ->
          PathMatchType'
            Prelude.<$> (x Data..:? "exact")
            Prelude.<*> (x Data..:? "prefix")
      )

instance Prelude.Hashable PathMatchType where
  hashWithSalt _salt PathMatchType' {..} =
    _salt
      `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData PathMatchType where
  rnf PathMatchType' {..} =
    Prelude.rnf exact `Prelude.seq` Prelude.rnf prefix

instance Data.ToJSON PathMatchType where
  toJSON PathMatchType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exact" Data..=) Prelude.<$> exact,
            ("prefix" Data..=) Prelude.<$> prefix
          ]
      )
