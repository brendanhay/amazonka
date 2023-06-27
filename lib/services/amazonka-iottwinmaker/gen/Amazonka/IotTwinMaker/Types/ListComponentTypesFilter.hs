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
-- Module      : Amazonka.IotTwinMaker.Types.ListComponentTypesFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ListComponentTypesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that filters items in a list of component types.
--
-- Only one object is accepted as a valid input.
--
-- /See:/ 'newListComponentTypesFilter' smart constructor.
data ListComponentTypesFilter = ListComponentTypesFilter'
  { -- | The component type that the component types in the list extend.
    extendsFrom :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that specifies whether the component types in the list
    -- are abstract.
    isAbstract :: Prelude.Maybe Prelude.Bool,
    -- | The namespace to which the component types in the list belong.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentTypesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extendsFrom', 'listComponentTypesFilter_extendsFrom' - The component type that the component types in the list extend.
--
-- 'isAbstract', 'listComponentTypesFilter_isAbstract' - A Boolean value that specifies whether the component types in the list
-- are abstract.
--
-- 'namespace', 'listComponentTypesFilter_namespace' - The namespace to which the component types in the list belong.
newListComponentTypesFilter ::
  ListComponentTypesFilter
newListComponentTypesFilter =
  ListComponentTypesFilter'
    { extendsFrom =
        Prelude.Nothing,
      isAbstract = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The component type that the component types in the list extend.
listComponentTypesFilter_extendsFrom :: Lens.Lens' ListComponentTypesFilter (Prelude.Maybe Prelude.Text)
listComponentTypesFilter_extendsFrom = Lens.lens (\ListComponentTypesFilter' {extendsFrom} -> extendsFrom) (\s@ListComponentTypesFilter' {} a -> s {extendsFrom = a} :: ListComponentTypesFilter)

-- | A Boolean value that specifies whether the component types in the list
-- are abstract.
listComponentTypesFilter_isAbstract :: Lens.Lens' ListComponentTypesFilter (Prelude.Maybe Prelude.Bool)
listComponentTypesFilter_isAbstract = Lens.lens (\ListComponentTypesFilter' {isAbstract} -> isAbstract) (\s@ListComponentTypesFilter' {} a -> s {isAbstract = a} :: ListComponentTypesFilter)

-- | The namespace to which the component types in the list belong.
listComponentTypesFilter_namespace :: Lens.Lens' ListComponentTypesFilter (Prelude.Maybe Prelude.Text)
listComponentTypesFilter_namespace = Lens.lens (\ListComponentTypesFilter' {namespace} -> namespace) (\s@ListComponentTypesFilter' {} a -> s {namespace = a} :: ListComponentTypesFilter)

instance Prelude.Hashable ListComponentTypesFilter where
  hashWithSalt _salt ListComponentTypesFilter' {..} =
    _salt
      `Prelude.hashWithSalt` extendsFrom
      `Prelude.hashWithSalt` isAbstract
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData ListComponentTypesFilter where
  rnf ListComponentTypesFilter' {..} =
    Prelude.rnf extendsFrom
      `Prelude.seq` Prelude.rnf isAbstract
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToJSON ListComponentTypesFilter where
  toJSON ListComponentTypesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("extendsFrom" Data..=) Prelude.<$> extendsFrom,
            ("isAbstract" Data..=) Prelude.<$> isAbstract,
            ("namespace" Data..=) Prelude.<$> namespace
          ]
      )
