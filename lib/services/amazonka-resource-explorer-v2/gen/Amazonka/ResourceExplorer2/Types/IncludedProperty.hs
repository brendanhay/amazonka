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
-- Module      : Amazonka.ResourceExplorer2.Types.IncludedProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.IncludedProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an additional property that describes a resource, that
-- you can optionally include in the view. This lets you view that property
-- in search results, and filter your search results based on the value of
-- the property.
--
-- /See:/ 'newIncludedProperty' smart constructor.
data IncludedProperty = IncludedProperty'
  { -- | The name of the property that is included in this view.
    --
    -- You can specify the following property names for this field:
    --
    -- -   @Tags@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncludedProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'includedProperty_name' - The name of the property that is included in this view.
--
-- You can specify the following property names for this field:
--
-- -   @Tags@
newIncludedProperty ::
  -- | 'name'
  Prelude.Text ->
  IncludedProperty
newIncludedProperty pName_ =
  IncludedProperty' {name = pName_}

-- | The name of the property that is included in this view.
--
-- You can specify the following property names for this field:
--
-- -   @Tags@
includedProperty_name :: Lens.Lens' IncludedProperty Prelude.Text
includedProperty_name = Lens.lens (\IncludedProperty' {name} -> name) (\s@IncludedProperty' {} a -> s {name = a} :: IncludedProperty)

instance Data.FromJSON IncludedProperty where
  parseJSON =
    Data.withObject
      "IncludedProperty"
      ( \x ->
          IncludedProperty' Prelude.<$> (x Data..: "Name")
      )

instance Prelude.Hashable IncludedProperty where
  hashWithSalt _salt IncludedProperty' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData IncludedProperty where
  rnf IncludedProperty' {..} = Prelude.rnf name

instance Data.ToJSON IncludedProperty where
  toJSON IncludedProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )
