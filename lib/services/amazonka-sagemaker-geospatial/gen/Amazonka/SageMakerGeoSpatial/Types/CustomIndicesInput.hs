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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.CustomIndicesInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.CustomIndicesInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.Operation

-- |
--
-- /See:/ 'newCustomIndicesInput' smart constructor.
data CustomIndicesInput = CustomIndicesInput'
  { operations :: Prelude.Maybe (Prelude.NonEmpty Operation)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomIndicesInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'customIndicesInput_operations' -
newCustomIndicesInput ::
  CustomIndicesInput
newCustomIndicesInput =
  CustomIndicesInput' {operations = Prelude.Nothing}

-- |
customIndicesInput_operations :: Lens.Lens' CustomIndicesInput (Prelude.Maybe (Prelude.NonEmpty Operation))
customIndicesInput_operations = Lens.lens (\CustomIndicesInput' {operations} -> operations) (\s@CustomIndicesInput' {} a -> s {operations = a} :: CustomIndicesInput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CustomIndicesInput where
  parseJSON =
    Data.withObject
      "CustomIndicesInput"
      ( \x ->
          CustomIndicesInput'
            Prelude.<$> (x Data..:? "Operations")
      )

instance Prelude.Hashable CustomIndicesInput where
  hashWithSalt _salt CustomIndicesInput' {..} =
    _salt `Prelude.hashWithSalt` operations

instance Prelude.NFData CustomIndicesInput where
  rnf CustomIndicesInput' {..} = Prelude.rnf operations

instance Data.ToJSON CustomIndicesInput where
  toJSON CustomIndicesInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Operations" Data..=) Prelude.<$> operations]
      )
