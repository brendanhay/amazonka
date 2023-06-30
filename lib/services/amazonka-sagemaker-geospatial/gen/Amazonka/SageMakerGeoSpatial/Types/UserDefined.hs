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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.UserDefined
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.UserDefined where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.Unit

-- |
--
-- /See:/ 'newUserDefined' smart constructor.
data UserDefined = UserDefined'
  { unit :: Unit,
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDefined' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'userDefined_unit' -
--
-- 'value', 'userDefined_value' -
newUserDefined ::
  -- | 'unit'
  Unit ->
  -- | 'value'
  Prelude.Double ->
  UserDefined
newUserDefined pUnit_ pValue_ =
  UserDefined' {unit = pUnit_, value = pValue_}

userDefined_unit :: Lens.Lens' UserDefined Unit
userDefined_unit = Lens.lens (\UserDefined' {unit} -> unit) (\s@UserDefined' {} a -> s {unit = a} :: UserDefined)

userDefined_value :: Lens.Lens' UserDefined Prelude.Double
userDefined_value = Lens.lens (\UserDefined' {value} -> value) (\s@UserDefined' {} a -> s {value = a} :: UserDefined)

instance Data.FromJSON UserDefined where
  parseJSON =
    Data.withObject
      "UserDefined"
      ( \x ->
          UserDefined'
            Prelude.<$> (x Data..: "Unit")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable UserDefined where
  hashWithSalt _salt UserDefined' {..} =
    _salt
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData UserDefined where
  rnf UserDefined' {..} =
    Prelude.rnf unit `Prelude.seq` Prelude.rnf value

instance Data.ToJSON UserDefined where
  toJSON UserDefined' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Unit" Data..= unit),
            Prelude.Just ("Value" Data..= value)
          ]
      )
