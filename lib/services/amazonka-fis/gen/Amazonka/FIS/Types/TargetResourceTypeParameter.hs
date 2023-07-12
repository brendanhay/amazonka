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
-- Module      : Amazonka.FIS.Types.TargetResourceTypeParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.TargetResourceTypeParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the parameters for a resource type. Use parameters to
-- determine which tasks are identified during target resolution.
--
-- /See:/ 'newTargetResourceTypeParameter' smart constructor.
data TargetResourceTypeParameter = TargetResourceTypeParameter'
  { -- | A description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the parameter is required.
    required :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetResourceTypeParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'targetResourceTypeParameter_description' - A description of the parameter.
--
-- 'required', 'targetResourceTypeParameter_required' - Indicates whether the parameter is required.
newTargetResourceTypeParameter ::
  TargetResourceTypeParameter
newTargetResourceTypeParameter =
  TargetResourceTypeParameter'
    { description =
        Prelude.Nothing,
      required = Prelude.Nothing
    }

-- | A description of the parameter.
targetResourceTypeParameter_description :: Lens.Lens' TargetResourceTypeParameter (Prelude.Maybe Prelude.Text)
targetResourceTypeParameter_description = Lens.lens (\TargetResourceTypeParameter' {description} -> description) (\s@TargetResourceTypeParameter' {} a -> s {description = a} :: TargetResourceTypeParameter)

-- | Indicates whether the parameter is required.
targetResourceTypeParameter_required :: Lens.Lens' TargetResourceTypeParameter (Prelude.Maybe Prelude.Bool)
targetResourceTypeParameter_required = Lens.lens (\TargetResourceTypeParameter' {required} -> required) (\s@TargetResourceTypeParameter' {} a -> s {required = a} :: TargetResourceTypeParameter)

instance Data.FromJSON TargetResourceTypeParameter where
  parseJSON =
    Data.withObject
      "TargetResourceTypeParameter"
      ( \x ->
          TargetResourceTypeParameter'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "required")
      )

instance Prelude.Hashable TargetResourceTypeParameter where
  hashWithSalt _salt TargetResourceTypeParameter' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` required

instance Prelude.NFData TargetResourceTypeParameter where
  rnf TargetResourceTypeParameter' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf required
