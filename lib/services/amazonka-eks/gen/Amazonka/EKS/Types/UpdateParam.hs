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
-- Module      : Amazonka.EKS.Types.UpdateParam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.UpdateParam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.UpdateParamType
import qualified Amazonka.Prelude as Prelude

-- | An object representing the details of an update request.
--
-- /See:/ 'newUpdateParam' smart constructor.
data UpdateParam = UpdateParam'
  { -- | The keys associated with an update request.
    type' :: Prelude.Maybe UpdateParamType,
    -- | The value of the keys submitted as part of an update request.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateParam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'updateParam_type' - The keys associated with an update request.
--
-- 'value', 'updateParam_value' - The value of the keys submitted as part of an update request.
newUpdateParam ::
  UpdateParam
newUpdateParam =
  UpdateParam'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The keys associated with an update request.
updateParam_type :: Lens.Lens' UpdateParam (Prelude.Maybe UpdateParamType)
updateParam_type = Lens.lens (\UpdateParam' {type'} -> type') (\s@UpdateParam' {} a -> s {type' = a} :: UpdateParam)

-- | The value of the keys submitted as part of an update request.
updateParam_value :: Lens.Lens' UpdateParam (Prelude.Maybe Prelude.Text)
updateParam_value = Lens.lens (\UpdateParam' {value} -> value) (\s@UpdateParam' {} a -> s {value = a} :: UpdateParam)

instance Data.FromJSON UpdateParam where
  parseJSON =
    Data.withObject
      "UpdateParam"
      ( \x ->
          UpdateParam'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable UpdateParam where
  hashWithSalt _salt UpdateParam' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData UpdateParam where
  rnf UpdateParam' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value
