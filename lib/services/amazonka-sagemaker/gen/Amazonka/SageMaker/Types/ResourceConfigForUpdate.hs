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
-- Module      : Amazonka.SageMaker.Types.ResourceConfigForUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ResourceConfigForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @ResourceConfig@ to update @KeepAlivePeriodInSeconds@. Other fields
-- in the @ResourceConfig@ cannot be updated.
--
-- /See:/ 'newResourceConfigForUpdate' smart constructor.
data ResourceConfigForUpdate = ResourceConfigForUpdate'
  { -- | The @KeepAlivePeriodInSeconds@ value specified in the @ResourceConfig@
    -- to update.
    keepAlivePeriodInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceConfigForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keepAlivePeriodInSeconds', 'resourceConfigForUpdate_keepAlivePeriodInSeconds' - The @KeepAlivePeriodInSeconds@ value specified in the @ResourceConfig@
-- to update.
newResourceConfigForUpdate ::
  -- | 'keepAlivePeriodInSeconds'
  Prelude.Natural ->
  ResourceConfigForUpdate
newResourceConfigForUpdate pKeepAlivePeriodInSeconds_ =
  ResourceConfigForUpdate'
    { keepAlivePeriodInSeconds =
        pKeepAlivePeriodInSeconds_
    }

-- | The @KeepAlivePeriodInSeconds@ value specified in the @ResourceConfig@
-- to update.
resourceConfigForUpdate_keepAlivePeriodInSeconds :: Lens.Lens' ResourceConfigForUpdate Prelude.Natural
resourceConfigForUpdate_keepAlivePeriodInSeconds = Lens.lens (\ResourceConfigForUpdate' {keepAlivePeriodInSeconds} -> keepAlivePeriodInSeconds) (\s@ResourceConfigForUpdate' {} a -> s {keepAlivePeriodInSeconds = a} :: ResourceConfigForUpdate)

instance Prelude.Hashable ResourceConfigForUpdate where
  hashWithSalt _salt ResourceConfigForUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` keepAlivePeriodInSeconds

instance Prelude.NFData ResourceConfigForUpdate where
  rnf ResourceConfigForUpdate' {..} =
    Prelude.rnf keepAlivePeriodInSeconds

instance Data.ToJSON ResourceConfigForUpdate where
  toJSON ResourceConfigForUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "KeepAlivePeriodInSeconds"
                  Data..= keepAlivePeriodInSeconds
              )
          ]
      )
