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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.PresenterOnlyConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.PresenterOnlyConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.PresenterPosition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the configuration for a presenter only video tile.
--
-- /See:/ 'newPresenterOnlyConfiguration' smart constructor.
data PresenterOnlyConfiguration = PresenterOnlyConfiguration'
  { -- | Defines the position of the presenter video tile. Default: @TopRight@.
    presenterPosition :: Prelude.Maybe PresenterPosition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PresenterOnlyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'presenterPosition', 'presenterOnlyConfiguration_presenterPosition' - Defines the position of the presenter video tile. Default: @TopRight@.
newPresenterOnlyConfiguration ::
  PresenterOnlyConfiguration
newPresenterOnlyConfiguration =
  PresenterOnlyConfiguration'
    { presenterPosition =
        Prelude.Nothing
    }

-- | Defines the position of the presenter video tile. Default: @TopRight@.
presenterOnlyConfiguration_presenterPosition :: Lens.Lens' PresenterOnlyConfiguration (Prelude.Maybe PresenterPosition)
presenterOnlyConfiguration_presenterPosition = Lens.lens (\PresenterOnlyConfiguration' {presenterPosition} -> presenterPosition) (\s@PresenterOnlyConfiguration' {} a -> s {presenterPosition = a} :: PresenterOnlyConfiguration)

instance Data.FromJSON PresenterOnlyConfiguration where
  parseJSON =
    Data.withObject
      "PresenterOnlyConfiguration"
      ( \x ->
          PresenterOnlyConfiguration'
            Prelude.<$> (x Data..:? "PresenterPosition")
      )

instance Prelude.Hashable PresenterOnlyConfiguration where
  hashWithSalt _salt PresenterOnlyConfiguration' {..} =
    _salt `Prelude.hashWithSalt` presenterPosition

instance Prelude.NFData PresenterOnlyConfiguration where
  rnf PresenterOnlyConfiguration' {..} =
    Prelude.rnf presenterPosition

instance Data.ToJSON PresenterOnlyConfiguration where
  toJSON PresenterOnlyConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PresenterPosition" Data..=)
              Prelude.<$> presenterPosition
          ]
      )
