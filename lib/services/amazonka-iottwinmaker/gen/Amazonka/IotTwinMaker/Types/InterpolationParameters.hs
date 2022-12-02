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
-- Module      : Amazonka.IotTwinMaker.Types.InterpolationParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.InterpolationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.InterpolationType
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies how to interpolate data in a list.
--
-- /See:/ 'newInterpolationParameters' smart constructor.
data InterpolationParameters = InterpolationParameters'
  { -- | The interpolation type.
    interpolationType :: Prelude.Maybe InterpolationType,
    -- | The interpolation time interval in seconds.
    intervalInSeconds :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InterpolationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interpolationType', 'interpolationParameters_interpolationType' - The interpolation type.
--
-- 'intervalInSeconds', 'interpolationParameters_intervalInSeconds' - The interpolation time interval in seconds.
newInterpolationParameters ::
  InterpolationParameters
newInterpolationParameters =
  InterpolationParameters'
    { interpolationType =
        Prelude.Nothing,
      intervalInSeconds = Prelude.Nothing
    }

-- | The interpolation type.
interpolationParameters_interpolationType :: Lens.Lens' InterpolationParameters (Prelude.Maybe InterpolationType)
interpolationParameters_interpolationType = Lens.lens (\InterpolationParameters' {interpolationType} -> interpolationType) (\s@InterpolationParameters' {} a -> s {interpolationType = a} :: InterpolationParameters)

-- | The interpolation time interval in seconds.
interpolationParameters_intervalInSeconds :: Lens.Lens' InterpolationParameters (Prelude.Maybe Prelude.Integer)
interpolationParameters_intervalInSeconds = Lens.lens (\InterpolationParameters' {intervalInSeconds} -> intervalInSeconds) (\s@InterpolationParameters' {} a -> s {intervalInSeconds = a} :: InterpolationParameters)

instance Prelude.Hashable InterpolationParameters where
  hashWithSalt _salt InterpolationParameters' {..} =
    _salt `Prelude.hashWithSalt` interpolationType
      `Prelude.hashWithSalt` intervalInSeconds

instance Prelude.NFData InterpolationParameters where
  rnf InterpolationParameters' {..} =
    Prelude.rnf interpolationType
      `Prelude.seq` Prelude.rnf intervalInSeconds

instance Data.ToJSON InterpolationParameters where
  toJSON InterpolationParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("interpolationType" Data..=)
              Prelude.<$> interpolationType,
            ("intervalInSeconds" Data..=)
              Prelude.<$> intervalInSeconds
          ]
      )
